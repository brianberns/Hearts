namespace Hearts.Experiments

open System
open System.Diagnostics
open System.IO

open TorchSharp

/// Fits closed forms with a few parameters to an exact W from WExact. Each form is fitted to the exact W at
/// the starting scores of the data's deals (so states count as often as games reach them), by the mean KL
/// divergence from the exact W, then scored like wfit and wexact: its cross-entropy against the actual
/// winners of the validation games. Every form is symmetric in the seats by construction. D, how far the
/// leader is from 100, sets how much play is left.
module WCurve =

    /// A closed form: its name, its parameters' starting values, and W from the scores (N x 4) and the
    /// parameters.
    type private Form =
        {
            Name : string
            Initial : float32[]
            Formula : string
            Probs : torch.Tensor -> torch.Tensor -> torch.Tensor
        }

    /// The positive parameter for the given raw one.
    let private pos (raw : torch.Tensor) = torch.nn.functional.softplus(raw)

    /// The raw parameter for the given positive one.
    let private unpos (x : float32) = float32 (log (exp (float x) - 1.0))

    /// A constant.
    let private k (x : float32) = torch.tensor(x)

    /// How far the leader is from 100, at least 0.
    let private distance (s : torch.Tensor) =
        let struct (high, _) = s.max(1L, true)
        (k 100.0f - high).clamp_min(0.0)

    /// The lowest score.
    let private low (s : torch.Tensor) =
        let struct (low, _) = s.min(1L, true)
        low

    /// A softmax of each seat's own logit.
    let private softmaxForm name formula initial (logits : torch.Tensor -> torch.Tensor -> torch.Tensor) =
        {
            Name = name
            Initial = initial |> Array.map unpos
            Formula = formula
            Probs = fun s p -> torch.nn.functional.softmax(logits s (pos p), 1L)
        }

    /// Points of a standard normal, and their weights, for integrating over it.
    let private gridZ, gridW =
        let z = [| for k in -30 .. 30 -> float32 k * 0.2f |]
        let w = z |> Array.map (fun z -> exp (-0.5f * z * z) * 0.2f / sqrt (2.0f * MathF.PI))
        torch.tensor(z).reshape(1L, 1L, 1L, -1L), torch.tensor(w)

    let private eye = torch.eye(4L).reshape(1L, 4L, 4L, 1L).to_type(torch.float32)

    /// Each seat's final score is its score plus a normal with s.d. sigma (N x 1); answers each seat's
    /// chance of being lowest.
    let private thurstone (s : torch.Tensor) (sigma : torch.Tensor) =
            // diff[n, i, j] = (s_j - s_i) / sigma; seat i, at s_i + sigma z, is below seat j with chance
            // Phi(diff + z) (z's sign flipped, by symmetry), so W_i is the mean over z of the product over j
        let diff = (s.unsqueeze(1L) - s.unsqueeze(2L)) / sigma.unsqueeze(2L)
        let cdf = k 0.5f * (k 1.0f + torch.erf((diff.unsqueeze(3L) + gridZ) / k (sqrt 2.0f)))
        let logBelow = (cdf.clamp_min(1e-30) |> torch.log) * (k 1.0f - eye)
        let probs = (logBelow.sum(2L).exp() * gridW).sum(2L)
        probs / probs.sum(1L, true)

    let private forms =
        [|
            softmaxForm "power" "logit_i = -a s_i / (b + D)^c" [| 1.0f; 10.0f; 0.5f |]
                (fun s p -> -p[0L] * s / (p[1L] + distance s).pow(p[2L]))
            softmaxForm "power-quadratic" "x_i = (s_i - min s) / (b + D)^c; logit_i = -a x_i - d x_i^2" [| 1.0f; 10.0f; 0.5f; 0.1f |]
                (fun s p ->
                    let x = (s - low s) / (p[1L] + distance s).pow(p[2L])
                    -p[0L] * x - p[3L] * x * x)
            {
                Name = "thurstone"
                Initial = [| 1.5f; 10.0f; 0.5f |] |> Array.map unpos
                Formula = "final score_i ~ Normal(s_i, sigma), sigma = a (b + D)^c; W_i = P(final score_i lowest)"
                Probs = fun s p -> let p = pos p in thurstone s (p[0L] * (p[1L] + distance s).pow(p[2L]))
            }
            {
                Name = "thurstone-2"
                Initial = [| 1.5f; 10.0f; 0.5f; 0.1f |] |> Array.map unpos
                Formula = "as thurstone, with sigma = a (b + D)^c + d D"
                Probs = fun s p -> let p = pos p in thurstone s (p[0L] * (p[1L] + distance s).pow(p[2L]) + p[3L] * distance s)
            }
        |]

    /// The exact W at the given scores (each under 100), by seat.
    let private lookup (table : float32[]) (s : int[]) =
        let order = [| 0; 1; 2; 3 |]
        Array.sortInPlaceWith (fun i j -> compare s[i] s[j]) order
        let i = 4 * WExact.index s[order[0]] s[order[1]] s[order[2]] s[order[3]]
        let w = Array.zeroCreate 4
        for k in 0 .. 3 do w[order[k]] <- table[i + k]
        w

    /// Scores as a tensor.
    let private toTensor (rows : WExact.Row[]) =
        torch.tensor(rows |> Array.collect (fun r -> r.Scores |> Array.map float32)).reshape(int64 rows.Length, 4L)

    /// W from the form, in chunks.
    let private probs form (p : torch.Tensor) (s : torch.Tensor) =
        use _ = torch.no_grad()
        let n = s.shape[0]
        torch.cat(
            [| for start in 0L .. 20000L .. n - 1L ->
                form.Probs (s.narrow(0L, start, min 20000L (n - start))) p |],
            0L)

    /// Mean KL divergence from the target to W.
    let private kl (target : torch.Tensor) (w : torch.Tensor) =
        (target * (target.clamp_min(1e-12).log() - w.clamp_min(1e-12).log())).sum(1L).mean()

    let run (dataPath : string) (tablePath : string) numSteps (names : string list) =
        Process.GetCurrentProcess().PriorityClass <- ProcessPriorityClass.BelowNormal
        torch.set_num_threads(8)
        let table =
            let bytes = File.ReadAllBytes(tablePath)
            let values = Array.zeroCreate<float32> (bytes.Length / 4)
            Buffer.BlockCopy(bytes, 0, values, 0, bytes.Length)
            values
        assert(table.Length = 4 * WExact.numStates)
        let rows = WExact.readRows dataPath
        let validRows = rows |> Array.filter (fun r -> r.Game % 10 = 0)
        let exactRows = rows |> Array.filter (fun r -> Array.max r.Scores < 100)
        let fitRows = Random(0).GetItems(exactRows, 50000)
        let target (rows : WExact.Row[]) =
            torch.tensor(rows |> Array.collect (fun r -> lookup table r.Scores)).reshape(int64 rows.Length, 4L)
        let fitS, fitT = toTensor fitRows, target fitRows
        let allS, allT = toTensor exactRows, target exactRows
        let validS = toTensor validRows
        use winners = torch.tensor(validRows |> Array.map (fun r -> int64 r.Winner))
        printfn $"{rows.Length} deals; fitting to {fitRows.Length} of the {exactRows.Length} under 100, scoring on {validRows.Length} validation deals"
        let crossEntropy (w : torch.Tensor) =
            (-(w.clamp_min(1e-12).log().gather(1L, winners.unsqueeze(1L)))).mean().item<float32>()
        printfn "exact W (wexact): validation loss 1.03765 on the i005 data"
        let fitted = Collections.Generic.Dictionary<string, float32[]>()
        for form in forms |> Array.filter (fun f -> names.IsEmpty || f.Name = "power" || List.contains f.Name names) do
            let stopwatch = Stopwatch.StartNew()
            let param = torch.nn.Parameter(torch.tensor(form.Initial))
            use optimizer = torch.optim.Adam([| param |], 0.05)
            for step in 1 .. numSteps do
                optimizer.zero_grad()
                use loss = kl fitT (form.Probs fitS param)
                loss.backward()
                optimizer.step() |> ignore
                if step % 500 = 0 then printfn $"   {form.Name} step {step}: KL %.6f{loss.item<float32>()}"
            let values = (pos param).detach().data<float32>().ToArray()
            fitted[form.Name] <- values
            let klAll = (kl allT (probs form param allS)).item<float32>()
            let loss = crossEntropy (probs form param validS)
            let ps = String.Join(", ", values |> Array.mapi (fun i v -> $"{char (int 'a' + i)} = %.4f{v}"))
            printfn $"{form.Name}: {form.Formula}"
            printfn $"   {ps}; KL from exact %.6f{klAll}; validation loss %.5f{loss} ({stopwatch.Elapsed})"
            let show scores =
                use s = torch.tensor(scores |> Array.map float32).reshape(1L, 4L)
                let w = (probs form param s).data<float32>().ToArray()
                let e = lookup table scores
                let f (xs : float32[]) = String.Join(" ", xs |> Array.map (fun x -> x.ToString("0.000")))
                printfn $"   %-14s{String.Join(',', scores)}: {f w}   (exact {f e})"
            show [| 13; 0; 0; 0 |]
            show [| 63; 50; 50; 50 |]
            show [| 93; 70; 70; 70 |]
            show [| 60; 90; 75; 80 |]
            show [| 95; 96; 97; 98 |]

            // the power form, one deal ahead: the next deal's outcomes exactly (GameScore's), then the form
        let a, b, c = let v = fitted["power"] in float v[0], float v[1], float v[2]
        let power (t : int[]) =
            let d = max 0 (100 - Array.max t)
            let logits = t |> Array.map (fun x -> -a * float x / (b + float d) ** c)
            let m = Array.max logits
            let e = logits |> Array.map (fun l -> exp (l - m))
            let total = Array.sum e
            e |> Array.map (fun x -> x / total)
        let outcomes = WExact.gameScoreOutcomes ()
        let lookahead (s : int[]) =
            let acc = Array.zeroCreate 4
            for o in outcomes do
                let t = Array.map2 (+) s o.Points
                let low = Array.min t
                if Array.max t >= 100 && (t |> Array.filter ((=) low) |> Array.length) = 1 then
                    let i = Array.findIndex ((=) low) t
                    acc[i] <- acc[i] + o.Prob
                else
                    let w = power t   // play continues (including a tie for low at the end)
                    for k in 0 .. 3 do acc[k] <- acc[k] + o.Prob * w[k]
            acc
        let stopwatch = Stopwatch.StartNew()
        let losses = validRows |> Array.Parallel.map (fun r -> -log ((lookahead r.Scores)[r.Winner]))
        printfn "power, one deal ahead: W = the next deal's outcomes (GameScore's) exactly, then the power form"
        printfn $"   validation loss %.5f{Array.average losses} ({stopwatch.Elapsed})"
        let f (xs : float[]) = String.Join(" ", xs |> Array.map (fun x -> x.ToString("0.000")))
        for scores in [| [| 13; 0; 0; 0 |]; [| 63; 50; 50; 50 |]; [| 93; 70; 70; 70 |]; [| 60; 90; 75; 80 |]; [| 95; 96; 97; 98 |] |] do
            printfn $"   %-14s{String.Join(',', scores)}: {f (lookahead scores)}   (exact {f (lookup table scores |> Array.map float)})"
