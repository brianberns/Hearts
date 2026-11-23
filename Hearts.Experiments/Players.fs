namespace Hearts.Experiments

open Hearts
open Hearts.Heuristic
open Hearts.Model

/// Players for experiments.
module Players =

    /// Loads an advantage model checkpoint for inference on the given device.
    let loadModelOn device (path : string) =
        let model = new AdvantageModel(Encoding.encodedLength * 2, 4, 0.0, device)
        model.load(path) |> ignore
        model.eval()
        model

    /// Loads an advantage model checkpoint for inference on the CPU.
    let loadModel path = loadModelOn TorchSharp.torch.CPU path

    /// The given player, guarded against Shoots that lose the game: when a Shoot would lose under the
    /// rules, and the player has taken every point so far, it plays Claude's card to hand an opponent a
    /// point (see Claude.shootLoses and Claude.breakMoon).
    let guarded (player : Player) =
        let act (infoSet : InformationSet) =
            match infoSet.LegalActionType with
                | ActionType.Play when Claude.shootLoses infoSet ->
                    Claude.breakMoon infoSet |> Option.defaultWith (fun () -> player.Act infoSet)
                | _ -> player.Act infoSet
        { Act = act }
