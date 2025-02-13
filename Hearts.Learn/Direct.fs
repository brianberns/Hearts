namespace Hearts.Learn

open System.IO

open MathNet.Numerics.LinearAlgebra

open Hearts
open Hearts.Model

module Direct =

    /// Generates training data using a standard player.
    let private generateTrainingData numDeals =

        let rec loop deal =
            seq {
                let play, sampleOpt =
                    let hand =
                        let seat = OpenDeal.currentPlayer deal
                        deal.UnplayedCardMap[seat]
                    let legalPlays =
                        ClosedDeal.legalPlays hand deal.ClosedDeal
                            |> Seq.toArray
                    if legalPlays.Length = 1 then
                        Array.exactlyOne legalPlays,
                        None
                    else
                        let play =
                            Trickster.player.Play hand deal.ClosedDeal
                        let regrets =
                            let strategy =
                                [|
                                    for card in legalPlays do
                                        if card = play then 1.0f
                                        else 0.0f
                                |]
                            let mean = Array.average strategy
                            strategy
                                |> Array.map (fun x -> x - mean)
                                |> DenseVector.ofArray
                                |> Strategy.toWide legalPlays
                        play,
                        AdvantageSample.create
                            hand deal.ClosedDeal regrets 1
                            |> Some

                match sampleOpt with
                    | Some sample -> yield sample
                    | None -> ()

                let deal = OpenDeal.addPlay play deal
                match Game.tryUpdateScore deal Score.zero with
                    | Some _ -> ()
                    | None -> yield! loop deal
            }

        OpenDeal.generate
            settings.Random
            numDeals
            (loop >> Seq.toArray)
                |> Array.concat

    /// Trains a model directly.
    let train numDeals =

            // generate training data
        printfn $"Number of deals: {numDeals}"
        let samples =
            generateTrainingData numDeals
                |> Seq.toArray
        printfn $"Number of samples: {samples.Length}"

            // train model
        let model = new AdvantageModel(settings.Device)
        AdvantageModel.train 0 samples model

            // save trained model
        Path.Combine(
            settings.ModelDirPath,
            $"AdvantageModel.pt")
                |> model.save
                |> ignore

            // evaluate trained model
        let avgPayoff =
            Tournament.run
                settings.Random
                Trickster.player
                (Trainer.createPlayer model)
        settings.Writer.add_scalar(
            $"advantage tournament", avgPayoff, 0)
