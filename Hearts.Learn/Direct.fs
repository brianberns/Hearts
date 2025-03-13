namespace Hearts.Learn

open System
open System.IO

open MathNet.Numerics.LinearAlgebra

open Hearts
open Hearts.Model

module Direct =

    /// Random number generator.
    let private rng = Random()

    /// Generates training data using a standard player.
    let private generateTrainingData (player : Player) numDeals =

        let rec loop deal =
            seq {
                let play, sampleOpt =
                    let infoSet = OpenDeal.currentInfoSet deal
                    let _, legalActions =
                        InformationSet.legalActions infoSet
                    if legalActions.Length = 1 then
                        Array.exactlyOne legalActions,
                        None
                    else
                        let _, action = player.Act infoSet
                        let regrets =
                            let strategy =
                                [|
                                    for card in legalActions do
                                        if card = action then 1.0f
                                        else 0.0f
                                |]
                            let mean = Array.average strategy
                            strategy
                                |> Array.map (fun x -> x - mean)
                                |> DenseVector.ofArray
                                |> Strategy.toWide legalActions
                        action,
                        AdvantageSample.create infoSet regrets 1
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
            rng
            numDeals
            (loop >> Seq.toArray)
                |> Array.concat

    /// Trains a model directly.
    let train numDeals =

            // generate training data
        printfn $"Number of deals: {numDeals}"
        let samples =
            generateTrainingData Trickster.player numDeals
                |> Seq.toArray
        printfn $"Number of samples: {samples.Length}"

            // train model
        let model =
            new AdvantageModel(
                settings.HiddenSize,
                settings.Device)
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
                rng
                Trickster.player
                (Trainer.createPlayer model)
        settings.Writer.add_scalar(
            $"advantage tournament", avgPayoff, 0)
