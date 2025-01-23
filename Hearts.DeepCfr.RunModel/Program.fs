namespace Hearts.DeepCfr

open System

module Program =

    let model =
        AdvantageModel.create
            settings.HiddenSize
            settings.LearningRate
    model.Network.load(@"C:\Users\brian\source\repos\Hearts\Hearts.DeepCfr\bin\Release\net8.0\Models\AdvantageModel022.pt")
        |> ignore

    let challenger =
        Trainer.createChallenger (
            Strategy.getFromAdvantage model)

    Tournament.run
        (Random(0))
        Database.player
        challenger
        |> ignore
