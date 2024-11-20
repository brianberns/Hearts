namespace Hearts.Web.Client

open Browser
open Hearts.Web.Client   // force AutoOpen

module Session =

    /// Runs a new session.
    let run surface persState =

        /// Plays a pair of duplicate games.
        let rec loop persState =
            async {
                if persState.DealOpt.IsSome then
                    console.log("Finishing deal in progress")
                else
                    console.log("New deal")

                let! persState = Deal.run surface persState
                do! loop persState
        }

        async {
            try
                do! loop persState
            with ex ->
                console.log(ex.StackTrace)
                window.alert(ex.StackTrace)
        } |> Async.StartImmediate

module App =

        // track animation speed slider
    let speedSlider = ~~"#animationSpeed"
    speedSlider.``val``(Settings.Current.AnimationSpeed)
    speedSlider.change(fun () ->
        { AnimationSpeed = speedSlider.``val``() }
            |> Settings.save)

        // prepare deck table
    DealView.prepareDeckTable ()

        // start a session when the browser is ready
    (~~document).ready(fun () ->
        let surface = ~~"main"
        PersistentState.get () |> Session.run surface)
