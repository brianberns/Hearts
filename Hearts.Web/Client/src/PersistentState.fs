namespace Hearts.Web.Client

open Browser

open Fable.SimpleJson

open PlayingCards
open Hearts

/// Persistent state.
type PersistentState =
    {
        /// Structure version number.
        VersionNum : int

        /// Number of games won by each player.
        GamesWon : Score

        /// Game score at the start of the current deal. Updated
        /// only at the end of the deal.
        GameScore : Score

        /// State of random number generator.
        RandomState : uint64   // can't persist entire RNG

        /// Current dealer.
        Dealer : Seat

        /// Exchange direction.
        ExchangeDirection : ExchangeDirection

        /// Current deal, if any.
        DealOpt : Option<OpenDeal>
    }

    /// Current deal.
    member this.Deal =
        match this.DealOpt with
            | Some deal -> deal
            | None -> failwith "No current deal"

module PersistentState =

    /// Initial persistent state.
    let private initial =
        {
            VersionNum = 7   // exchange direction
            GamesWon = Score.zero
            GameScore = Score.zero
            RandomState = Random().State   // start with arbitrary seed
            Dealer = Seat.South
            ExchangeDirection = ExchangeDirection.Left
            DealOpt = None
        }

    /// Local storage key.
    let private key = "HeartsPersistentState"

    /// Saves the given state.
    let save (persState : PersistentState) =
        WebStorage.localStorage[key]
            <- Json.serialize persState

    /// Save's initial state on client.
    let private createInitial () =
        save initial
        initial

    /// Answers the current state.
    let get () =
        let json = WebStorage.localStorage[key] 
        if isNull json then
            createInitial ()
        else
            let state =
                try Json.parseAs<PersistentState>(json)
                with _ -> createInitial ()
            if state.VersionNum < initial.VersionNum then   // ignore obsolete state
                createInitial ()
            else state

type PersistentState with

    /// Saves this state.
    member persState.Save() =
        PersistentState.save persState
