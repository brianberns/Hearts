﻿namespace Hearts.Web.Client

open Browser

open Fable.SimpleJson

open PlayingCards
open Hearts

/// Persistent state.
type PersistentState =
    {
        /// Number of points taken by each player.
        PointsTaken : Score

        /// State of random number generator.
        RandomState : uint64   // can't persist entire RNG

        /// Current dealer.
        Dealer : Seat

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
            PointsTaken = Score.zero
            RandomState = Random().State   // start with arbitrary seed
            Dealer = Seat.South
            DealOpt = None
        }

    /// Local storage key.
    let private key = "HeartsPersistentState"

    /// Saves the given state.
    let save (persState : PersistentState) =
#if MINI
        ()
#else
        WebStorage.localStorage[key]
            <- Json.serialize persState
#endif

    /// Answers the current state.
    let get () =
#if MINI
        initial
#else
        let json = WebStorage.localStorage[key] 
        if isNull json then
            save initial
            initial
        else
            Json.parseAs<PersistentState>(json)
#endif

type PersistentState with

    /// Saves this state.
    member persState.Save() =
        PersistentState.save persState
        persState
