﻿namespace Hearts.Web.Client

open Browser

open Fable.SimpleJson

open PlayingCards
open Hearts

/// Persistent state.
type PersistentState =
    {
        /// Total number of points taken by each player.
        TotalScore : Score

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
            TotalScore = Score.zero
            RandomState = Random().State   // start with arbitrary seed
            Dealer = Seat.South
            DealOpt = None
        }

    /// Local storage key.
    let private key = "HeartsPersistentState"

    /// Saves the given state.
    let save (persState : PersistentState) =
        WebStorage.localStorage[key]
            <- Json.serialize persState

    /// Answers the current state.
    let get () =
        let json = WebStorage.localStorage[key] 
        if isNull json then
            save initial
            initial
        else
            Json.parseAs<PersistentState>(json)

type PersistentState with

    /// Saves this state.
    member persState.Save() =
        PersistentState.save persState
        persState
