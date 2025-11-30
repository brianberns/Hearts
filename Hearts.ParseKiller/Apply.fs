namespace Hearts.ParseKiller

open Hearts

module PassMap =

    let apply (passMap : Map<_, _>) deal =
        let cards =
            let seats =
                Seat.cycle (OpenDeal.currentPlayer deal)
            seq {
                for seat in seats do
                    yield! passMap[seat]
            }
        (deal, cards)
            ||> Seq.fold (fun deal card ->
                OpenDeal.addPass card deal)

module TrickPlay =

    let apply plays deal =
        (deal, plays)
            ||> Seq.fold (fun deal (seat, card) ->
                assert(OpenDeal.currentPlayer deal = seat)
                OpenDeal.addPlay card deal)
