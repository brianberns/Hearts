namespace rec Hearts.PlayKiller

open PlayingCards
open Hearts

type Type =
    | TBool
    | TInt
    | TRank
    | TSuit
    | TCard
    | TCardSet
    | TFunction of Input : Type * Output : Type

type Expr =

        // bool
    | BoolLiteral of bool
    | Equal of Expr * Expr

        // int
    | IntLiteral of int
    | CardSetCount of CardSet : Expr

        // rank
    | RankLiteral of Rank
    | CardRank of Card : Expr

        // suit
    | SuitLiteral of Suit
    | CardSuit of Card : Expr

        // card
    | RankSuitCard of Rank : Expr * Suit : Expr
    | CardSetMin of CardSet : Expr

        // card set
    | CardSetLiteral of Set<Card>
    | CardSetSingleton of Expr
    | MyHand
    | LegalPlays
    | Where of CardSet : Expr * Lambda : Expr

        // general
    | If of Cond : Expr * Then : Expr * Else : Expr
    | Variable of Index : int
    | Lambda of ParamType : Type * Body : Expr
    | Apply of Lambda : Expr * Arg : Expr

module Expr =

    /// Answers the type of the given term, if it is well-typed.
    let typeOf term =

        /// Determines the type of a term within a given environment,
        /// which is a list of types of free variables (i.e. variables
        /// bound at a higher level than this).
        let rec loop env = function

            | BoolLiteral _ -> TBool
            | Equal (left, right) ->
                if loop env left = loop env right then TBool
                else failwith "Equality type mismatch"

            | IntLiteral _ -> TInt
            | CardSetCount cardSetExpr ->
                if loop env cardSetExpr = TCardSet then TInt
                else failwith "CardSet type mismatch"

            | RankLiteral _ -> TRank
            | CardRank cardExpr ->
                if loop env cardExpr = TCard then TRank
                else failwith "Card type mismatch"

            | SuitLiteral _ -> TSuit
            | CardSuit cardExpr ->
                if loop env cardExpr = TCard then TSuit
                else failwith "Card type mismatch"

            | RankSuitCard (rankExpr, suitExpr) ->
                if loop env rankExpr = TRank then
                    if loop env suitExpr = TSuit then TCard
                    else failwith "Suit type mismatch"
                else failwith "Rank type mismatch"
            | CardSetMin cardSetExpr ->
                if loop env cardSetExpr = TCardSet then TCard
                else failwith "CardSet type mismatch"

            | CardSetLiteral _ -> TCardSet
            | CardSetSingleton cardExpr ->
                if loop env cardExpr = TCard then TCardSet
                else failwith "Card type mismatch"

            | MyHand
            | LegalPlays -> TCardSet
            | Where (cardSetExpr, lambda) ->
                if loop env cardSetExpr = TCardSet then
                    match loop env lambda with
                         | TFunction (inType, outType) ->
                            if inType = TCard then
                                if outType = TBool then TCardSet
                                else failwith "Bool type mismatch"
                            else failwith "Card type mismatch"
                         | _ -> failwith "CardSet type mismatch"
                else failwith "CardSet type mismatch"

                // if cond then trueBranch else falseBranch
            | If (cond, trueBranch, falseBranch) ->
                match loop env cond with
                    | TBool ->   // condition must be a plain Boolean
                        let trueType = loop env trueBranch
                        let falseType = loop env falseBranch
                        if trueType = falseType then trueType   // branch types must match
                        else failwith "Branch type mismatch"
                    | _ -> failwith "Condition must be of type Boolean"

                // variable in the given environment
            | Variable i ->
                env
                    |> List.tryItem i
                    |> Option.defaultWith (fun () ->
                        failwith "Unbound variable")

                // fun (_ : paramType) -> body
            | Lambda (paramType, body) ->
                let bodyType =
                    let env' = paramType :: env   // add param type to environment
                    loop env' body
                TFunction (paramType, bodyType)

                // function application
            | Apply (lambda, arg) ->
                match loop env lambda with   // first term must be a function
                    | TFunction (inType, outType) ->
                        if loop env arg = inType then outType   // argument's type must match expected input type
                        else failwith "Unexpected argument type"
                    | _ -> failwith "Not a function"

        loop [] term

    /// Replaces all occurrences of param with arg in body.
    let rec private subst iParam arg body =
        let cont = subst iParam arg   // shorthand for simple recursive substitution
        match body with

                // no effect
            | BoolLiteral _
            | IntLiteral _
            | RankLiteral _
            | SuitLiteral _
            | CardSetLiteral _
            | MyHand
            | LegalPlays
                -> body

                // recursively substitute
            | Equal (left, right) ->
                Equal (cont left, cont right)
            | CardSetCount cardSetExpr ->
                CardSetCount (cont cardSetExpr)
            | CardRank cardExpr ->
                CardRank (cont cardExpr)
            | CardSuit cardExpr ->
                CardSuit (cont cardExpr)
            | CardSetMin cardSetExpr ->
                CardSetMin (cont cardSetExpr)
            | CardSetSingleton cardExpr ->
                CardSetSingleton (cont cardExpr)
            | RankSuitCard (rankExpr, suitExpr) ->
                RankSuitCard (cont rankExpr, cont suitExpr)
            | Where (cardSetExpr, lambda) ->
                Where (cont cardSetExpr, cont lambda)
            | If (cond, trueBranch, falseBranch) ->
                If (cont cond, cont trueBranch, cont falseBranch)
            | Apply (lambda, arg) ->
                Apply (cont lambda, cont arg)

                // substitute iff variables match
            | Variable iVar ->
                match compare iVar iParam with
                     | -1 -> body                  // not a match: no effect
                     |  0 -> arg                   // match: substitute value
                     |  1 -> Variable (iVar - 1)   // free variable: shift to maintain external references to it
                     |  _ -> failwith "Unexpected"

                // current var0 is known as var1 within
            | Lambda (argType, body') ->
                let body' = subst (iParam+1) arg body'
                Lambda (argType, body')

    let eval deal expr =
        let cont = eval deal
        match expr with

            | MyHand ->
                deal
                    |> OpenDeal.currentHand
                    |> CardSetLiteral

            | LegalPlays ->
                let hand = deal |> OpenDeal.currentHand
                deal.ClosedDeal
                    |> ClosedDeal.legalPlays hand
                    |> set
                    |> CardSetLiteral

            | Equal (left, right) ->
                let flag =
                    (Expr.eval deal left) = (Expr.eval deal right)
                BoolLiteral flag

            | CardSetCount cardSetExpr ->
                match cont cardSetExpr with
                    | CardSetLiteral cardSet ->
                        cardSet.Count |> IntLiteral
                    | _ -> failwith "Type error"

            | CardSetMin cardSetExpr ->
                match cont cardSetExpr with
                    | CardSetLiteral cardSet ->
                        let card = cardSet.MinimumElement
                        RankSuitCard (
                            RankLiteral card.Rank,
                            SuitLiteral card.Suit)
                    | _ -> failwith "Type error"

            | CardRank cardExpr ->
                match cont cardExpr with
                    | RankSuitCard (rankExpr, _) ->
                        cont rankExpr
                    | _ -> failwith "Type error"

            | CardSuit cardExpr ->
                match cont cardExpr with
                    | RankSuitCard (_, suitExpr) ->
                        cont suitExpr
                    | _ -> failwith "Type error"

            | CardSetSingleton cardExpr ->
                match cont cardExpr with
                    | RankSuitCard (rankExpr, suitExpr) ->
                        match cont rankExpr, cont suitExpr with
                            | RankLiteral rank, SuitLiteral suit ->
                                Card(rank, suit)
                                    |> Set.singleton
                                    |> CardSetLiteral
                            | _ -> failwith "Type error"
                    | _ -> failwith "Type error"

            | RankSuitCard (rankExpr, suitExpr) ->
                RankSuitCard (cont rankExpr, cont suitExpr)

            | Where (cardSetExpr, lambda) ->
                match cont cardSetExpr, cont lambda with
                    | CardSetLiteral cardSet, Lambda (argType, _)
                        when argType = TCard ->
                        cardSet
                            |> Set.filter (fun card ->
                                let cardExpr =
                                    RankSuitCard (
                                        RankLiteral card.Rank,
                                        SuitLiteral card.Suit)
                                match cont <| Apply (lambda, cardExpr) with
                                    | BoolLiteral flag -> flag
                                    | _ -> failwith "Type error")
                            |> CardSetLiteral
                    | _ -> failwith "Type error"

                // function application
            | Apply (lambda, arg) ->
                match cont lambda with
                    | Lambda (_, body) ->
                        subst 0 arg body
                            |> cont
                    | _ -> failwith "Not a function"

                // evaluate correct branch only
            | If (cond, trueBranch, falseBranch) ->
                match cont cond with
                    | BoolLiteral true  -> cont trueBranch
                    | BoolLiteral false -> cont falseBranch
                    | _ -> failwith "Condition must be of type Boolean"

                // no effect
            | _ -> expr
