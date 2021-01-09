namespace Hearts.PlayKiller

open PlayingCards
open Hearts

/// A type is either a primitive type (e.g. Boolean), or
/// a function type.
type Type =
    | TBool
    | TInt
    | TRank
    | TSuit
    | TCard
    | TCardSet
    | TFunction of Input : Type * Output : Type

module Type =

    /// Numeric type?
    let isNumeric = function
        | TInt
        | TRank -> true
        | _ -> false

/// An expression represents a typed value.
type Expr =

        // general
    | Variable of Index : int
    | Lambda of ParamType : Type * Body : Expr
    | Apply of Lambda : Expr * Arg : Expr

        // bool
    | BoolLiteral of bool
    | If of Cond : Expr * Then : Expr * Else : Expr
    | Equal of Expr * Expr

        // int
    | IntLiteral of int
    | GreaterThan of Expr * Expr
    | Add of IntLeft : Expr * IntRight : Expr
    | Subtract of IntLeft : Expr * IntRight : Expr

        // rank
    | RankLiteral of Rank
    | CardRank of Card : Expr

        // suit
    | SuitLiteral of Suit
    | CardSuit of Card : Expr

        // card
    | RankSuitCard of Rank : Expr * Suit : Expr

        // card set
    | CardSetLiteral of Set<Card>
    | MyHand
    | LegalPlays
    | CardSetSingleton of Expr
    | CardSetCount of CardSet : Expr
    | CardSetMin of CardSet : Expr
    | Where of CardSet : Expr * Lambda : Expr
    | TakeAscending of CardSet : Expr * Count : Expr
    | TakeDescending of CardSet : Expr * Count : Expr
    | Union of CardSetLeft : Expr * CardSetRight : Expr

module Expr =

    /// Answers the type of the given expression, if it is well-typed.
    let typeOf expr =

        /// Raises a type error.
        let typeError (expected : Type) (actual : Type) =
            assert(expected <> actual)
            failwith $"Type error: Expected {expected}, actual {actual}"

        /// Checks for a type error.
        let checkType expected actual cont =
            if expected <> actual then
                typeError expected actual
            else cont ()

        /// Determines the type of an expression within a given
        /// environment, which is a list of types of free variables
        /// (i.e. variables bound at a higher level than this).
        let rec loop env expr =

            let cont = loop env

            let check expected expr result =
                let actual = cont expr
                checkType expected actual (fun () -> result)

            match expr with

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
                    match cont lambda with
                        | TFunction (inType, outType) ->
                            check inType arg outType
                        | _ -> failwith "Not a function"

                | BoolLiteral _ -> TBool

                    // if cond then trueBranch else falseBranch
                | If (cond, trueBranch, falseBranch) ->
                    checkType TBool (cont cond) (fun () ->
                        let trueType = cont trueBranch
                        let falseType = cont falseBranch
                        checkType trueType falseType (fun () ->
                            trueType))

                | Equal (left, right) ->
                    let leftType = cont left
                    let rightType = cont right
                    checkType leftType rightType (fun () ->
                        TBool)

                | IntLiteral _ -> TInt

                | GreaterThan (left, right) ->
                    let leftType = cont left
                    if Type.isNumeric leftType then
                        check leftType right TBool
                    else failwith "Not numeric"

                | Add (left, right)
                | Subtract (left, right) ->
                    checkType TInt (cont left) (fun () ->
                        check TInt right TInt)

                | RankLiteral _ -> TRank

                | CardRank cardExpr ->
                    check TCard cardExpr TRank

                | SuitLiteral _ -> TSuit

                | CardSuit cardExpr ->
                    check TCard cardExpr TSuit

                | RankSuitCard (rankExpr, suitExpr) ->
                    checkType TRank (cont rankExpr) (fun () ->
                        check TSuit suitExpr TCard)

                | CardSetLiteral _ -> TCardSet

                | MyHand
                | LegalPlays -> TCardSet

                | CardSetSingleton cardExpr ->
                    check TCard cardExpr TCardSet

                | CardSetCount cardSetExpr ->
                    check TCardSet cardSetExpr TInt

                | CardSetMin cardSetExpr ->
                    check TCardSet cardSetExpr TCard

                | Where (cardSetExpr, lambda) ->
                    checkType TCardSet (cont cardSetExpr) (fun () ->
                        check (TFunction (TCard, TBool)) lambda TCardSet)

                | TakeAscending (cardSetExpr, countExpr)
                | TakeDescending (cardSetExpr, countExpr) ->
                    checkType TCardSet (cont cardSetExpr) (fun () ->
                        check TInt countExpr TCardSet)

                | Union (left, right) ->
                    checkType TCardSet (cont left) (fun () ->
                        check TCardSet right TCardSet)

        loop [] expr

    /// Replaces all occurrences of param with arg in body.
    let rec private subst iParam arg body =
        let cont = subst iParam arg   // shorthand for simple recursive substitution
        match body with

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

                // no effect
            | BoolLiteral _
            | IntLiteral _
            | RankLiteral _
            | SuitLiteral _
            | CardSetLiteral _
            | MyHand
            | LegalPlays -> body

                // recursively substitute
            | Equal (left, right) ->
                Equal (cont left, cont right)
            | GreaterThan (left, right) ->
                GreaterThan (cont left, cont right)
            | Add (left, right) ->
                Add (cont left, cont right)
            | Subtract (left, right) ->
                Subtract (cont left, cont right)
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
            | TakeAscending (cardSetExpr, countExpr) ->
                TakeAscending (cont cardSetExpr, cont countExpr)
            | TakeDescending (cardSetExpr, countExpr) ->
                TakeDescending (cont cardSetExpr, cont countExpr)
            | Union (left, right) ->
                Union (cont left, cont right)
            | If (cond, trueBranch, falseBranch) ->
                If (cont cond, cont trueBranch, cont falseBranch)
            | Apply (lambda, arg) ->
                Apply (cont lambda, cont arg)

    module private RankSuitCard =

        /// Creates a card expression from the given card.
        let fromCard (card : Card) =
            RankSuitCard (
                RankLiteral card.Rank,
                SuitLiteral card.Suit)

    /// Evaluates the given expression using the given deal.
    let eval deal expr =

        let rec loop = function

                // function application
            | Apply (lambda, arg) ->
                match loop lambda with
                    | Lambda (_, body) ->
                        subst 0 arg body
                            |> loop
                    | _ -> failwith "Not a function"

                // evaluate correct branch only
            | If (cond, trueBranch, falseBranch) ->
                match loop cond with
                    | BoolLiteral true  -> loop trueBranch
                    | BoolLiteral false -> loop falseBranch
                    | _ -> failwith "Condition must be of type Boolean"

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
                let flag = (loop left = loop right)
                BoolLiteral flag

            | GreaterThan (left, right) ->
                match loop left, loop right with
                    | IntLiteral leftValue, IntLiteral rightValue ->
                        BoolLiteral (leftValue > rightValue)
                    | RankLiteral leftValue, RankLiteral rightValue ->
                        BoolLiteral (leftValue > rightValue)
                    | _ -> failwith "Type error"

            | Add (leftInt, rightInt) ->
                match loop leftInt, loop rightInt with
                    | IntLiteral left, IntLiteral right ->
                        IntLiteral (left + right)
                    | _ -> failwith "Type error"

            | Subtract (leftInt, rightInt) ->
                match loop leftInt, loop rightInt with
                    | IntLiteral left, IntLiteral right ->
                        IntLiteral (left - right)
                    | _ -> failwith "Type error"

            | CardSetCount cardSetExpr ->
                match loop cardSetExpr with
                    | CardSetLiteral cardSet ->
                        cardSet.Count |> IntLiteral
                    | _ -> failwith "Type error"

            | CardSetMin cardSetExpr ->
                match loop cardSetExpr with
                    | CardSetLiteral cardSet ->
                        cardSet.MinimumElement
                            |> RankSuitCard.fromCard
                    | _ -> failwith "Type error"

            | CardRank cardExpr ->
                match loop cardExpr with
                    | RankSuitCard (rankExpr, _) ->
                        loop rankExpr
                    | _ -> failwith "Type error"

            | CardSuit cardExpr ->
                match loop cardExpr with
                    | RankSuitCard (_, suitExpr) ->
                        loop suitExpr
                    | _ -> failwith "Type error"

            | CardSetSingleton cardExpr ->
                match loop cardExpr with
                    | RankSuitCard (rankExpr, suitExpr) ->
                        match loop rankExpr, loop suitExpr with
                            | RankLiteral rank, SuitLiteral suit ->
                                Card(rank, suit)
                                    |> Set.singleton
                                    |> CardSetLiteral
                            | _ -> failwith "Type error"
                    | _ -> failwith "Type error"

            | RankSuitCard (rankExpr, suitExpr) ->
                RankSuitCard (loop rankExpr, loop suitExpr)

            | Where (cardSetExpr, lambda) ->
                match loop cardSetExpr, loop lambda with
                    | CardSetLiteral cardSet, Lambda (argType, _)
                        when argType = TCard ->
                        cardSet
                            |> Set.filter (fun card ->
                                let cardExpr =
                                    card |> RankSuitCard.fromCard
                                match loop <| Apply (lambda, cardExpr) with
                                    | BoolLiteral flag -> flag
                                    | _ -> failwith "Type error")
                            |> CardSetLiteral
                    | _ -> failwith "Type error"

            | TakeAscending (cardSetExpr, countExpr) ->
                match loop cardSetExpr, loop countExpr with
                    | CardSetLiteral cardSet, IntLiteral count ->
                        cardSet
                            |> Seq.sort
                            |> Seq.truncate count
                            |> set
                            |> CardSetLiteral
                    | _ -> failwith "Type error"

            | TakeDescending (cardSetExpr, countExpr) ->
                match loop cardSetExpr, loop countExpr with
                    | CardSetLiteral cardSet, IntLiteral count ->
                        cardSet
                            |> Seq.sortDescending
                            |> Seq.truncate count
                            |> set
                            |> CardSetLiteral
                    | _ -> failwith "Type error"

            | Union (leftCardSetExpr, rightCardSetExpr) ->
                match loop leftCardSetExpr, loop rightCardSetExpr with
                    | CardSetLiteral left, CardSetLiteral right ->
                        Set.union left right
                            |> CardSetLiteral
                    | _ -> failwith "Type error"

                // no effect
            | expr -> expr

        loop expr
