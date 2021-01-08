namespace rec Hearts.PlayKiller

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

/// An expression represents a typed value.
type Expr =

        // bool
    | BoolLiteral of bool
    | Equal of Expr * Expr
    | GreaterThan of Expr * Expr

        // int
    | IntLiteral of int
    | Add of IntLeft : Expr * IntRight : Expr
    | Subtract of IntLeft : Expr * IntRight : Expr
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
    | TakeAscending of CardSet : Expr * Count : Expr
    | TakeDescending of CardSet : Expr * Count : Expr
    | Union of CardSetLeft : Expr * CardSetRight : Expr

        // general
    | If of Cond : Expr * Then : Expr * Else : Expr
    | Variable of Index : int
    | Lambda of ParamType : Type * Body : Expr
    | Apply of Lambda : Expr * Arg : Expr

module Expr =

    /// Answers the type of the given expression, if it is well-typed.
    let typeOf expr =

        /// Raises a type error.
        let typeError (expected : Type) (actual : Type) =
            assert(expected <> actual)
            failwith $"Type error: Expected {expected}, actual {actual}"

        let isNumeric = function
            | TInt
            | TRank -> true
            | _ -> false

        /// Determines the type of a term within a given environment,
        /// which is a list of types of free variables (i.e. variables
        /// bound at a higher level than this).
        let rec loop env expr =
            let cont = loop env
            match expr with

                | BoolLiteral _ -> TBool

                | Equal (left, right) ->
                    let leftType = cont left
                    let rightType = cont right
                    if leftType = rightType then TBool
                    else typeError leftType rightType

                | GreaterThan (left, right) ->
                    let leftType = cont left
                    if isNumeric leftType then
                        let rightType = cont right
                        if leftType = rightType then TBool
                        else typeError leftType rightType
                    else failwith "Not numeric"

                | IntLiteral _ -> TInt

                | Add (left, right)
                | Subtract (left, right) ->
                    let typ = cont left
                    if typ = TInt then
                        let typ = cont right
                        if typ = TInt then TInt
                        else typeError TInt typ
                    else typeError TInt typ

                | CardSetCount cardSetExpr ->
                    let typ = cont cardSetExpr
                    if typ = TCardSet then TInt
                    else typeError TCardSet typ

                | RankLiteral _ -> TRank

                | CardRank cardExpr ->
                    let typ = cont cardExpr
                    if typ = TCard then TRank
                    else typeError TCard typ

                | SuitLiteral _ -> TSuit

                | CardSuit cardExpr ->
                    let typ = cont cardExpr
                    if typ = TCard then TSuit
                    else typeError TCard typ

                | RankSuitCard (rankExpr, suitExpr) ->
                    let typ = cont rankExpr
                    if typ = TRank then
                        let typ = cont suitExpr
                        if typ = TSuit then TCard
                        else typeError TSuit typ
                    else typeError TRank typ

                | CardSetMin cardSetExpr ->
                    let typ = cont cardSetExpr
                    if typ = TCardSet then TCard
                    else typeError TCardSet typ

                | CardSetLiteral _ -> TCardSet

                | CardSetSingleton cardExpr ->
                    let typ = cont cardExpr
                    if typ = TCard then TCardSet
                    else typeError TCard typ

                | MyHand
                | LegalPlays -> TCardSet

                | Where (cardSetExpr, lambda) ->
                    let typ = cont cardSetExpr
                    if typ = TCardSet then
                        match cont lambda with
                             | TFunction (inType, outType) ->
                                if inType = TCard then
                                    if outType = TBool then TCardSet
                                    else typeError TBool outType
                                else typeError TCard inType
                             | typ -> typeError (TFunction (TCard, TBool)) typ
                    else typeError TCardSet typ

                | TakeAscending (cardSetExpr, countExpr)
                | TakeDescending (cardSetExpr, countExpr) ->
                    let typ = cont cardSetExpr
                    if typ = TCardSet then
                        let typ = cont countExpr
                        if typ = TInt then TCardSet
                        else typeError TInt typ
                    else typeError TCardSet typ

                | Union (cardSetExprA, cardSetExprB) ->
                    let typ = cont cardSetExprA
                    if typ = TCardSet then
                        let typ = cont cardSetExprB
                        if typ = TCardSet then TCardSet
                        else typeError TCardSet typ
                    else typeError TCardSet typ

                    // if cond then trueBranch else falseBranch
                | If (cond, trueBranch, falseBranch) ->
                    match cont cond with
                        | TBool ->   // condition must be a plain Boolean
                            let trueType = cont trueBranch
                            let falseType = cont falseBranch
                            if trueType = falseType then trueType   // branch types must match
                            else typeError trueType falseType
                        | typ -> typeError TBool typ

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
                    match cont lambda with   // first term must be a function
                        | TFunction (inType, outType) ->
                            let typ = cont arg
                            if typ = inType then outType   // argument's type must match expected input type
                            else typeError inType typ
                        | _ -> failwith "Not a function"

        loop [] expr

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

    module private RankSuitCard =

        /// Creates a card expression from the given card.
        let fromCard (card : Card) =
            RankSuitCard (
                RankLiteral card.Rank,
                SuitLiteral card.Suit)

    /// Evaluates the given expression using the given deal.
    let eval deal expr =

        let rec loop = function

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

                // no effect
            | expr -> expr

        loop expr
