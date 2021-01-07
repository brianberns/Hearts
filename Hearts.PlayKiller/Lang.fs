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
    | CardSetCount of Expr

        // rank
    | RankLiteral of Rank
    | CardRank of Expr

        // suit
    | SuitLiteral of Suit
    | CardSuit of Expr

        // card
    | CardLiteral of Card
    | CardSetMin of Expr

        // card set
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

            | CardLiteral _ -> TCard
            | CardSetMin cardSetExpr ->
                if loop env cardSetExpr = TCardSet then TCard
                else failwith "CardSet type mismatch"

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

    (*
    let eval = function

            // bool
        | BoolLiteral value -> value
        | RankEqual (rankExprA, rankExprB) ->
            (rankExprA |> RankExpr.eval)
                = (rankExprB |> RankExpr.eval)
        | SuitEqual (suitExprA, suitExprB) ->
            (suitExprA |> SuitExpr.eval)
                = (suitExprB |> SuitExpr.eval)
        | CardEqual (cardExprA, cardExprB) ->
            (cardExprA |> CardExpr.eval)
                = (cardExprB |> CardExpr.eval)

            // int
        | IntLiteral n -> n
        | Count cardSetExpr ->
            cardSetExpr
                |> CardSetExpr.eval
                |> Set.count

            // rank
        | RankLiteral rank -> rank
        | CardRank cardExpr ->
            let card =
                cardExpr |> CardExpr.eval
            card.Rank

            // suit
        | SuitLiteral suit -> suit
        | CardSuit cardExpr ->
            let card =
                cardExpr |> CardExpr.eval
            card.Suit

            // card
        | CardLiteral card -> card
        | Min cardSetExpr ->
            cardSetExpr
                |> CardSetExpr.eval
                |> Set.minElement

            // card set
        | Singleton cardExpr ->
            cardExpr
                |> CardExpr.eval
                |> Set.singleton
        | MyHand ->
            OpenDeal.currentHand deal
        | LegalPlays ->
            let hand = OpenDeal.currentHand deal
            deal.ClosedDeal
                |> ClosedDeal.legalPlays hand
                |> set
        | Where (cardSetExpr, boolExpr) ->
            cardSetExpr
                |> CardSetExpr.eval deal
                |> Set.filter (fun card ->
                    boolExpr
                        |> BoolExpr.eval)
    *)
