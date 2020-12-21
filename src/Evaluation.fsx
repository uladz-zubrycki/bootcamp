#load "Domain.fs"
namespace Poker

open Poker

type HandValueCards = { Cards: Card list; Rest: Card list }

type HandValue =
    | StraightFlush of StraightFlush
    | FourOfKind of CardRank * HandValueCards
    | FullHouse of FullHouse
    | Flush of CardSuit * HandValueCards
    | Straight of HandValueCards
    | ThreeOfKind of CardRank * HandValueCards
    | TwoPairs of TwoPairs
    | Pair of CardRank * HandValueCards
    | HighCard of HandValueCards

and StraightFlush =
    | StraightFlushValues of HandValue * HandValue

    static member Create values =
        match values with
        | (Straight _, Flush _)
        | (Flush _, Straight _) -> StraightFlush(StraightFlushValues values)
        | _ -> failwithf "That's a bug. StraightFlush expects Straight and Flush as input"

and FullHouse =
    | FullHouseValues of HandValue * HandValue

    static member Create values =
        match values with
        | (ThreeOfKind _, Pair _)
        | (Pair _, ThreeOfKind _) -> FullHouse(FullHouseValues values)
        | _ -> failwithf "That's a bug. FullHouse expects ThreeOfKind and Pair as input"

and TwoPairs =
    | TwoPairsValues of HandValue * HandValue

    static member Create values =
        match values with
        | (Pair _, Pair _) -> TwoPairs(TwoPairsValues values)
        | _ -> failwithf "That's a bug. TwoPairs expects two Pair's as input"

module Evaluation =
    let private getRankOrder rank =
        match rank with
        | Ace -> 12
        | King -> 11
        | Queen -> 10
        | Jack -> 9
        | Ten -> 8
        | Nine -> 7
        | Eight -> 6
        | Seven -> 5
        | Six -> 4
        | Five -> 3
        | Four -> 2
        | Three -> 1
        | Two -> 0

    type private OrderedCardList =
        | OrderedCards of Card list
        static member Create cards =
            OrderedCards
                (cards
                 |> List.sortBy (fun c -> getRankOrder c.Rank))

    let private highCard (OrderedCards cards): HandValue =
        HighCard
            ({ Cards = cards |> List.take 1
               Rest = cards |> List.skip 1 })

    let private straight (OrderedCards cards): (HandValue list) =
        let orders =
            cards |> List.map (fun c -> getRankOrder c.Rank)

        let normalized =
            let minOrder = orders |> List.head
            if minOrder = 0
            then orders
            else orders |> List.map (fun order -> order - minOrder)

        let aceOrder = getRankOrder Ace

        match normalized with
        | [ 0; 1; 2; 3; 4 ] -> [ Straight({ Cards = cards; Rest = [] }) ]
        | [ 0; 1; 2; 3; max ] when max = aceOrder -> [ Straight({ Cards = cards; Rest = [] }) ]
        | _ -> []

    let private flush (OrderedCards cards): (HandValue list) =
        let suits =
            cards
            |> List.map (fun c -> c.Suit)
            |> List.distinct

        if suits.Length = 1 then
            let suit = suits |> List.head
            [ Flush(suit, { Cards = cards; Rest = [] }) ]
        else
            []

    let private multipleOfKind (OrderedCards cards): (HandValue list) =
        let rest rank =
            cards |> List.filter (fun c -> c.Rank <> rank)

        cards
        |> List.groupBy (fun c -> c.Rank)
        |> List.choose (fun (rank, cards) ->
            match cards.Length with
            | 2 -> Some(Pair(rank, { Cards = cards; Rest = rest rank }))
            | 3 -> Some(ThreeOfKind(rank, { Cards = cards; Rest = rest rank }))
            | 4 -> Some(FourOfKind(rank, { Cards = cards; Rest = rest rank }))
            | _ -> None)

    let private createComplexEvaluator choose create (values: HandValue list): ((HandValue * HandValue list) option) =
        match values |> List.choose (choose) with
        | [ fst; snd ] -> Some(create (fst, snd), values |> List.except [ fst; snd ])
        | _ -> None

    let private straightFlush (values: HandValue list): ((HandValue * HandValue list) option) =
        createComplexEvaluator (function
            | Straight _ as x -> Some x
            | Flush _ as x -> Some x
            | _ -> None) StraightFlush.Create values

    let private fullHouse (values: HandValue list): ((HandValue * HandValue list) option) =
        createComplexEvaluator (function
            | Pair _ as x -> Some x
            | ThreeOfKind _ as x -> Some x
            | _ -> None) FullHouse.Create values

    let private twoPairs (values: HandValue list): ((HandValue * HandValue list) option) =
        createComplexEvaluator (function
            | Pair _ as x -> Some x
            | _ -> None) TwoPairs.Create values

    let private simpleEvaluators = [ flush; multipleOfKind; straight ]
    let private complexEvaluators = [ twoPairs; fullHouse; straightFlush ]

    let evaluateHand (cards: Card list): HandValue list =
        let orderedCards = OrderedCardList.Create cards

        let simpleHandValues =
            simpleEvaluators
            |> List.collect (fun ev -> ev orderedCards)

        let handValues =
            complexEvaluators
            |> List.fold (fun (high, rest) ev ->
                match ev rest with
                | Some (value, rest) -> (value :: high, rest)
                | None -> (high, rest)) ([], simpleHandValues)
            ||> (@)

        if handValues.Length = 0 then [ highCard orderedCards ] else handValues
