module Poker.Evaluation

open Poker

type HandCase =
    { BoardCards: Card list
      HandCards: Card list }
    static member Create (boardCards: Card list) (handCards: Card list) =
        if (boardCards.Length + handCards.Length) <> 5
        then failwith "Hand case must have 5 cards for strength evaluation"

        { BoardCards = boardCards
          HandCards = handCards }

type HandValue =
    | StraightFlush of StraightFlush
    | FourOfKind of SomeOfRankCards
    | FullHouse of FullHouse
    | Flush of Flush
    | Straight of Straight
    | ThreeOfKind of ThreeOfKind
    | TwoPairs of TwoPairs
    | Pair of Pair
    | HighCard of Card list

and HandValueCards = { Cards: Card list; Rest: Card list }

and SomeOfRankCards = CardRank * HandValueCards

and Pair = PairCards of SomeOfRankCards

and ThreeOfKind = ThreeOfKindCards of SomeOfRankCards

and Straight = { Cards: Card list }

and Flush = { Cards: Card list }

and StraightFlush =
    | StraightFlushValues of Straight * Flush

    static member Create values =
        match values with
        | Straight straight, Flush flush -> StraightFlush(StraightFlushValues(straight, flush))
        | Flush flush, Straight straight -> StraightFlush(StraightFlushValues(straight, flush))
        | _ -> failwithf "StraightFlush expects Straight and Flush as input"

and FullHouse =
    | FullHouseValues of ThreeOfKind * Pair

    static member Create values =
        match values with
        | ThreeOfKind three, Pair pair -> FullHouse(FullHouseValues(three, pair))
        | Pair pair, ThreeOfKind three -> FullHouse(FullHouseValues(three, pair))
        | _ -> failwithf "FullHouse expects ThreeOfKind and Pair as input"

and TwoPairs =
    | TwoPairsValues of Pair list

    static member Create values =
        match values with
        | Pair fst, Pair snd -> TwoPairs(TwoPairsValues([ fst; snd ]))
        | _ -> failwithf "TwoPairs expects two Pair's as input"

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
             |> List.sortByDescending (fun c -> getRankOrder c.Rank))

let private zipCompare comparer fst snd =
    (fst, snd)
    ||> Seq.zip
    |> Seq.map (comparer)
    |> Seq.tryFind ((<>) 0)
    |> Option.orElse (Some 0)
    |> Option.get

let private compareRanks (fst: CardRank) (snd: CardRank) =
    (getRankOrder fst) - (getRankOrder snd)

let private compareCards (fst: Card list) (snd: Card list) =
    (fst, snd)
    ||> zipCompare (fun (fst, snd) -> compareRanks fst.Rank snd.Rank)

let private compareSomeOfRankCards ((fstRank, fstValue): SomeOfRankCards) ((sndRank, sndValue): SomeOfRankCards) =
    let rankDiff = compareRanks fstRank sndRank
    if rankDiff <> 0
    then rankDiff
    else compareCards fstValue.Rest sndValue.Rest

let private compareTwoPairs (TwoPairsValues fst) (TwoPairsValues snd) =
    let getSortedRanks handValue =
        handValue
        |> List.map (fun (PairCards (rank, _)) -> rank)
        |> List.sortBy getRankOrder

    let cardsDiff =
        (getSortedRanks fst, getSortedRanks snd)
        ||> zipCompare (fun (fst, snd) -> compareRanks fst snd)

    if cardsDiff <> 0 then
        cardsDiff
    else
        let getRest handValue =
            let pairs =
                handValue
                |> List.map (fun (PairCards (_, v)) -> v)

            match pairs with
            | [ fst; snd ] -> fst.Rest |> List.except snd.Cards
            | _ -> failwith "TwoPairs should consist of two Pair's"

        compareCards (getRest fst) (getRest snd)

let private compareStraights (fst: Straight) (snd: Straight) =
    let getLowestOrder (cards: Card list) =
        let lowestCard = cards |> List.last
        let highestCard = cards |> List.head
        match lowestCard.Rank, highestCard.Rank with
        | Two, Ace -> -1
        | _ -> getRankOrder lowestCard.Rank

    (getLowestOrder fst.Cards)
    - (getLowestOrder snd.Cards)

let private compareStraightFlushes (StraightFlushValues (fst, _)) (StraightFlushValues (snd, _)) =
    compareStraights fst snd

let private compareFullHouses (FullHouseValues (ThreeOfKindCards fstThree, _))
                              (FullHouseValues (ThreeOfKindCards sndThree, _))
                              =
    compareSomeOfRankCards fstThree sndThree

let private compareValues fst snd =
    let getValueOrder value =
        match value with
        | StraightFlush _ -> 8
        | FourOfKind _ -> 7
        | FullHouse _ -> 6
        | Flush _ -> 5
        | Straight _ -> 4
        | ThreeOfKind _ -> 3
        | TwoPairs _ -> 2
        | Pair _ -> 1
        | HighCard _ -> 0

    let valueOrderDiff =
        (getValueOrder fst) - (getValueOrder snd)

    if valueOrderDiff <> 0 then
        valueOrderDiff
    else
        match fst, snd with
        | HighCard f, HighCard s -> compareCards f s
        | Pair (PairCards f), Pair (PairCards s) -> compareSomeOfRankCards f s
        | TwoPairs f, TwoPairs s -> compareTwoPairs f s
        | ThreeOfKind (ThreeOfKindCards f), ThreeOfKind (ThreeOfKindCards s) -> compareSomeOfRankCards f s
        | Straight f, Straight s -> compareStraights f s
        | Flush f, Flush s -> compareCards f.Cards s.Cards
        | FullHouse f, FullHouse s -> compareFullHouses f s
        | FourOfKind f, FourOfKind s -> compareSomeOfRankCards f s
        | StraightFlush f, StraightFlush s -> compareStraightFlushes f s
        | f, s -> failwithf "All the cases should be covered but got a %A %A" f s

let private highCard (OrderedCards cards): HandValue = HighCard cards

let private straight (OrderedCards cards): (HandValue list) =
    let orders =
        cards |> List.map (fun c -> getRankOrder c.Rank)

    let normalized =
        let minOrder = orders |> List.last
        if minOrder = 0
        then orders
        else orders |> List.map (fun order -> order - minOrder)

    let aceOrder = getRankOrder Ace

    match normalized with
    | [ 4; 3; 2; 1; 0 ] -> [ Straight({ Cards = cards }) ]
    | [ max; 3; 2; 1; 0 ] when max = aceOrder -> [ Straight({ Cards = cards }) ]
    | _ -> []

let private flush (OrderedCards cards): (HandValue list) =
    let suits =
        cards
        |> List.map (fun c -> c.Suit)
        |> List.distinct

    if suits.Length = 1
    then [ Flush({ Cards = cards }) ]
    else []

let private multipleOfKind (OrderedCards cards): (HandValue list) =
    let ofOtherRank rank =
        cards |> List.filter (fun c -> c.Rank <> rank)

    cards
    |> List.groupBy (fun c -> c.Rank)
    |> List.choose (fun (r, c) ->
        match c.Length with
        | 2 -> Some(Pair(PairCards(r, { Cards = c; Rest = ofOtherRank r })))
        | 3 -> Some(ThreeOfKind(ThreeOfKindCards(r, { Cards = c; Rest = ofOtherRank r })))
        | 4 -> Some(FourOfKind(r, { Cards = c; Rest = ofOtherRank r }))
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

let evaluateHand (hand: HandCase): HandValue =
    let orderedCards =
        OrderedCardList.Create(hand.BoardCards @ hand.HandCards)

    let simpleHandValues =
        simpleEvaluators
        |> List.collect (fun ev -> ev orderedCards)

    let handValues =
        complexEvaluators
        |> List.fold (fun (high, rest) ev ->
            match ev rest with
            | Some (value, rest) -> (value :: high, rest)
            | None -> high, rest) ([], simpleHandValues)
        ||> (@)

    if handValues.Length = 0
    then highCard orderedCards
    else if handValues.Length = 1
    then handValues.[0]
    else handValues |> List.minWith compareValues

let sortHands (getHandCases: 'a -> HandCase list) (hands: 'a list): (((('a * HandCase) * HandValue) list) list) =
    let ordered =
        hands
        |> List.map (fun hand ->
            let handCases = getHandCases hand

            let (maxCase, maxValue) =
                handCases
                |> List.map (fun case -> case, evaluateHand case)
                |> List.maxWith (fun (_, fst) (_, snd) -> compareValues fst snd)

            ((hand, maxCase), maxValue))
        |> List.sortWith (fun (_, fst) (_, snd) -> compareValues fst snd)

    let equalityGroups: (('a * HandCase) * HandValue) list list = []
    (ordered, equalityGroups)
    ||> List.foldBack (fun curItem groups ->
            let curValue = snd curItem
            match groups with
            | [] -> [ [ curItem ] ]
            | curGroup :: rest ->
                let groupValue = curGroup |> List.head |> snd
                if compareValues curValue groupValue = 0
                then (curItem :: curGroup) :: rest
                else [ curItem ] :: (curGroup :: rest))
