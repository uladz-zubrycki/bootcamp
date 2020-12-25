module Poker.Parsing

open Poker
open System

let private revDict (dictionary: Collections.Generic.IDictionary<'a, 'b>) =
    dictionary
    |> Seq.map (fun kv -> (kv.Value, kv.Key))
    |> dict

let private suitMapping =
    [ 's', Spades
      'h', Hearts
      'd', Diamonds
      'c', Clubs ]
    |> dict

let private suitBackMapping = suitMapping |> revDict

let private rankMapping =
    [ 'A', Ace
      'K', King
      'Q', Queen
      'J', Jack
      'T', Ten
      '9', Nine
      '8', Eight
      '7', Seven
      '6', Six
      '5', Five
      '4', Four
      '3', Three
      '2', Two ]
    |> dict

let private rankBackMapping = rankMapping |> revDict

let private formatRank rank = rankBackMapping.[rank]
let private formatSuit suit = suitBackMapping.[suit]

let private formatCard (card: Card): string =
    sprintf "%c%c" (formatRank card.Rank) (formatSuit card.Suit)

let private formatHand (Hand cards) =
    cards |> List.map formatCard |> String.concat ""

let formatHands (hands: (Hand list) list): string =
    hands
    |> List.map
        (fun equalityGroup ->
            equalityGroup
            |> List.map formatHand
            |> String.concat "=")
    |> String.concat " "

let verifyInput (input: string) =
    if input.Trim().Length = 0
    then Error "Empty input"
    else Ok input

let private verifyCardsUnique (cards: Card list) =
    cards
    |> List.groupBy (fun c -> (c.Rank, c.Suit))
    |> List.filter (fun (_, group) -> group.Length > 1)
    |> List.map
        (fun ((rank, suit), group) ->
            Error(
                sprintf
                    "Cards should be unique, but '%c%c' is present %d times"
                    (formatRank rank)
                    (formatSuit suit)
                    group.Length
            ))
    |> Result.ofAllOk
    |> Result.map (fun _ -> cards)

let private consumeToken (separator: char) (input: string) =
    let (tokenIndex, restIndex) =
        let index = input.IndexOf(separator)

        if index > 0
        then (index, index + 1)
        else (input.Length, input.Length)

    (input.Substring(0, tokenIndex), input.Substring(restIndex).Trim())

let parseGameType (input: string) =
    let (gameType, rest) = consumeToken ' ' input

    match gameType with
    | "texas-holdem" -> Ok(TexasGame, rest)
    | "omaha-holdem" -> Ok(OmahaGame, rest)
    | "five-card-draw" -> Ok(FiveCardGame, rest)
    | _ -> Error(sprintf "Unknown game type '%s'" gameType)

let private parseCard (rank, suit) =
    let parseSuit suit =
        match suitMapping.TryGetValue suit with
        | true, s -> Ok s
        | _ -> Error(sprintf "Unknown card suit '%c'" suit)

    let parseRank rank =
        match rankMapping.TryGetValue rank with
        | true, r -> Ok r
        | _ -> Error(sprintf "Unknown card rank '%c'" rank)

    match (parseRank rank, parseSuit suit) with
    | Ok r, Ok s -> Ok { Rank = r; Suit = s }
    | r, s ->
        let errors =
            [ Result.tryError r; Result.tryError s ]
            |> List.choose id

        Error(sprintf "Invalid card '%c%c': %A" rank suit errors)

let parseCards (input: string) =
    if input.Length % 2 <> 0 then
        Error(sprintf "Card sequence should contain even number of chars, but got %d" input.Length)
    else
        input
        |> Seq.chunkBySize 2
        |> Seq.map
            (function
            | [| r; s |] -> parseCard (r, s)
            | xs -> failwithf "Expected to have an array of length 2, but got %A" xs)
        |> Seq.toList
        |> Result.ofAllOk
        |> Result.bind verifyCardsUnique
        |> Result.mapError (sprintf "%A")

let parseBoard (input: string) =
    if input.Length = 0 then
        Error "Can't find board definition"
    else
        let (boardInput, rest) = consumeToken ' ' input

        boardInput
        |> parseCards
        |> Result.bind Board.Create
        |> Result.mapError (sprintf "Invalid board '%s': %s" boardInput)
        |> Result.map (fun board -> (board, rest))

let parseHands (createHand: Card list -> Result<Hand, string>) (input: string) =
    let parseHand handInput =
        handInput
        |> parseCards
        |> Result.bind createHand
        |> Result.mapError (sprintf "Invalid hand '%s': %s" handInput)

    let hands =
        input.Split(' ')
        |> Array.filter (String.IsNullOrEmpty >> not)
        |> Array.map parseHand

    if hands.Length > 0
    then hands |> List.ofArray |> Result.ofAllOk
    else Error [ "At least one hand is required" ]

let parseGameWithBoard (parseHands: string -> Result<Hand list, string list>) createGame (input: string) =
    parseBoard input
    |> Result.bind
        (fun ((Board boardCards), rest) ->
            parseHands rest
            |> Result.bind
                (fun hands ->
                    let handsCards =
                        hands |> List.collect (fun (Hand cards) -> cards)

                    (boardCards @ handsCards)
                    |> verifyCardsUnique
                    |> Result.map (fun _ -> (Board(boardCards), hands)))
            |> Result.mapError (sprintf "%A"))
    |> Result.map createGame
