#load "Domain.fsx"#load "Result.fsx"
namespace Poker

open Poker

module Parsing =
    let parseGameType (input: string) =
        let gameTypeIndex = input.IndexOf(' ')
        if (gameTypeIndex > 0) then
            let gameType = input.Substring(0, gameTypeIndex)
            let rest = input.Substring(gameTypeIndex + 1)
            match gameType with
            | "texas-holdem" -> Ok(TexasGame, rest)
            | "omaha-holdem" -> Ok(OmahaGame, rest)
            | "five-card-draw" -> Ok(FiveCardGame, rest)
            | _ -> Error(sprintf "Unknown game type '%s'" gameType)
        else
            Error "Can't find game type definition"

    let private parseCard (rank, suit) =
        let parseSuit suit =
            match suit with
            | 's' -> Ok Spades
            | 'h' -> Ok Hearts
            | 'd' -> Ok Diamonds
            | 'c' -> Ok Clubs
            | _ -> Error(sprintf "Unknown card suit '%c'" suit)

        let parseRank rank =
            match rank with
            | 'A' -> Ok Ace
            | 'K' -> Ok King
            | 'Q' -> Ok Queen
            | 'J' -> Ok Jack
            | 'T' -> Ok Ten
            | '9' -> Ok Nine
            | '8' -> Ok Eight
            | '7' -> Ok Seven
            | '6' -> Ok Six
            | '5' -> Ok Five
            | '4' -> Ok Four
            | '3' -> Ok Three
            | '2' -> Ok Two
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
            |> Seq.map (function
                | [| r; s |] -> parseCard (r, s)
                | xs -> failwithf "That's a bug. Expected to have an array of length 2, but got %A" xs)
            |> Seq.toList
            |> Result.ofAllOk
            |> Result.mapError (sprintf "%A")

    let parseBoard (input: string) =
        let boardIndex = input.IndexOf(' ')
        if boardIndex > 0 then
            let boardInput = input.Substring(0, boardIndex)
            let rest = input.Substring(boardIndex + 1)

            boardInput
            |> parseCards
            |> Result.bind Board.Create
            |> Result.mapError (sprintf "Invalid board '%s': %s" boardInput)
            |> Result.map (fun board -> (board, rest))
        else
            Error "Can't find board definition"

    let parseHands (createHand: Card list -> Result<Hand, string>) (input: string) =
        let parseHand handInput =
            handInput
            |> parseCards
            |> Result.bind createHand
            |> Result.mapError (sprintf "Invalid hand '%s': %s" handInput)

        let hands = input.Split(' ') |> Array.map parseHand

        if hands.Length > 0
        then hands |> List.ofArray |> Result.ofAllOk
        else Error [ "At least one hand is required" ]

    let parseGameWithBoard (parseHands: string -> Result<Hand list, string list>) createGame (input: string) =
        parseBoard input
        |> Result.bind (fun (board, rest) ->
            parseHands rest
            |> Result.mapError (sprintf "%A")
            |> Result.map (fun hands -> (board, hands)))
        |> Result.map createGame
