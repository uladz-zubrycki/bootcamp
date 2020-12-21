namespace Poker

open Poker
open Poker.Parsing
open Poker.Evaluation

module Texas =
    let parse input =
        let createGame (board, hands) =
            Texas { Board = board; Hands = hands }

        let parseTexasHands = parseHands (Hand.Create 2)

        input
        |> parseGameWithBoard parseTexasHands createGame
        |> Result.mapError (sprintf "Abnormal Texas Hold'em game: %s")

    let evaluate (board: Board) (hands: Hand list) =
        hands
        |> sortHands (fun (Hand cards) ->
            let (Board boardCards) = board
            let indices = List.init (cards.Length + 1) id

            indices
            |> List.map (fun i -> (i, boardCards.Length - i))
            |> List.collect (fun (handCount, boardCount) ->
                let handSubsets = cards |> List.subsets handCount
                let boardSubsets = boardCards |> List.subsets boardCount

                boardSubsets
                |> List.collect (fun boardSubset ->
                    handSubsets
                    |> List.map (fun handSubset -> (boardSubset, handSubset))))
            |> List.map (fun (boardCards, handCards) -> HandCase.Create boardCards handCards))

    let handle (game: TexasGame) =
        evaluate game.Board game.Hands
        |> List.map (List.map (fst >> fst))
        |> formatHands

module Omaha =
    let parse input =
        let createGame (board, hands) =
            Omaha { Board = board; Hands = hands }

        let parseOmahaHands = parseHands (Hand.Create 4)

        input
        |> parseGameWithBoard parseOmahaHands createGame
        |> Result.mapError (sprintf "Abnormal Omaha Hold'em game: %A")

    let evaluate (board: Board) (hands: Hand list) =
        hands
        |> sortHands (fun (Hand cards) ->
            let (Board boardCards) = board
            let boardSubsets = boardCards |> List.subsets 3
            let handSubsets = cards |> List.subsets 2

            let subsets =
                boardSubsets
                |> List.collect (fun boardSubset ->
                    handSubsets
                    |> List.map (fun handSubset -> (boardSubset, handSubset)))

            subsets
            |> List.map (fun (boardCards, handCards) -> HandCase.Create boardCards handCards))

    let handle (game: OmahaGame) =
        evaluate game.Board game.Hands
        |> List.map (List.map (fst >> fst))
        |> formatHands

module FiveCard =
    let parse input =
        let createGame hands =
            FiveCard { Hands = hands }

        input
        |> parseHands (Hand.Create 5)
        |> Result.map createGame
        |> Result.mapError (sprintf "Abnormal Five Card Draw game: %A")

    let evaluate (hands: Hand list) =
        hands
        |> sortHands (fun (Hand hand) -> [ HandCase.Create [] hand ])

    let handle (game: FiveCardGame) =
        evaluate game.Hands
        |> List.map (List.map (fst >> fst))
        |> formatHands
