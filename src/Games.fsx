#load "Parsing.fsx"#load "Evaluation.fsx"
namespace Poker

open Poker
open Poker.Parsing

module Texas =
    let parse input =
        let createGame (board, hands) = Texas { Board = board; Hands = hands }
        let parseTexasHands = parseHands (Hand.Create 2)

        input
        |> parseGameWithBoard parseTexasHands createGame
        |> Result.mapError (sprintf "Abnormal Texas Hold'em game definition: %A")

    let handle (game: TexasGame) = "Texas"

module Omaha =
    let parse input =
        let createGame (board, hands) = Omaha { Board = board; Hands = hands }
        let parseOmahaHands = parseHands (Hand.Create 4)

        input
        |> parseGameWithBoard parseOmahaHands createGame
        |> Result.mapError (sprintf "Abnormal Omaha Hold'em game definition: %A")

    let handle (game: OmahaGame) = "Omaha"

module FiveCard =
    let parse input =
        let createGame hands = FiveCard { Hands = hands }

        input
        |> parseHands (Hand.Create 5)
        |> Result.map createGame
        |> Result.mapError (sprintf "Abnormal Five Card Draw game definition: %A")

    let handle (game: FiveCardGame) = "Five Card"
