#load "Utils.fs"
#load "Domain.fs"
#load "Formatting.fs"
#load "Evaluation.fs"
#load "Games.fs"

open Poker
open Poker.Parsing
open Poker.Evaluation
open System
open System.Text.RegularExpressions

let printSuit suit =
    match suit with
    | Spades -> "♠"
    | Hearts -> "♥"
    | Diamonds -> "♦"
    | Clubs -> "♣"

let printRank rank =
    match rank with
    | Ace -> "A"
    | King -> "K"
    | Queen -> "Q"
    | Jack -> "J"
    | Ten -> "10"
    | Nine -> "9"
    | Eight -> "8"
    | Seven -> "7"
    | Six -> "6"
    | Five -> "5"
    | Four -> "4"
    | Three -> "3"
    | Two -> "2"

fsi.AddPrinter<CardSuit> printSuit
fsi.AddPrinter<CardRank> printRank
fsi.AddPrinter<Card>(fun c -> sprintf "%s%s" (printSuit c.Suit) (printRank c.Rank))

let handleGame game =
    match game with
    | Texas g -> Texas.handle g
    | Omaha g -> Omaha.handle g
    | FiveCard g -> FiveCard.handle g

let chooseParser gameType =
    match gameType with
    | TexasGame -> Texas.parse
    | OmahaGame -> Omaha.parse
    | FiveCardGame -> FiveCard.parse

let processLine input =
    let gameResult =
        try
            input
            |> verifyInput
            |> Result.bind parseGameType
            |> Result.bind (fun (gameType, rest) ->
                let parser = chooseParser gameType
                parser rest)
            |> Result.map handleGame
        with e ->
            let error =
                let noNewLine =
                    e.Message.Replace("\r\n", " ").Replace("\r", " ").Replace("\n", " ")

                Regex.Replace(noNewLine, "\s+", " ")

            Error error

    match gameResult with
    | Ok output -> output
    | Error error -> sprintf "Error: %s" error
