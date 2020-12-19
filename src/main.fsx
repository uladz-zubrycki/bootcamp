#load "Games.fsx"

open System
open System.IO
open Poker
open Poker.Parsing

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

let verifyLine (input: string) =
    if input.Trim().Length = 0 then Error "Empty input" else Ok input

let processLine input =
    let gameResult =
        input
        |> verifyLine
        |> Result.bind parseGameType
        |> Result.bind (fun (gameType, rest) ->
            let parser = chooseParser gameType
            parser rest)
        |> Result.map handleGame

    match gameResult with
    | Ok output -> output
    | Error error -> sprintf "Error: %s" error

let rec main (input: TextReader) (output: TextWriter) (map: string -> string) =
    match input.ReadLine() |> Option.ofObj with
    | Some (line) ->
        output.WriteLine(map line)
        main input output map
    | None -> ()

main Console.In Console.Out processLine
