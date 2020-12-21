open System
open System.IO
open Poker
open Poker.Parsing
open System.Text.RegularExpressions

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
        with
            | e -> 
                let error = 
                    let noNewLine = 
                        e.Message.Replace("\r\n", " ")
                            .Replace("\r", " ")
                            .Replace("\n", " ")
                    Regex.Replace(noNewLine, "\s+", " ")

                Error error

    match gameResult with
    | Ok output -> output
    | Error error -> sprintf "Error: %s" error

let rec processInput (input: TextReader) (output: TextWriter) (map: string -> string) =
    match input.ReadLine() |> Option.ofObj with
    | Some (line) ->
        output.WriteLine(map line)
        processInput input output map
    | None -> ()

[<EntryPoint>]
let main _ =
    processInput Console.In Console.Out processLine
    0





