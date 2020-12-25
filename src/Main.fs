module Poker.Main

open System
open System.IO
open Poker
open Poker.Parsing
open System.Text.RegularExpressions

let private handleGame game =
  match game with
  | Texas g -> Texas.handle g
  | Omaha g -> Omaha.handle g
  | FiveCard g -> FiveCard.handle g

let private chooseParser gameType =
  match gameType with
  | TexasGame -> Texas.parse
  | OmahaGame -> Omaha.parse
  | FiveCardGame -> FiveCard.parse

let private beautifyError (error: string) =
  let withoutLineBreaks =
    error
      .Replace("\r\n", " ")
      .Replace("\r", " ")
      .Replace("\n", " ")

  Regex.Replace(withoutLineBreaks, "\s+", " ")

let private processLine input =
  let gameResult =
    try
      input
      |> verifyInput
      |> Result.bind parseGameType
      |> Result.bind
           (fun (gameType, rest) ->
             let parser = chooseParser gameType
             parser rest)
      |> Result.map handleGame
    with e -> Error e.Message

  match gameResult with
  | Ok output -> output
  | Error error -> sprintf "Error: %s" (beautifyError error)

let processInput (input: TextReader) (output: TextWriter) =
  let rec inner (map: string -> string) =
    match input.ReadLine() |> Option.ofObj with
    | Some (line) ->
        output.WriteLine(map line)
        inner map
    | None -> ()

  inner processLine
