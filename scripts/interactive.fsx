#load @"..\src\Utils.fs"
#load @"..\src\Domain.fs"
#load @"..\src\Formatting.fs"
#load @"..\src\Evaluation.fs"
#load @"..\src\Games.fs"
#load @"..\src\Main.fs"

open System.IO
open Poker
open Poker.Main

Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)
let input = @"..\data\input.txt"
let output = @"..\out\result.txt"

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

if File.Exists output
then File.Delete output

let reader = new StreamReader(input)
let writer = new StreamWriter(output)
processInput reader writer
writer.Close()
reader.Close()
