open System.IO

let gamesCount = 100000
let minHandCount = 3
let filePath = @"D:\dev\bootcamp\data\input.txt"

type GameRules =
    { Name: string
      BoardSize: int
      HandSize: int }

let games =
    [ { Name = "texas-holdem"
        BoardSize = 5
        HandSize = 2 }
      { Name = "omaha-holdem"
        BoardSize = 5
        HandSize = 4 }
      { Name = "five-card-draw"
        BoardSize = 0
        HandSize = 5 } ]

let suits = [ "s"; "c"; "d"; "h" ]

let ranks =
    [ "A"
      "K"
      "Q"
      "J"
      "T"
      "9"
      "8"
      "7"
      "6"
      "5"
      "3"
      "2" ]

let cards =
    suits
    |> List.collect (fun suit ->
        ranks
        |> List.map (fun rank -> sprintf "%s%s" rank suit))

open System

let rand = Random()

let randomIndices size =
    let rec inner (taken: int Set) (result: int List) =
        if result.Length = size then
            result
        else
            let index = rand.Next(size)
            if not (taken.Contains index)
            then inner (taken.Add index) (index :: result)
            else inner taken result

    inner Set.empty []

let definitions =
    seq {
        for game in games do
            for i in 1 .. gamesCount do
                let indices = randomIndices cards.Length

                let cards =
                    cards |> List.permute (fun i -> indices.[i])

                let board =
                    cards
                    |> List.take game.BoardSize
                    |> String.concat ""

                let handChunks =
                    cards
                    |> List.skip game.BoardSize
                    |> List.chunkBySize game.HandSize
                    |> List.filter (fun h -> h.Length = game.HandSize)

                let handsCount =
                    rand.Next(minHandCount, handChunks.Length)

                let hands =
                    handChunks
                    |> List.take handsCount
                    |> List.map (String.concat "")
                    |> String.concat " "

                let definition =
                    [ game.Name; board; hands ]
                    |> List.filter (String.IsNullOrEmpty >> not)
                    |> String.concat " "

                yield definition
    }
    |> Seq.toList


if File.Exists(filePath)
then File.Delete(filePath)

File.WriteAllLines(filePath, definitions)
