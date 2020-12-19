open System
open System.IO

let rec main (input: TextReader) (output: TextWriter) (map: string -> string) =
    match input.ReadLine() |> Option.ofObj with
    | Some (line) ->
        output.WriteLine(map line)
        main input output map
    | None -> ()

main Console.In Console.Out (sprintf "output: %s")
