namespace Poker

module Result =
    let tryError result =
        match result with
        | Ok _ -> None
        | Error e -> Some e

    let get result =
        match result with
        | Ok x -> x
        | Error e -> failwithf "Can't get result out of Error (%A)" e

    let ofAllOk results =
        let errors = results |> List.choose tryError
        if errors.Length > 0
        then Error errors
        else Ok(results |> List.map get)

module List =
    let maxWith (comparer: 'a -> 'a -> int) (list: 'a list) =
        match list |> List.tryHead with
        | None -> invalidArg "list" "At least one alement is required"
        | Some head ->
            list
            |> List.fold (fun max item ->
                if comparer item max > 0
                then item
                else max) head

    let minWith (comparer: 'a -> 'a -> int) (list: 'a list) =
        match list |> List.tryHead with
        | None -> invalidArg "list" "At least one alement is required"
        | Some head ->
            list
            |> List.fold (fun min item ->
                if comparer item min < 0
                then item
                else min) head

    let subsets (size: int) (input: 'a list) =
        let rec inner items =
            match items with
            | [] -> [ [] ]
            | head :: xs ->
                inner xs
                |> List.fold (fun sets set ->
                    if set.Length < size
                    then (head :: set) :: set :: sets
                    else set :: sets) []

        inner input
        |> List.choose (fun subset ->
            if List.length subset = size
            then Some subset
            else None)
