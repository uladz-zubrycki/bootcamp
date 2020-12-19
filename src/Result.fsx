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
        if errors.Length > 0 then Error errors else Ok(results |> List.map get)
