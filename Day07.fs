namespace AOC2024

type Result =
    | Match of (int64 -> int64 -> int64) list
    | NotAMatch

type Operator =
    | Add
    | Mul

module Day07 =
    open System

    let toFunction =
        function
        | Add -> (+)
        | Mul -> (*)

    let allCombinations length =
        [ for i in 0..length ->
              (Add |> List.replicate i) @ (Mul |> List.replicate (length - i))
              |> List.allSorts ]
        |> List.concat

    let calculate (numbers: int64 list) operators =
        match numbers with
        | first :: rest ->
            first
            |> (List.map2
                    (fun number operator -> operator number)
                    rest
                    operators
                |> List.reduce (>>))
        | _ -> 0

    let verify (testValue: int64) (numbers: int64 list) =
        let operationSets =
            allCombinations (numbers.Length - 1)
            |> List.map (List.map toFunction)

        operationSets
        |> List.map (fun operators ->
            if calculate numbers operators = testValue then
                Match(operators)
            else
                NotAMatch)
        |> List.filter (function
            | Match _ -> true
            | _ -> false)
        |> (fun results -> if results |> List.length > 0 then testValue else 0)

    let parse (puzzle: string) =
        puzzle.Split(
            "\n",
            StringSplitOptions.RemoveEmptyEntries
            ||| StringSplitOptions.TrimEntries
        )
        |> List.ofArray
        |> List.map (fun line ->
            match
                line.Split(
                    " ",
                    StringSplitOptions.RemoveEmptyEntries
                    ||| StringSplitOptions.TrimEntries
                )
                |> List.ofArray
            with
            | testValue :: numbers ->
                testValue.Trim(':') |> int64, numbers |> List.map int64
            | _ -> failwith $"Couldn't parse {line}")

    let solve puzzle =
        puzzle
        |> parse
        |> List.map (fun equation -> equation ||> verify)
        |> List.sum

    let main =
        (fun () ->
            let part1 = Library.getInputForDay 7 |> solve

            $"Solutions for day 7:\nPart 1: {part1}\n")
