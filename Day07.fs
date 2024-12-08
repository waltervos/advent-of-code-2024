namespace AOC2024

type Result =
    | Match of string list
    | NotAMatch

module Day07 =
    open System

    let allCombinations length =
        [ for i in 0..length ->
              ("+" |> List.replicate i) @ ("*" |> List.replicate (length - i))
              |> List.allSorts ]
        |> List.concat

    let calculate (numbers: int64 list) operations =
        let rec operate numbers operations state =
            match numbers, operations with
            | (number :: nTail), (_ :: _) when
                numbers.Length > operations.Length
                ->
                operate nTail operations number
            | (number :: nTail), (operation :: oTail) ->
                let newState =
                    match operation with
                    | "+" -> state + number
                    | "*" -> state * number
                    | _ -> state

                newState |> operate nTail oTail
            | _ -> state

        operate numbers operations 0

    let verify (testValue: int64) (numbers: int64 list) =
        let operationSets = allCombinations ((numbers |> List.length) - 1)

        operationSets
        |> List.map (fun operations ->
            if calculate numbers operations = testValue then
                Match(operations)
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
