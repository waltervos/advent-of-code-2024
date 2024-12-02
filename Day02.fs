namespace AOC2024


module Day02 =
    open System

    let toVariants report =
        report |> List.mapi (fun i v -> report |> List.removeAt i)

    let isSafeDiff diff = diff >= 1 && diff <= 3

    let isSafeReport report =
        report
        |> List.pairwise |> List.map (fun (left, right) -> left - right)
        |> (fun diffList ->
            match diffList |> List.partition (fun diff -> diff >= 0) with
            | [], everything
            | everything, [] -> diffList |> List.map (fun diff -> abs diff) |> List.forall isSafeDiff
            | _ -> false)

    let solvePart1 reports =
        reports
        |> List.map isSafeReport
        |> List.filter (fun isSafe -> isSafe)
        |> List.length

    let solvePart2 reports =
        reports
        |> List.map toVariants
        |> List.map solvePart1
        |> List.filter (fun i -> i > 0)
        |> List.length

    let parse (s: string) =
        s.Split("\n", StringSplitOptions.TrimEntries)
        |> List.ofArray
        |> List.map (fun s -> s.Split(" ") |> List.ofArray)
        |> List.map (fun l -> l |> List.map (fun s -> s |> Int32.Parse))

    let main =
        let reports = Library.getInputForDay 2 |> parse
        let part1 = reports |> solvePart1
        let part2 = reports |> solvePart2

        $"Solutions for day 2:\nPart 1: {part1}\nPart 2: {part2}\n"
