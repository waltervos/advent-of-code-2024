namespace AOC2024


module Day02 =
    open System

    let toVariants report =
        report |> Seq.mapi (fun i v -> report |> Seq.removeAt i)

    let isSafeDiff diff = diff >= 1 && diff <= 3

    let isSafeReport report =
        report
        |> Seq.pairwise
        |> Seq.map (fun (left, right) -> left - right)
        |> (fun diffList ->
            match diffList |> List.ofSeq |> List.partition (fun diff -> diff >= 0) with
            | [], everything
            | everything, [] -> diffList |> Seq.map (fun diff -> abs diff) |> Seq.forall isSafeDiff
            | _ -> false)

    let solvePart1 reports =
        reports
        |> Seq.map isSafeReport
        |> Seq.filter (fun isSafe -> isSafe)
        |> Seq.length

    let solvePart2 reports =
        reports
        |> Seq.map toVariants
        |> Seq.map solvePart1
        |> Seq.filter (fun i -> i > 0)
        |> Seq.length

    let parse (s: string) =
        s.Split("\n", StringSplitOptions.TrimEntries)
        |> Seq.map (fun s -> s.Split(" "))
        |> Seq.map (fun l -> l |> Seq.map (fun s -> s |> Int32.Parse))

    let main =
        let reports = Library.getInputForDay 2 |> parse
        let part1 = reports |> solvePart1
        let part2 = reports |> solvePart2

        $"Solutions for day 2:\nPart 1: {part1}\nPart 2: {part2}\n"
