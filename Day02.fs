namespace AOC2024


module Day02 =
    open System

    let toVariants report =
        report |> List.mapi (fun i v -> report |> List.removeAt i)

    let asDiffList report =
        report |> List.pairwise |> List.map (fun (left, right) -> left - right)

    let isSafeDiff diff = diff >= 1 && diff <= 3

    let isSafeReport report =
        report
        |> asDiffList
        |> (fun diffList ->

            if
                (diffList |> List.forall (fun diff -> diff > 0)
                 || diffList |> List.forall (fun diff -> diff < 0))
            then
                let absDiffList = diffList |> List.map (fun diff -> abs diff)

                absDiffList |> List.forall isSafeDiff

            else
                false)

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
        (reports |> solvePart1, reports |> solvePart2)
