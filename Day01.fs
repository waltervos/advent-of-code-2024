namespace AOC2024


open System

module Day01 =
    let solve left right =
        let pt1 =
            [ left; right ]
            |> List.map (fun l -> l |> List.sort)
            |> List.transpose
            |> List.map (fun l -> l |> List.reduce (fun nLeft nRight -> abs (nLeft - nRight)))
            |> List.sum

        let pt2 =
            left
            |> List.map (fun nLeft -> (right |> List.filter (fun nRight -> nRight = nLeft) |> List.length) * nLeft)
            |> List.sum

        pt1, pt2

    let parse (s: string) =
        let intListList =
            s.Split("\n", StringSplitOptions.TrimEntries)
            |> List.ofArray
            |> List.map (fun s -> s.Split("   ") |> List.ofArray)
            |> List.map (fun l -> l |> List.map (fun s -> s |> Int32.Parse))
            |> List.transpose

        intListList[0], intListList[1]

    let main =
        let part1, part2 = Library.getInputForDay 1 |> parse ||> solve

        $"Solutions for day 1:\nPart 1: {part1}\nPart 2: {part2}\n"
