namespace AOC2024


open System

module Day01 =
    let solve left right =
        let pt1 =
            [ left; right ]
            |> Seq.map (fun l -> l |> Seq.sort)
            |> Seq.transpose
            |> Seq.map (fun l -> l |> Seq.reduce (fun nLeft nRight -> abs (nLeft - nRight)))
            |> Seq.sum

        let pt2 =
            left
            |> Seq.map (fun nLeft -> (right |> Seq.filter (fun nRight -> nRight = nLeft) |> Seq.length) * nLeft)
            |> Seq.sum

        pt1, pt2

    let parse (s: string) =
        let intListList =
            s.Split("\n", StringSplitOptions.TrimEntries)
            |> Seq.ofArray
            |> Seq.map (fun s -> s.Split("   ") |> Seq.ofArray)
            |> Seq.map (fun l -> l |> Seq.map (fun s -> s |> Int32.Parse))
            |> Seq.transpose
            |> List.ofSeq

        intListList[0], intListList[1]

    let main =
        let part1, part2 = Library.getInputForDay 1 |> parse ||> solve

        $"Solutions for day 1:\nPart 1: {part1}\nPart 2: {part2}\n"
