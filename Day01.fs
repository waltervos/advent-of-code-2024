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

    let main = Library.getInputForDay 1 |> parse ||> solve
