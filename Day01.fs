namespace AOC2024


open System

module Day01 =
    let solve l r =
        let leftIndexed = l |> List.sort

        let rightIndexed = r |> List.sort

        let pt1 =
            [ leftIndexed; rightIndexed ]
            |> List.transpose
            |> List.map (fun l -> l |> List.reduce (fun i1 i2 -> abs (i1 - i2)))
            |> List.sum

        let pt2 =
            leftIndexed
            |> List.map (fun nl -> (rightIndexed |> List.filter (fun nr -> nr = nl) |> List.length) * nl) |> List.sum

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
        let left, right = Library.getInputForDay 1 |> parse

        let pt1, pt2 = (left, right) ||> solve

        pt1, pt2
