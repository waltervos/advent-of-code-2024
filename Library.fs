namespace AOC2024

module Library =
    open System
    let getInputForDay day =
        let path = $"./inputs/day-{day}.txt"
        path |> System.IO.File.ReadAllText

    let parseInt (s: string) =
        match System.Int32.TryParse s with
        | success, result when success -> Some result
        | _ -> None

    let toGrid (puzzle: string) =
        puzzle.Split("\n", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> List.ofArray
        |> List.map (fun s -> s.ToCharArray() |> List.ofArray)

module List =
    let takeBack n = List.rev >> List.take n >> List.rev

    let rotate45 taker filler matrix =
        let maxWidth = matrix |> List.length
        let maxHeight = matrix |> List.head |> List.length
        [ for i in 0 .. (maxWidth - 1) -> (matrix[i] |> taker (maxHeight - i) |> filler i) ]

    let change index value lst =
        lst |> List.removeAt index |> List.insertAt index value

    let swap index1 index2 lst =
        let elem1 = lst |> List.item index1
        let elem2 = lst |> List.item index2

        lst |> change index1 elem2 |> change index2 elem1

    let allSorts lst =
        let maxIndex = (lst |> List.length) - 1
        [ for i in 0..maxIndex do for j in 0..maxIndex -> lst |> swap i j ] |> List.distinct


module Grid =
    let windowed size grid =
        grid
        |> Seq.map (Seq.windowed size)
        |> Seq.windowed size
        |> Seq.map Seq.transpose
        |> Seq.concat
