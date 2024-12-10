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
    let change index value lst =
        lst |> List.removeAt index |> List.insertAt index value

    let swap index1 index2 lst =
        let elem1 = lst |> List.item index1
        let elem2 = lst |> List.item index2

        lst |> change index1 elem2 |> change index2 elem1

    let allSorts lst =
        let maxIndex = (lst |> List.length) - 1
        [ for i in 0..maxIndex do for j in 0..maxIndex -> lst |> swap i j ] |> List.distinct


module Seq =
    let change index value lst =
        lst |> Seq.removeAt index |> Seq.insertAt index value
    let swap index1 index2 lst =
        let elem1 = lst |> Seq.item index1
        let elem2 = lst |> Seq.item index2

        lst |> change index1 elem2 |> change index2 elem1

module Grid =
    let windowed size grid =
        grid
        |> Seq.map (Seq.windowed size)
        |> Seq.windowed size
        |> Seq.map Seq.transpose
        |> Seq.concat
