namespace AOC2024

module String =
    open System

    let split (on: char) (string: string) =
        string.Split(
            on,
            StringSplitOptions.RemoveEmptyEntries
            ||| StringSplitOptions.TrimEntries
        )

    let splitOnString (on: string) (string: string) =
        string.Split(
            on,
            StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
        )

    let remove startAt count (string: string) = string.Remove(startAt, count)

    let replace (subject: string) (newString: string) (oldString: string) = 
        subject.Replace(oldString, newString)

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
        puzzle
        |> String.split '\n'
        |> List.ofArray
        |> List.map (fun s -> s.ToCharArray() |> List.ofArray)

    let toGrid' (puzzle: string) =
        puzzle
        |> String.split '\n'
        |> Array.map (Seq.map char >> Array.ofSeq)
        |> (fun parsed ->
            let width, height =
                parsed[0] |> Array.length, parsed |> Array.length

            Array2D.init height width (fun y x -> parsed[y][x]))

module List =
    let change index value lst =
        lst |> List.removeAt index |> List.insertAt index value

    let swap index1 index2 lst =
        let elem1 = lst |> List.item index1
        let elem2 = lst |> List.item index2

        lst |> change index1 elem2 |> change index2 elem1

    let allSorts lst =
        let maxIndex = (lst |> List.length) - 1

        [ for i in 0..maxIndex do
              for j in 0..maxIndex -> lst |> swap i j ]
        |> List.distinct


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

    let inBounds grid (x, y) =
        x >= 0
        && x < (grid |> Array2D.length2)
        && y >= 0
        && y < (grid |> Array2D.length1)

    let set x y (value: 'a) (grid: 'a array2d) =
        grid[y, x] <- value
        grid
