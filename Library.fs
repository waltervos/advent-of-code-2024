namespace AOC2024

module Library =
    let getInputForDay day =
        let path = $"./inputs/day-{day}.txt"
        path |> System.IO.File.ReadAllText

    let parseInt (s: string) =
        match System.Int32.TryParse s with
        | success, result when success -> Some result
        | _ -> None

module List =
    let takeBack n = List.rev >> List.take n >> List.rev

    let rotate45 taker filler matrix =
        let maxWidth = matrix |> List.length
        let maxHeight = matrix |> List.head |> List.length
        [ for i in 0 .. (maxWidth - 1) -> (matrix[i] |> taker (maxHeight - i) |> filler i) ]

module Grid =
    let windowed grid count = grid
