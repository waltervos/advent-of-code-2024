namespace AOC2024

module Day04 =
    open System

    let toZero fromN = [ fromN .. -1 .. 0 ]
    let fromZero toN = [ 0..toN ]

    let explode grid =
        let maxX = (grid |> List.head |> List.length) - 1
        let maxY = (grid |> List.length) - 1

        seq {
            seq { for y in maxY |> fromZero -> seq { for x in maxX |> fromZero -> (x, y) } } // Vertical, travelling left to right
            seq { for x in maxX |> fromZero -> seq { for y in maxY |> fromZero -> (x, y) } } // Horizontal, travelling top to bottom

            seq { // Top left to bottom right, travelling top to bottom
                for x in maxX |> fromZero ->
                    seq { for y in maxY |> fromZero -> (x + y, y) }
                    |> Seq.filter (fun (x, y) -> x <= maxX)
            }

            seq { // Top left to bottom right, travelling left to right
                for y in maxY |> fromZero ->
                    seq { for x in maxX |> fromZero -> (x, y + x) }
                    |> Seq.filter (fun (x, y) -> y <= maxY)
            }

            seq { // Bottom right to top left, travelling right to left < < (0, 2), (1, 1), (2, 0) >, < (0, 1), (1, 0) > >
                for x in maxX |> toZero ->
                    seq { for y in maxY |> toZero -> (x - y, y) }
                    |> Seq.filter (fun (x, y) -> x >= 0)
            }

            seq { // Bottom right to top left, top to bottom < (0, 2), (1, 1), (2, 0) >, < (1, 2), (2, 1) >
                for (x, y) in (maxX |> fromZero, maxY |> toZero) ||> List.zip ->
                    seq { for i in maxX |> fromZero -> (x + i, y) }
                    |> Seq.filter (fun (x, y) -> x <= maxX)
            }
            |> Seq.transpose
        }
        |> Seq.map (Seq.filter (fun s -> s |> Seq.length > 1))
        |> Seq.concat
        |> Seq.distinctBy (fun s -> s |> List.ofSeq)
        |> Seq.map (Seq.map (fun (x, y) -> grid[y][x]))
        |> Seq.map (fun row -> new String(row |> Array.ofSeq))

    let countChristmas row =
        let christmasList = [ 'X'; 'M'; 'A'; 'S' ] |> List.toArray

        row
        |> Seq.windowed 4
        |> Seq.filter (fun window -> window = christmasList || window = (christmasList |> Array.rev))
        |> Seq.length

    let solve (puzzle: string) =
        puzzle.Split("\n", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> List.ofArray
        |> List.map (fun s -> s.ToCharArray() |> List.ofArray)
        |> explode
        |> Seq.map countChristmas
        |> Seq.sum

    let main =
        let part1 = Library.getInputForDay 4 |> solve

        $"Solutions for day 4:\nPart 1: {part1}\n"
