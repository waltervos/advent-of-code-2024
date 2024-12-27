namespace AOC2024

module Day25 =
    type Schematic =
        | Lock of int seq
        | Key of int seq

    let fits pair =
        match pair with
        | Lock l, Key k ->
            (k, l)
            ||> Seq.forall2 (fun keyColumn lockColumn ->
                keyColumn + lockColumn <= 5)
        | _ -> false

    let parseSchematic x =
        x
        |> Seq.map (List.ofSeq)
        |> Seq.transpose
        |> Seq.map (Seq.sumBy (fun c -> if c = '#' then 1 else 0))

    let parse puzzle =
        puzzle
        |> (String.splitOnString "\r\n\r\n" >> Seq.map (String.split '\n'))
        |> Seq.map (fun lockOrKey ->
            match lockOrKey |> List.ofArray with
            | head :: tail when head = "#####" -> Lock(tail |> parseSchematic)
            | head :: tail when tail |> List.last = "#####" ->
                Key(head :: (tail |> List.take 5) |> parseSchematic)
            | _ -> failwith "Wrong!")

    let countFits (schematics: Schematic seq) =
        schematics
        |> Seq.allPairs schematics
        |> Seq.map fits
        |> Seq.sumBy (function
            | true -> 1
            | false -> 0)

    let solve puzzle = puzzle |> parse |> countFits

    let main =
        fun () ->
            let day = 25
            let part1 = Library.getInputForDay day |> solve

            $"Solutions for day {day}:\nPart 1: {part1}\n"
