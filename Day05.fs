namespace AOC2024

open System

module Day05 =
    let getMiddleItem input =
        input |> Seq.item ((input |> Seq.length) / 2)

    let getMiddlePageIfCorrect rules update =
        let result =
            rules
            |> Seq.map (fun (before, after) ->
                if update |> Seq.contains before && update |> Seq.contains after then
                    let flipped = update |> Seq.indexed |> Seq.map (fun (i, v) -> v, i) |> Map.ofSeq
                    flipped[before] < flipped[after]
                else
                    true)
            |> Seq.forall (fun ruleFollowed -> ruleFollowed)

        if result then update |> getMiddleItem else 0


    let parse (puzzle: string) =
        puzzle.Split(
            $"{Environment.NewLine}{Environment.NewLine}",
            StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries
        )
        |> Seq.map (fun s ->
            s.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries))
        |> List.ofSeq
        |> (fun lists ->
            match lists with
            | [ rules; updates ] ->
                let parsedRules =
                    rules
                    |> Seq.map (fun s ->
                        match s.Split('|') |> Array.map int with
                        | [| before; after |] -> (before, after)
                        | _ -> failwith $"{s} is not a valid rule")

                parsedRules, updates |> Seq.map (fun s -> s.Split(',') |> Seq.map int)
            | _ -> failwith "Couldn't parse")

    let solve (puzzle: string) =
        puzzle
        |> parse
        ||> (fun rules updates -> 
            updates
            |> Seq.map (getMiddlePageIfCorrect rules)
            |> Seq.sum
        )

    let main =
        let part1 = Library.getInputForDay 5 |> solve

        $"Solutions for day 5:\nPart 1: {part1}\n"
