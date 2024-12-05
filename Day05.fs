namespace AOC2024

open System


module Day05 =
    let day = 5

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

    let nonZeroOrZero input =
        input
        |> List.sort
        |> (fun sorted -> if sorted[0] = 0 then sorted |> List.last else sorted[0])

    let comparedByRules rules onePage otherPage =
        rules
        |> Seq.map (fun (before, after) ->
            if onePage = before && otherPage = after then -1
            elif onePage = after && otherPage = before then 1
            else 0)
        |> List.ofSeq
        |> nonZeroOrZero

    let getMiddleItem input =
        input |> Seq.item ((input |> Seq.length) / 2)

    let isCorrect rules update =
        rules
        |> Seq.map (fun (before, after) ->
            if update |> Seq.contains before && update |> Seq.contains after then
                let flipped = update |> Seq.indexed |> Seq.map (fun (i, v) -> v, i) |> Map.ofSeq
                flipped[before] < flipped[after]
            else
                true)
        |> Seq.forall (fun ruleFollowed -> ruleFollowed)

    let getMiddlePageIfCorrect rules update =
        let isCorrect' = isCorrect rules

        if update |> isCorrect' then update |> getMiddleItem else 0


    let getMiddlePageIfIncorrect rules update =
        let isCorrect' = isCorrect rules
        let comparer = comparedByRules rules

        if update |> isCorrect' then
            0
        else
            update |> Seq.sortWith comparer |> getMiddleItem

    let solvePart1 rules updates =
        updates |> Seq.map (getMiddlePageIfCorrect rules) |> Seq.sum

    let solvePart2 rules updates =
        updates |> Seq.map (getMiddlePageIfIncorrect rules) |> Seq.sum

    let solve (puzzle: string) =
        let rules, updates = puzzle |> parse
        ((rules, updates) ||> solvePart1, (rules, updates) ||> solvePart2)

    let main =
        let part1, part2 = Library.getInputForDay day |> solve

        $"Solutions for day {day}:\nPart 1: {part1}\nPart 2: {part2}\n"
