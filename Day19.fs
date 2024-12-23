namespace AOC2024

module Day19 =
    let parse puzzle =
        match
            puzzle
            |> String.splitOnString "\r\n\r\n"
            |> Array.partition (fun s -> s.Contains(","))
        with
        | (towels, designs) ->
            (towels |> Array.item 0 |> String.split ','),
            (designs |> Array.item 0 |> String.split '\n')

    let rec isPossible towels design =
        if design |> System.String.IsNullOrWhiteSpace then
            true
        else
            let newDesigns =
                towels
                |> Seq.map (String.replace design " ")
                |> Seq.filter (fun newDesign -> newDesign <> design)

            if newDesigns |> Seq.length > 0 then
                newDesigns |> Seq.map (isPossible towels) |> Seq.contains true
            else
                false

    let solve puzzle =
        let towels, designs = parse puzzle

        designs
        |> Seq.mapi (fun i design ->
            printfn "Working on design %i: %s" (i + 1) design
            let canBeMade = design |> isPossible towels

            if canBeMade then
                do printfn "Design %i can be made" (i + 1)

            canBeMade)

        |> Seq.filter (id)
        |> Seq.length

    let main =
        fun () ->
            let day = 19
            let part1 = Library.getInputForDay day |> solve

            $"Solutions for day {day}:\nPart 1: {part1}\n"
