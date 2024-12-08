open AOC2024

module Program =
    [<EntryPoint>]
    let main args =
        let dayRunners =
            Map[(1, Day01.main)
                (2, Day02.main)
                (3, Day03.main)
                (4, Day04.main)
                (5, Day05.main)
                (6, Day06.main)
                (7, Day07.main)]

        match args with
        | [| day |] ->
            match Library.parseInt day with
            | Some day when dayRunners.ContainsKey day ->
                printfn "Solving day %i..." day
                let output = dayRunners[day]()
                printfn "%s" output
                0
            | _ ->
                printfn "%s is not a valid day, or isn't solved yet." day
                1
        | [||] ->
            dayRunners
            |> Map.iter (fun day runner ->
                printfn "Solving day %i..." day
                runner () |> printfn "%s")

            0
        | _ ->
            printfn
                "Please supply only a day number as an argument, or supply no argument to run all the solutions."

            1
