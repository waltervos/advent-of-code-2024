open AOC2024

module Program =
    [<EntryPoint>]
    let main args =
        let dayRunners =
            Map[(1, (fun () -> Day01.main))
                (2, (fun () -> Day02.main))
                (3, (fun () -> Day03.main))]

        match args with
        | [| day |] ->
            match Library.parseInt day with
            | Some day when dayRunners.ContainsKey day ->
                let output = dayRunners[day]()
                printfn "%s" output
                0
            | _ ->
                printfn "%s is not a valid day, or isn't solved yet." day
                1
        | [||] ->
            dayRunners |> Map.iter (fun day runner -> runner () |> printfn "%s")
            0
        | _ ->
            printfn "Please supply only a day number as an argument, or supply no argument to run all the solutions."
            1
