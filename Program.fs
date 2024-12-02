open AOC2024

module Program =
    [<EntryPoint>]
    let main args =
        let dayRunners =
            Map[(1, (fun () -> Day01.main))
                (2, (fun () -> Day02.main))]

        match args with
        | [| day |] ->
            match Library.parseInt day with
            | Some day when dayRunners.ContainsKey day ->
                let output = dayRunners[day]()
                printfn "%s" output
                0
            | _ -> 
                printfn "%s is not a valid day" day
                1
        | _ ->
            printfn "Please supply a day number as an argument" 
            1
