namespace AOC2024

module Day23 =
    let startingWithT = (Set.exists (String.startsWith "t"))

    let findConnections predicate connections =
        connections
        |> Seq.filter predicate
        |> Seq.map (fun connectionWithT ->
            connections
            |> Seq.map (fun connection ->
                let difference = Set.difference connection connectionWithT

                if difference |> Set.count = 1 then
                    connectionWithT |> Set.union difference
                else
                    Set.empty)
            |> Seq.countBy id)
        |> Seq.concat
        |> Seq.choose (function
            | c, 2 -> Some c
            | _ -> None)
        |> Seq.filter (Set.count >> (=) 3)
        |> Seq.distinct
        |> Seq.map (fun s -> System.String.Join(',', s))
        |> Seq.sort

    let areConnected connections (node1, node2) =
        Seq.exists ((=) (set [ node1; node2 ])) connections


    let findConnections' (connections: Set<string> seq) =
        connections
        |> Seq.reduce Set.union
        |> Seq.map (fun node ->
            (node,
             connections
             |> Seq.filter (Set.contains node)
             |> Set.unionMany
             |> Set.remove node))
        |> Seq.map (fun (node, connectedNodes) ->
            connectedNodes
            |> Seq.allPairs connectedNodes
            |> Seq.filter (areConnected connections)
            |> Seq.countBy (fst >> id)
            |> Seq.filter (snd >> (=) 2) // Maybe not 2, but something dynamic?
            |> Seq.map (fst)
            |> Seq.append [ node ]
            |> Seq.sort)
        |> Seq.map (fun s -> System.String.Join(',', s))
        |> Seq.distinct
        |> Seq.sortByDescending _.Length
        |> Seq.head


    let parse puzzle =
        puzzle
        |> String.split '\n'
        |> Seq.map (fun connection ->
            match connection |> String.split '-' with
            | [| left; right |] -> set [ left; right ]
            | _ -> failwith "Wrong!")

    let solve puzzle =
        let connections = puzzle |> parse
        let part1 = connections |> findConnections startingWithT |> Seq.length
        let part2 = connections |> findConnections'

        part1, part2

    let main =
        (fun () ->
            let day = 23
            let part1, part2 = Library.getInputForDay day |> solve

            $"Solutions for day {day}:\nPart 1: {part1}\nPart 2: {part2 |> Seq.toList}\n")
