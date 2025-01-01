namespace AOC2024

module Tuple =
    let either predicate (first, second) =
        first |> predicate || second |> predicate

type Graph<'t when 't: comparison> = { Nodes: Set<'t>; Edges: Set<'t * 't> }

module Graph =
    let empty = { Nodes = Set.empty; Edges = Set.empty }

    let addNodes nodes graph =
        { graph with
            Nodes = graph.Nodes |> Set.union (set nodes) }

    let addNode node graph = graph |> addNodes (set [ node ])

    let addEdges edges graph =
        { graph with
            Edges = graph.Edges |> Set.union edges }

    let addEdge node1 node2 graph =
        graph |> addEdges (set [ (node1, node2) ])

    let nodes graph = graph.Nodes

    let unionMany graphs =
        { Nodes = graphs |> Seq.map _.Nodes |> Set.unionMany
          Edges = graphs |> Seq.map _.Edges |> Set.unionMany }

    let union graph1 graph2 = unionMany (seq [ graph1; graph2 ])


    let getFrom node graph =
        let edges = graph.Edges |> Set.filter (Tuple.either ((=) node))

        let nodes =
            graph.Nodes
            |> Set.filter (fun n -> edges |> Set.exists (Tuple.either ((=) n)))
            |> Set.add node

        { Nodes = nodes; Edges = edges }

    let explodeFrom node graph = seq [ graph |> getFrom node ]

    let filterNodes predicate graph =
        let nodes = graph |> nodes |> Set.filter (predicate graph.Edges)

        { graph with Nodes = nodes }

    let isConnected node1 node2 graph =
        graph.Edges
        |> Set.exists (function
            | (n1, n2) when n1 = node1 && n2 = node2 || n1 = node2 && n2 = node1 ->
                true
            | _ -> false)





module String =
    open System

    let startsWith (string: string) (subject: string) =
        subject.StartsWith(string)

    let split (on: char) (string: string) =
        string.Split(
            on,
            StringSplitOptions.RemoveEmptyEntries
            ||| StringSplitOptions.TrimEntries
        )

    let splitOnString (on: string) (string: string) =
        string.Split(
            on,
            StringSplitOptions.TrimEntries
            ||| StringSplitOptions.RemoveEmptyEntries
        )

    let allIndexesOf (substring: string) (string: string) =
        seq {
            for i in 0 .. string.Length - 1 do
                let index = string.IndexOf(substring, i)

                if index > -1 then
                    yield seq { index .. index + substring.Length - 1 }
        }
        |> Seq.distinct

    let remove startAt count (string: string) = string.Remove(startAt, count)

    let replace (subject: string) (newChar: char) (oldString: string) =
        let replacer = new String(newChar, oldString.Length)
        subject.Replace(oldString, replacer)

module Library =
    open System

    let getInputForDay day =
        let path = $"./inputs/day-{day}.txt"
        path |> System.IO.File.ReadAllText

    let parseInt (s: string) =
        match System.Int32.TryParse s with
        | success, result when success -> Some result
        | _ -> None

    let toGrid (puzzle: string) =
        puzzle
        |> String.split '\n'
        |> List.ofArray
        |> List.map (fun s -> s.ToCharArray() |> List.ofArray)

    let toGrid' (puzzle: string) =
        puzzle
        |> String.split '\n'
        |> Array.map (Seq.map char >> Array.ofSeq)
        |> (fun parsed ->
            let width, height =
                parsed[0] |> Array.length, parsed |> Array.length

            Array2D.init height width (fun y x -> parsed[y][x]))

    let say message =
        printf "%O - " DateTime.Now
        printfn message

module List =
    let change index value lst =
        lst |> List.removeAt index |> List.insertAt index value

    let swap index1 index2 lst =
        let elem1 = lst |> List.item index1
        let elem2 = lst |> List.item index2

        lst |> change index1 elem2 |> change index2 elem1

    let allSorts lst =
        let maxIndex = (lst |> List.length) - 1

        [ for i in 0..maxIndex do
              for j in 0..maxIndex -> lst |> swap i j ]
        |> List.distinct


module Seq =
    let change index value lst =
        lst |> Seq.removeAt index |> Seq.insertAt index value

    let swap index1 index2 lst =
        let elem1 = lst |> Seq.item index1
        let elem2 = lst |> Seq.item index2

        lst |> change index1 elem2 |> change index2 elem1

    let partition predicate seq =
        seq
        |> Seq.fold
            (fun (trueValues, falseValues) el ->
                if (el |> predicate) then
                    (trueValues |> Seq.append [ el ], falseValues)
                else
                    (falseValues, trueValues |> Seq.append [ el ]))
            (Seq.empty, Seq.empty)

module Grid =
    let windowed size grid =
        grid
        |> Seq.map (Seq.windowed size)
        |> Seq.windowed size
        |> Seq.map Seq.transpose
        |> Seq.concat

    let inBounds grid (x, y) =
        x >= 0
        && x < (grid |> Array2D.length2)
        && y >= 0
        && y < (grid |> Array2D.length1)

    let set x y (value: 'a) (grid: 'a array2d) =
        grid[y, x] <- value
        grid
