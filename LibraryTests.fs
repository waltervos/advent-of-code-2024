module LibraryTests

open NUnit.Framework
open FsUnit
open AOC2024

[<Test>]
let ``Searching for nodes from an unconnected node yields only the search node ``
    ()
    =
    let graph =
        { Nodes = set [ 1; 2; 3 ]
          Edges = Set.empty }

    graph
    |> Graph.getFrom 1
    |> should equal { Nodes = set [ 1 ]; Edges = Set.empty }

[<Test>]
let ``Searching for nodes from a connected node leaves out edges to unconnected nodes``
    ()
    =
    let graph =
        { Nodes = set [ 1; 2; 3 ]
          Edges = set [ (1, 2); (2, 3) ] }

    graph
    |> Graph.getFrom 1
    |> should
        equal
        { Nodes = set [ 1; 2 ]
          Edges = set [ (1, 2) ] }

[<Test>]
let ``Exploding a graph from an unconnected node, yields one graph`` () =
    let graph =
        { Nodes = set [ 1; 2; 3 ]
          Edges = Set.empty }

    graph
    |> Graph.explodeFrom 1
    |> should equal [ { Nodes = set [ 1 ]; Edges = Set.empty } ]


[<Test>]
let ``Exploding a graph from a once connected node, yields one graph`` () =
    let graph =
        { Nodes = set [ 1; 2; 3 ]
          Edges = set [ (1, 2); (2, 3) ] }

    graph
    |> Graph.explodeFrom 1
    |> should
        equal
        [ { Nodes = set [ 1; 2 ]
            Edges = set [ (1, 2) ] } ]

// [<Test>]
// let ``Exploding a graph from a twice connected node, yields two graphs`` () =
//     let graph =
//         { Nodes = set [ 1; 2; 3 ]
//           Edges = set [ (1, 2); (1, 3) ] }

//     graph
//     |> Graph.explodeFrom 1
//     |> should
//         equal
//         [ { Nodes = set [ 1; 2 ]
//             Edges = set [ (1, 2) ] }
//           { Nodes = set [ 1; 3 ]
//             Edges = set [ (1, 3) ] } ]
