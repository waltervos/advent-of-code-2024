module Day10Tests

open AOC2024
open AOC2024.Day10

open NUnit.Framework
open FsUnit

[<Test>]
[<Ignore("Later")>]
let ``Something`` () =
    let example =
        "0123
1234
8765
9876"

    example
    |> String.split '\n'
    |> Seq.map (Seq.map (string >> int))
    |> List.ofSeq
    |> List.map List.ofSeq
    |> (fun s ->
        let height = s |> List.length
        let width = s |> List.head |> List.length
        Array2D.init height width (fun y x -> s[y][x]))
    |> (fun map ->
        map
        |> Array2D.mapi (fun y x n ->
            if n = 0 then findTrailsFrom (x, y) map else []))
    |> should equal 1


// [<Test>]
// let ``It finds a single next waypoint`` () =
//     Array2D.create 2 1 1
//     |> Grid.set 0 0 0
//     |> findTrailsFrom (0, 0)
//     |> should equal [ [ (0, 0); (0, 1) ] ]

// [<Test>]
// let ``It finds the two next waypoints`` () =
//     Array2D.create 2 2 1
//     |> Grid.set 0 0 0
//     |> findTrailsFrom (0, 0)
//     |> should equal [ [ (0, 0); (0, 1) ]; [ (0, 0); (1, 0) ] ]

// [<Test>]
// let ``It finds the three next waypoints`` () =
//     Array2D.create 3 1 1
//     |> Grid.set 0 0 0
//     |> Grid.set 0 2 2
//     |> findTrailsFrom (0, 0)
//     |> should equal [ [ (0, 0); (0, 1); (0, 2) ] ]


[<Test>]
[<Ignore("Maybe some other time")>]
let ``What?`` () =
    let map = Array2D.create 4 4 0

    [ [ 0; 1; 2; 3 ]; [ 1; 2; 3; 4 ]; [ 8; 7; 6; 5 ]; [ 9; 8; 7; 6 ] ]
    |> List.iteri (fun y r -> r |> List.iteri (fun x i -> map[y, x] <- i))

    printfn "%A" map

    map
    |> findTrailsFrom (0, 0)
    |> should equal []
