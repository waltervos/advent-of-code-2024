module Day06Tests

open FsUnit
open NUnit.Framework

open AOC2024
open AOC2024.Day06

let example =
    "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

let guard = { Position = 4, 6; Facing = North }

[<Test>]
let ``Example from adventofcode.com`` () =
    let result = example |> solve guard

    result |> fst |> should equal 41

[<Test>]
let ``It takes a step forward without obstacles, when southbound`` () =
    let map = [ [ Available ]; [ Available ] ]
    let guard = { Position = 0, 0; Facing = South }

    (map, guard) ||> next |> snd |> should equal { guard with Position = 0, 1 }

[<Test>]
let ``It takes a step forward without obstacles, when eastbound`` () =
    let map = [ [ Available; Available ] ]
    let guard = { Position = 0, 0; Facing = East }

    (map, guard) ||> next |> snd |> should equal { guard with Position = 1, 0 }

[<Test>]
let ``It takes a step forward without obstacles, when northbound`` () =
    let map = [ [ Available ]; [ Available ] ]
    let guard = { Position = 0, 1; Facing = North }

    (map, guard) ||> next |> snd |> should equal { guard with Position = 0, 0 }

[<Test>]
let ``It takes a step forward without obstacles, when westbound`` () =
    let map = [ [ Available; Available ] ]
    let guard = { Position = 1, 0; Facing = West }

    (map, guard) ||> next |> snd |> should equal { guard with Position = 0, 0 }

[<Test>]
let ``It turns right when faced with an obstacle`` () =
    let map = [ [ Available ]; [ Obstacle ] ]
    let guard = { Position = 0, 0; Facing = South }

    (map, guard)
    ||> next
    |> should equal (Some((0, 1), South), { guard with Facing = West })

let loopExample =
    "....#.....
.........#
..........
..#.......
.......#..
..........
.#.#^.....
........#.
#.........
......#..."

[<Test>]
let ``It stops after detecting a loop`` () =
    let map = loopExample |> Library.toGrid |> toMap

    let result = ((map, guard) ||> keepWalking State.init)

    result |> should equal (InLoopAfter(3, 6))

[<Test>]
let ``Example from adventofcode.com, count obstacles that cause loops`` () =
    let result = example |> solve guard

    result |> snd |> should equal 6
