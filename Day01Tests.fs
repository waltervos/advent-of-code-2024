module Day01Tests

open NUnit.Framework
open FsUnit


open AOC2024

[<Test>]
let ``Example from adventofcode.com`` () =
    let left = [ 3; 4; 2; 1; 3; 3 ]

    let right = [ 4; 3; 5; 3; 9; 3 ]

    (left, right) ||> Day01.solve |> should equal (11, 31)

[<Test>]
let ``Parsing the input format`` () =
    let input = "1   2
3   4
5   6"

    input |> Day01.parse |> should equal ([ 1; 3; 5 ], [ 2; 4; 6 ])
