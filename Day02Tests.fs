module Day02Tests

open NUnit.Framework
open FsUnit


open AOC2024

let exampleInput =
    [ [ 7; 6; 4; 2; 1 ]
      [ 1; 2; 7; 8; 9 ]
      [ 9; 7; 6; 2; 1 ]
      [ 1; 3; 2; 4; 5 ]
      [ 8; 6; 4; 4; 1 ]
      [ 1; 3; 6; 7; 9 ] ]

[<Test>]
let ``Example from adventofcode.com`` () =
    exampleInput |> Day02.solvePart1 |> should equal 2

[<Test>]
let ``Example from adventofcode.com, part 2`` () =
    exampleInput |> Day02.solvePart2 |> should equal 4

[<Test>]
let ``Parsing the input format`` () =
    let input =
        "1 2
3 4"

    input |> Day02.parse |> should equal [ [ 1; 2 ]; [ 3; 4 ] ]

[<Test>]
let ``It creates variants of the same report`` () =
    [ 1; 2; 3 ] |> Day02.toVariants |> should equal [ [ 2; 3 ]; [ 1; 3 ]; [ 1; 2 ] ]
