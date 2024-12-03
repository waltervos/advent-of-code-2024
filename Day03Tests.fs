module Day03Tests

open NUnit.Framework
open FsUnit


open AOC2024


let exampleInputPt1 =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let exampleInputPt2 =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
"

[<Test>]
let ``Example from adventofcode.com, part 1`` () =
    exampleInputPt1 |> Day03.solvePart1 |> should equal 161

[<Test>]
let ``Example from adventofcode.com, part 2`` () =
    exampleInputPt2 |> Day03.solvePart2 |> should equal 48
