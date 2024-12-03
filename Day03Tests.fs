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
let ``Example from adventofcode.com, part 1, with part 1 input`` () =
    exampleInputPt1 |> Day03.solve |> should equal (161, 161)

[<Test>]
let ``Example from adventofcode.com, part 2, with part 2 input`` () =
    exampleInputPt2 |> Day03.solve |> should equal (161, 48)
