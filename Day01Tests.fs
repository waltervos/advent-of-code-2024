module Day01Tests

open NUnit.Framework
open FsUnit

let solve l r = 11

[<Test>]
let ``Example from adventofcode.com`` () =
    let left = [ 3; 4; 2; 1; 3; 3 ]

    let right = [ 4; 3; 5; 3; 9; 3 ]

    (left, right) ||> solve |> should equal 11

