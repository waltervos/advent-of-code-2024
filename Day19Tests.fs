module Day19Tests

open NUnit.Framework
open FsUnit

open AOC2024.Day19


[<Test>]
[<Ignore("Hmm")>]
let ``Solving the example`` () =
    let puzzle =
        "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"

    puzzle
    |> solve
    |> should equal 6
