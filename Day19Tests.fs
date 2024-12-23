module Day19Tests

open NUnit.Framework
open FsUnit

open AOC2024.Day19


[<Test>]
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

    let towels, designs = parse puzzle

    designs
    |> Seq.map (isPossible towels)
    |> Seq.filter (id)
    |> Seq.length
    |> should equal 6
