module Day11Tests

open AOC2024
open AOC2024.Day11

open NUnit.Framework
open FsUnit
open System.Collections.Generic



[<Test>]
let ``It blinks once`` () =
    let example = "0 1 10 99 999"
    let cache = Dictionary()

    example
    |> parse
    |> Seq.map (blink 1 cache)
    |> Seq.sum
    |> should equal 7

[<Test>]
let ``It solves the example`` () =
    let example = "125 17"
    let cache = Dictionary()

    example
    |> parse
    |> Seq.map (blink 25 cache)
    |> Seq.sum
    |> should equal 55312
