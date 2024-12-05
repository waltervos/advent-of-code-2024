module Day05Tests

open FsUnit
open NUnit.Framework

open AOC2024
open System

type Rule = { Page: int; MustBeBefore: int }

let example =
    "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

[<Test>]
let ``Example from adventofcode.com, part 1`` () =
    example |> Day05.parse ||> Day05.solvePart1 |> should equal 143

[<Test>]
let ``Example from adventofcode.com, part 2`` () =
    example |> Day05.parse ||> Day05.solvePart2 |> should equal 123


[<TestCase([| 1; 2; 3 |], 2)>]
[<TestCase([| 1; 2; 3; 4; 5 |], 3)>]
[<TestCase([| 1; 2; 3; 4; 5; 6; 7 |], 4)>]
let ``Finding the middle item in a list`` (l, middle) =
    l |> List.ofArray |> Day05.getMiddleItem |> should equal middle

[<Test>]
let ``Processing a correct update with rules, yields the middle page`` () =
    let update = [ 1; 2; 3 ]
    let rules = [ (1, 2); (2, 3) ]

    update |> Day05.getMiddlePageIfCorrect rules |> should equal 2

[<Test>]
let ``Processing a completely incorrect update with rules`` () =
    let update = [ 3; 2; 1 ]
    let rules = [ (1, 2); (2, 3) ]

    update |> Day05.getMiddlePageIfCorrect rules |> should equal 0

[<Test>]
let ``Processing a partially correct update with rules`` () =
    let update = [ 1; 3; 2 ]
    let rules = [ (1, 2); (2, 3) ]

    update |> Day05.getMiddlePageIfCorrect rules |> should equal 0

[<Test>]
let ``Sorting according to the rules`` () =            
    let update = [ 1; 3; 2 ]
    let rules = [ (1, 2); (2, 3); (4,3) ]
    let comparer = Day05.comparedByRules rules

    update
    |> Seq.sortWith comparer
    |> should equal [ 1; 2; 3 ]
