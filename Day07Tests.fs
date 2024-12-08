module Day07Tests

open FsUnit
open NUnit.Framework

open AOC2024
open System

[<Test>]
let ``It creates all combinations of + and * of a certain length`` () =
    Day07.allCombinations 3
    |> should
        equal
        [ [ "*"; "*"; "*" ]
          [ "+"; "*"; "*" ]
          [ "*"; "+"; "*" ]
          [ "*"; "*"; "+" ]
          [ "+"; "+"; "*" ]
          [ "*"; "+"; "+" ]
          [ "+"; "*"; "+" ]
          [ "+"; "+"; "+" ] ]

[<Test>]
let ``It creates different sortings of all elements in the list`` () =
    List.allSorts [ "+"; "+"; "*" ]
    |> should equal [ [ "+"; "+"; "*" ]; [ "*"; "+"; "+" ]; [ "+"; "*"; "+" ] ]

[<Test>]
let ``It performs the operations left to right`` () =
    let numbers = [ 1; 2; 3 ] |> List.map int64
    let operations = [ "+"; "*" ]

    Day07.calculate numbers operations |> should equal 9

[<Test>]
let ``It performs the operations left to right, when in order of precedence``
    ()
    =
    let numbers = [ 1; 2; 3 ] |> List.map int64
    let operations = [ "*"; "+" ]

    Day07.calculate numbers operations |> should equal 5

[<Test>]
let ``Solve an example`` () =
    let testValue = 3267
    let numbers = [ 81; 40; 27 ] |> List.map int64

    numbers |> Day07.verify testValue |> should equal testValue


[<Test>]
let ``Solving the example puzzle`` () =
    let example =
        "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

    example |> Day07.solve |> should equal 3749
