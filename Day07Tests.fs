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
        [ [ Mul; Mul; Mul ]
          [ Add; Mul; Mul ]
          [ Mul; Add; Mul ]
          [ Mul; Mul; Add ]
          [ Add; Add; Mul ]
          [ Mul; Add; Add ]
          [ Add; Mul; Add ]
          [ Add; Add; Add ] ]

[<Test>]
let ``It creates different sortings of all elements in the list`` () =
    List.allSorts [ Add; Add; Mul ]
    |> should equal [ [ Add; Add; Mul ]; [ Mul; Add; Add ]; [ Add; Mul; Add ] ]

[<Test>]
let ``It performs the operations left to right`` () =
    let numbers = [ 1; 2; 3 ] |> List.map int64
    let operations = [ Add; Mul ] |> List.map Day07.toFunction

    Day07.calculate numbers operations |> should equal 9

[<Test>]
let ``It performs the operations left to right, when in order of precedence``
    ()
    =
    let numbers = [ 1; 2; 3 ] |> List.map int64
    let operations = [ Mul; Add ] |> List.map Day07.toFunction

    Day07.calculate numbers operations |> should equal 5

[<TestCase(10, [| 5; 2; 3 |])>]
[<TestCase(3267, [| 81; 40; 27 |])>]
let ``Solve a valid example`` (testValue, numbers) =
    numbers
    |> List.ofArray
    |> List.map int64
    |> Day07.verify testValue
    |> should equal testValue

[<TestCase(2, [| 3; 1; 2 |], 0)>]
[<TestCase(100, [| 50; 50; 2 |], 0)>]
let ``Solve an invalid example`` (testValue, numbers, expected) =
    numbers
    |> List.ofArray
    |> List.map int64
    |> Day07.verify testValue
    |> should equal expected


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

[<Test>]
let ``With composition`` () =
    let mul a b = a * b

    let testValue = 3267
    let numbers = [ 81; 40; 27 ]

    let allCombinations =
        Day07.allCombinations (numbers.Length - 1)
        |> List.map (
            List.map (fun operator ->
                match operator with
                | Add -> (+)
                | Mul -> (fun x y -> x * y))
        )

    let calculate numbers operators =
        match numbers with
        | first :: rest ->
            first
            |> (List.map2
                    (fun number operator -> operator number)
                    rest
                    operators
                |> List.reduce (>>))
        | _ -> 0

    allCombinations
    |> List.map (fun combination -> calculate numbers combination)
    |> should be (supersetOf [ testValue ])
