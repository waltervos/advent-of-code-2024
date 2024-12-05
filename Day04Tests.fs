module Day04Tests

open NUnit.Framework
open FsUnit


open AOC2024
open System
open System.Text.RegularExpressions

[<Test>]
let ``Example from adventofcode.com`` () =
    let example =
        "MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX"

    example |> Day04.solve |> should equal 18

// [<Test>]
// let ``Solve the small example`` () =
//     let example =
//         "..X...
// .SAMX.
// .A..A.
// XMAS.S
// .X...."

//     example |> Day04.solve |> should equal '?' // Crashes on List.zip because width and height aren't equal

[<Test>]
let ``Finding coordinate series in a grid`` () =
    let grid = [ [ '1'; '2' ]; [ '3'; '4' ] ]

    let expected =
        [ 
          [ '1'; '2' ]
          [ '3'; '4' ]
          [ '1'; '3' ]
          [ '2'; '4' ]
          [ '1'; '4' ]
          [ '3'; '2' ] 
        ]

    grid
    |> Day04.explode
    |> Seq.map List.ofSeq
    |> List.ofSeq
    |> should equal expected

[<Test>]
let ``Finding coordinate series in a larger grid`` () =
    let grid = [ 
        [ 'A'; 'B'; 'C' ];
        [ 'D'; 'E'; 'F' ];
        [ 'G'; 'H'; 'I' ] ]

    grid
    |> Day04.explode
    |> Seq.map List.ofSeq
    |> List.ofSeq
    |> should be (supersetOf [ [ 'B'; 'F' ]; ['H'; 'F'] ])
