module Day09Tests

open FsUnit
open NUnit.Framework

open AOC2024
open AOC2024.Day09

let diskBefore =
    [ FileBlock 0
      FreeBlock 2
      FreeBlock 2
      FileBlock 1
      FileBlock 1
      FileBlock 1
      FreeBlock 4
      FreeBlock 4
      FreeBlock 4
      FreeBlock 4
      FileBlock 2
      FileBlock 2
      FileBlock 2
      FileBlock 2
      FileBlock 2 ]

let diskAfter =
    [ FileBlock 0
      FileBlock 2
      FileBlock 2
      FileBlock 1
      FileBlock 1
      FileBlock 1
      FileBlock 2
      FileBlock 2
      FileBlock 2
      FreeBlock 4
      FreeBlock 4
      FreeBlock 4
      FreeBlock 4
      FreeBlock 2
      FreeBlock 2 ]

[<Test>]
let ``It does something`` () =
    diskAfter
    |> List.mapi (fun index block ->
        match block with
        | FileBlock id -> index * id
        | FreeBlock _ -> 0)
    |> List.sum
    |> should equal 60

[<Test>]
let ``It does something else`` () =
    let myDiskAfter =
        diskBefore
        |> List.swap 1 14
        |> List.swap 2 13
        |> List.swap 6 12
        |> List.swap 7 11
        |> List.swap 8 10

    diskAfter |> should equal myDiskAfter

[<Test>]
let ``Parsing a small example`` () =
    let example = "12345"

    example |> parse |> should equal diskBefore

[<Test>]
let ``It moves one file`` () =
    [ FileBlock 0; FreeBlock 1; FileBlock 1 ]
    |> move
    |> should equal [ FileBlock 0; FileBlock 1; FreeBlock 1 ]

[<Test>]
let ``It moves files until done`` () =
    diskBefore |> move |> should equal diskAfter

[<Test>]
let ``Solving the large example`` () =
    let example = "2333133121414131402"

    example |> solve |> should equal 1928
