module Day09Tests

open FsUnit
open NUnit.Framework

open AOC2024
open AOC2024.Day09

let diskBefore =
    (fun () ->
        [| FileBlock 0
           FreeBlock
           FreeBlock
           FileBlock 1
           FileBlock 1
           FileBlock 1
           FreeBlock
           FreeBlock
           FreeBlock
           FreeBlock
           FileBlock 2
           FileBlock 2
           FileBlock 2
           FileBlock 2
           FileBlock 2 |])

let diskAfter =
    [| FileBlock 0
       FileBlock 2
       FileBlock 2
       FileBlock 1
       FileBlock 1
       FileBlock 1
       FileBlock 2
       FileBlock 2
       FileBlock 2
       FreeBlock
       FreeBlock
       FreeBlock
       FreeBlock
       FreeBlock
       FreeBlock |]

[<Test>]
let ``It calculates the checksum`` () =
    diskAfter |> checksum |> should equal 60

[<Test>]
let ``Parsing a small example`` () =
    let example = "12345"

    example |> parse |> should equal (diskBefore ())

[<Test>]
let ``It moves one file`` () =
    [| FileBlock 0; FreeBlock; FileBlock 1 |]
    |> move
    |> should equal [| FileBlock 0; FileBlock 1; FreeBlock |]

[<Test>]
let ``It moves files until done`` () =
    diskBefore () |> move |> should equal diskAfter

[<Test>]
let ``Solving the large example`` () =
    let example = "2333133121414131402"

    example |> solve |> should equal 1928
