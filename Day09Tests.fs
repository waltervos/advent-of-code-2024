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
    |> compactFragmented
    |> should equal [| FileBlock 0; FileBlock 1; FreeBlock |]

[<Test>]
let ``It moves files until done`` () =
    diskBefore () |> compactFragmented |> should equal diskAfter

[<Test>]
let ``Solving the large example for part 1 and 2`` () =
    let example = "2333133121414131402"

    example |> solve |> should equal (1928, 2858)

[<Test>]
let ``Finding all the block indexes of the last file`` () =
    [| FreeBlock; FileBlock 0; FileBlock 0 |]
    |> findLastFile'
    |> should equal [| 1; 2 |]

[<Test>]
let ``Finding all the block indexes of the last file, skipping indexes already visited``
    ()
    =
    [| FileBlock 0
       FreeBlock
       FileBlock 1
       FreeBlock
       FileBlock 2
       FileBlock 2 |]
    |> findLastFile 3
    |> should equal [| 2 |]

[<Test>]
let ``Finding enough free blocks to store a file`` () =
    (2, [| FreeBlock; FreeBlock; FileBlock 0; FileBlock 0 |])
    ||> tryFindFreeBlocks
    |> should equal (Some [| 0; 1 |])

[<Test>]
let ``Finding insufficient free blocks to store a file`` () =
    (2, [| FreeBlock; FileBlock 0; FileBlock 0 |])
    ||> tryFindFreeBlocks
    |> should equal None


[<Test>]
let ``Moving a complete file, part 2`` () =
    [| FreeBlock; FreeBlock; FileBlock 0; FileBlock 0 |]
    |> compactDefragmented
    |> should equal [| FileBlock 0; FileBlock 0; FreeBlock; FreeBlock |]

[<Test>]
let ``Moving a complete file when there's no space, part 2`` () =
    [| FreeBlock; FileBlock 0; FileBlock 0 |]
    |> compactDefragmented
    |> should equal [| FreeBlock; FileBlock 0; FileBlock 0 |]

[<Test>]
let ``Moving a complete file when there's nothing to move, part 2`` () =
    [| FileBlock 0; FileBlock 0; FreeBlock; FreeBlock |]
    |> compactDefragmented
    |> should equal [| FileBlock 0; FileBlock 0; FreeBlock; FreeBlock |]

[<Test>]
let ``It only moves the second last file when the last file is too big`` () =
    [| FreeBlock
       FreeBlock
       FileBlock 0
       FreeBlock
       FileBlock 1
       FileBlock 1
       FileBlock 1 |]
    |> compactDefragmented
    |> should
        equal
        [| FileBlock 0
           FreeBlock
           FreeBlock
           FreeBlock
           FileBlock 1
           FileBlock 1
           FileBlock 1 |]

[<Test>]
let ``It compacts the example disc in part 2`` () =
    let example = "2333133121414131402"

    example
    |> parse
    |> compactDefragmented
    |> should
        equal
        [| FileBlock 0
           FileBlock 0
           FileBlock 9
           FileBlock 9
           FileBlock 2
           FileBlock 1
           FileBlock 1
           FileBlock 1
           FileBlock 7
           FileBlock 7
           FileBlock 7
           FreeBlock
           FileBlock 4
           FileBlock 4
           FreeBlock
           FileBlock 3
           FileBlock 3
           FileBlock 3
           FreeBlock
           FreeBlock
           FreeBlock
           FreeBlock
           FileBlock 5
           FileBlock 5
           FileBlock 5
           FileBlock 5
           FreeBlock
           FileBlock 6
           FileBlock 6
           FileBlock 6
           FileBlock 6
           FreeBlock
           FreeBlock
           FreeBlock
           FreeBlock
           FreeBlock
           FileBlock 8
           FileBlock 8
           FileBlock 8
           FileBlock 8
           FreeBlock
           FreeBlock |]

[<Test>]
let ``It keeps searching until it finds a suitable free block`` () =
    [| FileBlock 0; FreeBlock; FileBlock 1; FileBlock 1; FreeBlock; FreeBlock; FileBlock 2 |]
    |> tryFindFreeBlocks 2
    |> should equal (Some [|4;5|])
