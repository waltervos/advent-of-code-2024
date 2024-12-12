namespace AOC2024


type Block =
    | FileBlock of int
    | FreeBlock

    override this.ToString() : string =
        match this with
        | FileBlock id -> $"{id}"
        | FreeBlock -> "."

module Day09 =
    let day = 9

    let isFree =
        function
        | FreeBlock -> true
        | FileBlock _ -> false

    let isFile = isFree >> not


    let findLastFile' inBlocks =
        match inBlocks |> Array.tryFindIndexBack isFile with
        | Some index ->
            inBlocks[0..index]
            |> Array.indexed
            |> Array.rev
            |> Array.takeWhile (fun (i, b) -> isFile b)
            |> (fun fileBlocks ->
                match fileBlocks |> Array.head with
                | _, (FileBlock _ as fb) ->
                    fileBlocks |> Array.takeWhile (fun (i, b) -> b = fb)
                | _ -> fileBlocks)
            |> Array.rev
            |> Array.map fst
        | _ -> [||]

    let findLastFile downFrom (inBlocks: Block array) =
        inBlocks[0..downFrom] |> findLastFile'

    let tryFindFreeBlocks ofSize (inBlocks: Block array) =
        let inner ofSize inBlocks startingFrom =
            inBlocks
            |> Array.indexed
            |> Array.skip startingFrom
            |> Array.windowed ofSize
            |> Array.tryFind (Array.forall (snd >> isFree))
            |> (function
            | Some indexedBlocks -> Some(indexedBlocks |> Array.map fst)
            | _ -> None)

        inner ofSize inBlocks 0

    let compactFragmented blocks =
        let rec move blocks =
            match
                blocks |> Array.tryFindIndex isFree,
                blocks |> Array.tryFindIndexBack isFile
            with
            | Some firstFreeBlockIndex, Some lastFileBlockIndex when
                lastFileBlockIndex > firstFreeBlockIndex
                ->

                Array.set blocks firstFreeBlockIndex blocks[lastFileBlockIndex]
                Array.set blocks lastFileBlockIndex FreeBlock

                printfn
                    "Swapped free block at position %i with last file block at position %i (%.2f%%)"
                    firstFreeBlockIndex
                    lastFileBlockIndex
                    ((firstFreeBlockIndex |> double)
                     / (lastFileBlockIndex |> double)
                     * 100.)

                blocks |> move
            | _ -> blocks

        move blocks

    let compactDefragmented blocks =
        let rec move blocks maxIndexSeen =
            let lastFileIndexes = findLastFile (maxIndexSeen - 1) blocks

            if lastFileIndexes.Length = 0 then
                blocks
            else
                match blocks |> tryFindFreeBlocks lastFileIndexes.Length with
                | Some freeIndexes when
                    (freeIndexes |> Array.last) < (lastFileIndexes |> Array.head)
                    ->

                    for (i, index) in lastFileIndexes |> Array.indexed do
                        Array.set blocks freeIndexes[i] blocks[index]
                        Array.set blocks index FreeBlock

                    printfn
                        "Swapped free block at position %i with last file block at position %A (%.2f%% to go)"
                        (freeIndexes |> Array.head)
                        (lastFileIndexes |> Array.head)
                        ((lastFileIndexes |> Array.last |> double)
                         / (double blocks.Length)
                         * 100.)

                    move blocks (lastFileIndexes |> Array.head)
                | _ -> move blocks (lastFileIndexes |> Array.head)

        move blocks blocks.Length

    let checksum blocks =
        printfn "Calculating checksum..."

        blocks
        |> Array.mapi (fun position block ->
            match block with
            | FileBlock id -> uint64 (position * id)
            | _ -> 0UL)
        |> Array.sum

    let parse puzzle =
        puzzle
        |> Seq.map (string >> int)
        |> Seq.toArray
        |> Array.mapi (fun index size ->
            [| for _ in 0 .. size - 1 do
                   if index % 2 = 0 then FileBlock(index / 2) else FreeBlock |])
        |> Array.concat

    let solve puzzle =
        puzzle |> parse |> compactFragmented |> checksum,
        puzzle |> parse |> compactDefragmented |> checksum

    let main =
        (fun () ->
            let part1, part2 = Library.getInputForDay day |> solve

            $"Solutions for day {day}:\nPart 1: {part1}\nPart 2: {part2}\n")
