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

    let rec move blocks =
        match
            blocks |> Array.tryFindIndex isFree,
            blocks |> Array.tryFindIndexBack isFile
        with
        | Some firstFreeBlockIndex, Some lastFileBlockIndex when lastFileBlockIndex >= firstFreeBlockIndex ->
            
            Array.set blocks firstFreeBlockIndex blocks[lastFileBlockIndex]
            Array.set blocks lastFileBlockIndex FreeBlock

            printfn "Swapped free block at position %i with last file block at position %i (%.2f%%)"
                firstFreeBlockIndex
                lastFileBlockIndex
                ((firstFreeBlockIndex |> double) / (lastFileBlockIndex |> double) * 100.)
            
            blocks |> move
        | _ -> blocks

    let checksum blocks =
        blocks
        |> Array.mapi (fun position block -> 
            match block with
            | FileBlock id -> uint64 (position * id)
            | _ -> 0UL
        )
        |> Array.sum

    let parse puzzle =
        puzzle
        |> Seq.map (string >> int)
        |> Seq.toArray
        |> Array.mapi (fun index size ->
            [|
                for _ in 0 .. size - 1 do
                    if index % 2 = 0 then FileBlock(index / 2) else FreeBlock
            |])
        |> Array.concat

    let solve puzzle =
        puzzle |> parse |> move |> checksum

    let main =
        (fun () ->
            let part1 = Library.getInputForDay day |> solve

            $"Solutions for day {day}:\nPart 1: {part1}\n")
