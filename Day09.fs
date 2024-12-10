namespace AOC2024


type Block =
    | FileBlock of int
    | FreeBlock of int

    override this.ToString() : string =
        match this with
        | FileBlock id -> $"{id}"
        | FreeBlock length -> [ for i in 1..length -> '.'] |> List.toArray |> System.String


module Day09 =
    let day = 9
    
    let isFree =
        function
        | FreeBlock _ -> true
        | FileBlock _ -> false

    let isFile = isFree >> not

    let rec move blocks = // Nope. You need as many spaces as the file ID is long to move a file there
        match
            blocks |> Seq.tryFindIndex isFree,
            blocks |> Seq.tryFindIndexBack isFile
        with
        | Some firstFreeBlock, Some lastFileBlock when lastFileBlock >= firstFreeBlock ->
            blocks |> Seq.swap firstFreeBlock lastFileBlock |> move
        | _ -> blocks

    let checksum blocks =
        blocks
        |> Seq.mapi (fun position block -> 
            match block with
            | FileBlock id -> position * id
            | _ -> 0
        )
        |> Seq.map int64
        |> Seq.sum

    let parse puzzle =
        puzzle
        |> Seq.map (string >> int)
        |> Seq.mapi (fun index size ->
            seq {
                for _ in 0 .. size - 1 do
                    if index % 2 = 0 then FileBlock(index / 2) else FreeBlock size
            })
        |> Seq.concat

    let solve puzzle =
        puzzle |> parse |> move |> checksum

    let main =
        (fun () ->
            let part1 = Library.getInputForDay day |> solve

            $"Solutions for day {day}:\nPart 1: {part1}\n")
