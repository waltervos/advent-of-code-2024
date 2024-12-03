namespace AOC2024

type Instruction =
    | Mul of int * int
    | Do
    | Don't
    | Noop

    member this.Result =
        match this with
        | Mul(first, second) -> first * second
        | _ -> 0

type State = { Enabled: bool; Total: int }


module Day03 =
    open System.Text.RegularExpressions

    let classify (regexMatch: Match) =
        match regexMatch.Groups |> List.ofSeq |> List.filter (fun g -> g.Length > 0) with
        | [ _; first; second ] -> Mul(first.Value |> int, second.Value |> int)
        | [ group ] when group.Value = "do()" -> Do
        | [ group ] when group.Value = "don't()" -> Don't
        | _ -> Noop

    let solvePart1 puzzle =
        let pattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

        Regex.Matches(puzzle, pattern)
        |> Seq.map classify
        |> Seq.map (fun i -> i.Result)
        |> Seq.sum

    let solvePart2 puzzle =
        let pattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don\'t\\(\\)"

        Regex.Matches(puzzle, pattern)
        |> Seq.map classify
        |> Seq.fold
            (fun state value ->
                match state, value with
                | { Enabled = true; Total = currentTotal }, (Mul(_, _) as multiplication) ->
                    { state with
                        Total = multiplication.Result + currentTotal }
                | { Enabled = _; Total = _ }, Do -> { state with Enabled = true }
                | { Enabled = _; Total = _ }, Don't -> { state with Enabled = false }
                | _ -> state)
            { Enabled = true; Total = 0 }
        |> (fun s -> s.Total)

    let solve puzzle =
        (puzzle |> solvePart1, puzzle |> solvePart2)

    let main =
        let part1, part2 = Library.getInputForDay 3 |> solve

        $"Solutions for day 3:\nPart 1: {part1}\nPart 2: {part2}\n"
