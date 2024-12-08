namespace AOC2024

open System.Text.RegularExpressions

type Instruction =
    | Multiplication of int * int
    | Do
    | Don't

type State =
    { Enabled: bool
      ConditionalTotal: int
      Total: int }

module Day03 =
    let classify (regexMatch: Match) =
        match
            regexMatch.Groups
            |> Seq.filter (fun g -> g.Length > 0)
            |> List.ofSeq
        with
        | [ _; first; second ] ->
            Multiplication(first.Value |> int, second.Value |> int)
        | [ group ] when group.Value = "do()" -> Do
        | [ group ] when group.Value = "don't()" -> Don't
        | _ -> failwith $"{regexMatch} is not a match we can classify."

    let solve puzzle =
        let pattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don\'t\\(\\)"

        let initialState =
            { Enabled = true
              ConditionalTotal = 0
              Total = 0 }

        let endState =
            Regex.Matches(puzzle, pattern)
            |> Seq.map classify
            |> Seq.fold
                (fun state value ->
                    match state, value with
                    | { Enabled = enabled }, Multiplication(first, second) ->
                        { state with
                            ConditionalTotal =
                                if enabled then
                                    state.ConditionalTotal + first * second
                                else
                                    state.ConditionalTotal
                            Total = state.Total + first * second }
                    | { Enabled = _ }, Do -> { state with Enabled = true }
                    | { Enabled = _ }, Don't -> { state with Enabled = false })
                initialState

        (endState.Total, endState.ConditionalTotal)

    let main =
        fun () ->
            let part1, part2 = Library.getInputForDay 3 |> solve

            $"Solutions for day 3:\nPart 1: {part1}\nPart 2: {part2}\n"
