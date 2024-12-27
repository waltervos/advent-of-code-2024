namespace AOC2024

type DesignBin =
    { remaining: string }

    member this.capacity =
        this.remaining.Length
        - (this.remaining |> String.filter (fun c -> c = ' ')).Length

    member this.add towel =
        { this with
            remaining = String.replace this.remaining ' ' towel }

    member this.canFit(towel: string) = this.remaining.Contains(towel)

module Day19 =
    open System

    let isWhollyImpossible towels design =
        towels
        |> Array.fold
            (fun s t -> s |> Seq.append (design |> String.allIndexesOf t))
            Seq.empty<int seq>
        |> Seq.concat
        |> Seq.distinct
        |> Seq.sort
        |> Seq.forall2 (=) [ 0 .. design.Length - 1 ]
        |> not

    let parse puzzle =
        match
            puzzle
            |> String.splitOnString "\r\n\r\n"
            |> Array.partition (fun s -> s.Contains(","))
        with
        | (towels, designs) ->
            (towels |> Array.item 0 |> String.split ','),
            (designs |> Array.item 0 |> String.split '\n')

    let any predicate array =
        match array |> Array.tryFind predicate with
        | Some el -> true
        | _ -> false

    let solve puzzle =
        let towels, designs = parse puzzle
        towels, designs

    let main =
        fun () ->
            // let day = 19
            // let part1 = Library.getInputForDay day |> solve

            // $"Solutions for day {day}:\nPart 1: {part1}\n"

            $"No idea\n"
