namespace AOC2024

module Day11 =
    open System
    open System.Collections.Generic

    let equalPartsOf (s: string) =
        let halfLength = (s |> String.length) / 2

        let left = s[0 .. halfLength - 1]
        let right = s[halfLength..].TrimStart('0')

        (left, if right.Length > 0 then right else "0")

    let rec blink times (cache: Dictionary<string * int, int64>) stone =
        let rec innerBlink blinksLeft stone =
            if blinksLeft = 0 then
                1L
            else
                let innerBlink' = innerBlink (blinksLeft - 1)
                let cacheKey = (stone, blinksLeft)

                match cache.ContainsKey(cacheKey), stone with
                | true, _ -> ()
                | false, "0" ->
                    cache.TryAdd(cacheKey, "1" |> innerBlink')
                    |> ignore
                | false, _ when stone.Length % 2 = 0 ->
                    let left, right = equalPartsOf stone

                    cache.TryAdd(
                        cacheKey,
                        (left |> innerBlink') + (right |> innerBlink')
                    )
                    |> ignore
                | false, _ ->
                    let newStone = (int64 stone) * 2024L |> string

                    cache.TryAdd(cacheKey, newStone |> innerBlink')
                    |> ignore

                cache[cacheKey]

        printfn "%O: Blinking %i times at stone %s" DateTime.Now times stone

        let count = stone |> innerBlink times

        printfn "%O: Stone %s evolved into %i stones" DateTime.Now stone count

        count

    let parse puzzle = puzzle |> String.split ' '

    let solve puzzle =
        let cache = Dictionary()

        let parsed = puzzle |> parse
        let part1 = parsed |> Seq.map (blink 25 cache)

        // let cache = Map.empty<int * int, int>
        let part2 = parsed |> Seq.map (blink 75 cache)

        part1 |> Seq.sum, part2 |> Seq.sum

    let main =
        fun () ->
            let day = 11
            let part1, part2 = Library.getInputForDay day |> solve

            $"Solutions for day 11:\nPart 1: {part1}\nPart 2: {part2}\n"
