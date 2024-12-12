namespace AOC2024

module Day10 =

    let findTrailsFrom (x, y) map =
        let rec inner (x, y) (map: int array2d) trails =
            printfn "Looking at %A, having value %i" (x, y) map[y, x]

            match map[y, x] with
            | 9 -> trails
            | _ ->
                let up, down, left, right =
                    ((x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y))

                let stepUp =
                    if up |> Grid.inBounds map then
                        if map[snd up, fst up] = (map[y, x] + 1) then
                            inner
                                up
                                map
                                trails
                                |> List.map (fun trail ->
                                    if trail |> List.last = (x, y) then
                                        [ up ] |> List.append trail
                                    else
                                        trail)
                        else
                            []
                    else
                        []

                let stepDown =
                    if down |> Grid.inBounds map then
                        if map[snd down, fst down] = (map[y, x] + 1) then
                            inner 
                                down
                                map
                                trails
                                |> List.map (fun trail ->
                                    if trail |> List.last = (x, y) then
                                        [ down ] |> List.append trail
                                    else
                                        trail)
                        else
                            []
                    else
                        []

                let stepLeft =
                    if left |> Grid.inBounds map then
                        if map[snd left, fst left] = (map[y, x] + 1) then
                            inner
                                left
                                map
                                trails
                                |> List.map (fun trail ->
                                    if trail |> List.last = (x, y) then
                                        [ left ] |> List.append trail
                                    else
                                        trail)
                        else
                            []
                    else
                        []

                let stepRight =
                    if right |> Grid.inBounds map then
                        if map[snd right, fst right] = (map[y, x] + 1) then
                            inner
                                right
                                map
                                trails
                                |> List.map (fun trail ->
                                    if trail |> List.last = (x, y) then
                                        [ right ] |> List.append trail
                                    else
                                        trail)
                        else
                            []
                    else
                        []

                List.concat [ stepUp; stepDown; stepLeft; stepRight ] |> List.distinct


        inner (x, y) map [ [ (x, y) ] ]
        |> List.sortByDescending List.length
        |> (fun trails ->
            let longestTrail = trails |> List.head |> List.length

            let filtered =
                trails |> List.filter (fun l -> l.Length = longestTrail)

            printfn "We're here?\n"
            filtered)

    let main =
        (fun () ->
            let map = Array2D.create 4 4 0

            [ [ 0; 1; 2; 3 ]; [ 1; 2; 3; 4 ]; [ 8; 7; 6; 5 ]; [ 9; 8; 7; 6 ] ]
            |> List.iteri (fun y r ->
                r |> List.iteri (fun x i -> map[y, x] <- i))

            printfn "%A" map

            map
            |> findTrailsFrom (0, 0)
            |> (fun r ->
                printfn "Klaar:"
                printfn "%A" r
                r)
            |> ignore

            $"Klaar!!!")
