namespace AOC2024

module Day10 =

    let findTrailsFrom (x, y) map =
        let rec inner (x, y) (map: int array2d) trails =
            printfn "Looking at %A, having value %i" (x,y) map[y,x]
            let upDownLeftRight =
                [ (x, y - 1); (x, y + 1); (x - 1, y); (x + 1, y) ]

            if map[y, x] <> 9 then
                [ for trail in trails do
                      for nextX, nextY in upDownLeftRight do
                          if
                              (nextX, nextY) |> Grid.inBounds map
                              && map[nextY, nextX] = (map[y, x] + 1)
                          then
                              printfn
                                  "%i at %A is one higher than %i"
                                  map[nextY, nextX]
                                  (nextX, nextY)
                                  map[y, x]

                              inner
                                  (nextX, nextY)
                                  map
                                  (trails @ [ [ (nextX, nextY) ] ])
                              |> List.concat
                              |> List.distinct // <- Definitely doing something wrong if we need this... :D
                          else
                              printfn "In the first else!"
                              trail ]
            else
                printfn "In the second else!"
                trails


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
