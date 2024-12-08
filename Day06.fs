namespace AOC2024

type Spot =
    | Available
    | Obstacle
    | Guard

type Direction =
    | North
    | South
    | East
    | West

type Position = int * int

type Guard =
    { Position: Position
      Facing: Direction }

type Ended =
    | OffMapAfter of Position list
    | InLoopAfter of Position

module Day06 =
    let day = 6

    type State =
        { PositionsHeld: Position list
          ObstaclesMet: (Position * Direction) list }

        static member init =
            { PositionsHeld = []
              ObstaclesMet = [] }

    let turnRight from =
        let next =
            Map[(North, East)
                (East, South)
                (South, West)
                (West, North)]

        next[from]

    let next (map: Spot list list) guard =
        let x, y =
            if guard.Facing = South then
                guard.Position |> fst, (guard.Position |> snd) + 1
            elif guard.Facing = East then
                (guard.Position |> fst) + 1, guard.Position |> snd
            elif guard.Facing = North then
                (guard.Position |> fst), (guard.Position |> snd) - 1
            elif guard.Facing = West then
                (guard.Position |> fst) - 1, guard.Position |> snd
            else
                guard.Position

        match map |> List.tryItem y with
        | Some row ->
            match row |> List.tryItem x with
            | Some Obstacle ->
                Some((x, y), guard.Facing),
                { guard with
                    Facing = guard.Facing |> turnRight }
            | _ -> None, { guard with Position = x, y }
        | _ -> None, { guard with Position = x, y }

    let inBoundsOf (map: 'a list list) (x, y) =
        x >= 0 && x < map[0].Length && y >= 0 && y < map.Length

    let rec keepWalking state map guard =
        let obstacle, nextGuard = guard |> next map
        let x, y = nextGuard.Position

        if (x, y) |> inBoundsOf map then
            let newState =
                { state with
                    PositionsHeld = state.PositionsHeld @ [ nextGuard.Position ] }

            match obstacle with
            | Some obstacleMet when
                state.ObstaclesMet |> List.contains obstacleMet
                ->
                InLoopAfter(state.ObstaclesMet |> List.last |> fst)
            | Some obstacleMet ->
                nextGuard
                |> keepWalking
                    ({ newState with
                        ObstaclesMet = state.ObstaclesMet @ [ obstacleMet ] })
                    map
            | _ -> nextGuard |> keepWalking newState map
        else
            OffMapAfter(state.PositionsHeld |> List.distinct)

    let toMap grid =
        grid
        |> List.map (
            List.map (fun c ->
                match c with
                | '.'
                | '^' -> Available
                | '#' -> Obstacle
                | _ -> failwith $"{c} is not a valid character")
        )


    let parse puzzle = puzzle |> Library.toGrid |> toMap

    let solvePart1 map guard = (map, guard) ||> keepWalking State.init

    let solvePart2 map guard positionsHeld =
        // let alternativeMaps =
        //     [ for (x, y) in positionsHeld ->
        //           map
        //           |> List.mapi (fun yI row ->
        //               row
        //               |> List.mapi (fun xI spot ->
        //                   if xI = x && yI = y then Obstacle else spot)) ]

        let alternativeMaps =
            [ for y in 0 .. (map |> List.length) - 1 do
                  for x in 0 .. (map |> List.length) - 1 ->
                      map |> List.change y (map[y] |> List.change x Obstacle) ]
            |> List.filter (fun aMap -> aMap <> map)

        [ for i, alternativeMap in alternativeMaps |> List.indexed ->
              (solvePart1 alternativeMap guard) ]
        |> List.filter (fun ended ->
            match ended with
            | InLoopAfter _ -> true
            | _ -> false

        )
        |> List.distinct
        |> List.length

    let solve guard puzzle =
        let map = puzzle |> parse
        let part1 = ((map, guard) ||> solvePart1)

        match part1 with
        | OffMapAfter positionsHeld ->
            positionsHeld |> List.length,
            ((map, guard) ||> solvePart2 <| positionsHeld)
        | _ -> failwith "Somehow we didn't go off map after part 1?"

    let main =
        let guard = { Position = 59, 71; Facing = North }
        let part1, part2 = Library.getInputForDay day |> solve guard

        $"Solutions for day {day}:\nPart 1: {part1}\nPart 2: {part2}\n"
