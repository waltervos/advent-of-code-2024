namespace AOC2024


type Direction =
    | North
    | South
    | East
    | West

type Position = int * int

type Guard =
    { Position: Position
      Facing: Direction }

type Spot =
    | Available
    | Obstacle
    | Guard of Guard

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
        let map =
            grid
            |> List.mapi (fun y row ->
                row
                |> List.mapi (fun x column ->
                    match column with
                    | '.' -> Available
                    | '^' -> Guard { Position = (x, y); Facing = North }
                    | '#' -> Obstacle
                    | _ -> failwith $"{column} is not a valid character"))

        // We can now find the guard from the map, yes?

        map

    let parse puzzle = puzzle |> Library.toGrid |> toMap

    let solvePart1 map guard = (map, guard) ||> keepWalking State.init

    let solvePart2 map guard positionsHeld =
        let alternativeMaps =
            seq {
                for (x, y) in positionsHeld ->
                    map
                    |> List.mapi (fun yI row ->
                        row
                        |> List.mapi (fun xI spot ->
                            if
                                xI = x && yI = y && (x, y) <> guard.Position
                            then
                                Obstacle
                            else
                                spot))
            }

        let versions = alternativeMaps |> Seq.length

        seq {
            for i, alternativeMap in alternativeMaps |> Seq.indexed ->
                (solvePart1 alternativeMap guard)
        }
        |> Seq.filter (fun ended ->
            match ended with
            | InLoopAfter _ -> true
            | _ -> false

        )
        |> Seq.length

    let solve guard puzzle =
        let map = puzzle |> parse
        let part1 = ((map, guard) ||> solvePart1)

        match part1 with
        | OffMapAfter positionsHeld ->
            positionsHeld |> List.length,
            ((map, guard) ||> solvePart2 <| positionsHeld)
        | _ -> failwith "Somehow we didn't go off map after part 1?"

    let main =
        fun () ->
            let guard = { Position = 59, 71; Facing = North }
            let part1, part2 = Library.getInputForDay day |> solve guard

            $"Solutions for day {day}:\nPart 1: {part1}\nPart 2: {part2}\n"
