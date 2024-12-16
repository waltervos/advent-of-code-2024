namespace AOC2024

type Quadrant =
    | NorthWest
    | NorthEast
    | SouthEast
    | SouthWest
    | BetweenQuadrants

type Robot =
    { Position: int * int
      Velocity: int * int }

    member this.Move(gridWidth, gridHeight) =
        let x, y = this.Position
        let horizontalVelocity, verticalVelocity = this.Velocity
        let newX = x + horizontalVelocity
        let newY = y + verticalVelocity

        { this with
            Position =
                (if newX >= gridWidth then newX % gridWidth
                 elif newX < 0 then gridWidth - (abs newX % gridWidth)
                 else newX),
                (if newY >= gridHeight then newY % gridHeight
                 elif newY < 0 then gridHeight - (abs newY % gridHeight)
                 else newY) }

module Day14 =
    open System.Text
    open System.Threading

    let toString (width, height) robots =
        let positions = robots |> Array.ofSeq |> Array.map _.Position

        let sb = StringBuilder()

        for y in [ 0 .. height - 1 ] do
            if y > 0 then
                sb.AppendLine("") |> ignore

            for x in [ 0 .. width - 1 ] do
                if positions |> Array.contains (x, y) then
                    sb.Append("#") |> ignore
                else
                    sb.Append(".") |> ignore

        sb |> string

    let toQuadrant (gridWidth, gridHeight) (xPosition, yPosition) =
        let horizontalMiddle = gridWidth / 2
        let verticalMiddle = gridHeight / 2

        if xPosition = horizontalMiddle || yPosition = verticalMiddle then
            BetweenQuadrants
        elif xPosition < horizontalMiddle then
            if yPosition < verticalMiddle then NorthWest else SouthWest
        else if yPosition < verticalMiddle then
            NorthEast
        else
            SouthEast

    let countByQuadrant (width, height) (robots: Robot seq) =
        robots
        |> Seq.map (fun r -> toQuadrant (width, height) r.Position)
        |> Seq.filter ((<>) BetweenQuadrants)
        |> Seq.fold
            (fun s q ->
                s
                |> Map.change q (function
                    | Some count -> Some(count + 1)
                    | _ -> Some 1))
            Map.empty<Quadrant, int>

    let rec move timesLeft gridSize (robot: Robot) =
        if timesLeft = 0 then
            robot
        else
            robot.Move gridSize |> move (timesLeft - 1) gridSize

    let mightBeChristmas robots =
        robots
        |> Seq.countBy _.Position
        |> Seq.forall (snd >> (=) 1)

    let rec moveUntilChristmas secondsPassed gridSize robots =
        let currentRobots =
            robots
            |> Seq.map (move 1 gridSize)
        
        let currentSecondsPassed = (secondsPassed + 1)

        if currentSecondsPassed % 1000 = 0 then
            printfn "%O: Looking at second %i"  System.DateTime.Now currentSecondsPassed

        if currentRobots |> mightBeChristmas then
            System.Console.Clear()
            System.Console.SetCursorPosition(0, 0)

            printfn "%s" (currentRobots |> toString gridSize)

            printfn
                "Is this a Christmas tree (%i)?"
                currentSecondsPassed

            match System.Console.ReadKey() |> string with
            | "Y"
            | "y" -> currentSecondsPassed
            | "N"
            | "n"
            | _ -> robots |> moveUntilChristmas currentSecondsPassed gridSize
        else
            currentRobots |> moveUntilChristmas currentSecondsPassed gridSize

    let parse puzzle =
        puzzle
        |> String.split '\n'
        |> Seq.map (String.split ' ')
        |> Seq.map (fun pAndV ->
            match
                pAndV
                |> Array.map (fun pOrV ->
                    pOrV
                    |> String.remove 0 2
                    |> String.split ','
                    |> Array.map int)
                |> Array.concat
            with
            | [| positionX; positionY; velocityX; velocityY |] as r ->
                { Position = positionX, positionY
                  Velocity = velocityX, velocityY }
            | _ -> failwith "Can't parse!")

    let solvePart1 (width, height) robots =
        robots
        |> Seq.map (move 100 (width, height))
        |> countByQuadrant (width, height)
        |> Map.values
        |> Seq.reduce (*)

    // let solvePart2 = moveUntilChristmas 0

    let solve (width, height) puzzle =
        let robots = puzzle |> parse

        let part1 = robots |> solvePart1 (width, height)

        let part2 = robots |> moveUntilChristmas 0 (width, height)

        part1, part2

    let main =
        fun () ->
            let day = 14
            let part1, part2 = Library.getInputForDay day |> solve (101, 103)

            $"Solutions for day {day}:\nPart 1: {part1}\nPart2: {part2}"
