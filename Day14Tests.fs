module Day14Tests

open NUnit.Framework
open FsUnit

open AOC2024
open AOC2024.Day14
open System.Text

let position robot = robot.Position

[<Test>]
let ``It moves to the right`` () =
    let robot = { Position = 0, 0; Velocity = 1, 0 }
    let gridSize = 2, 1

    (gridSize, robot)
    ||> (fun gridSize robot -> robot.Move gridSize)
    |> position
    |> should equal (1, 0)

[<Test>]
let ``It moves down`` () =
    let robot = { Position = 0, 0; Velocity = 0, 1 }
    let gridSize = 1, 2

    (gridSize, robot)
    ||> (fun gridSize robot -> robot.Move gridSize)
    |> position
    |> should equal (0, 1)

[<Test>]
let ``It moves to the left`` () =
    let robot = { Position = 1, 0; Velocity = -1, 0 }
    let gridSize = 2, 1

    (gridSize, robot)
    ||> (fun gridSize robot -> robot.Move gridSize)
    |> position
    |> should equal (0, 0)

[<Test>]
let ``It moves up`` () =
    let robot = { Position = 0, 1; Velocity = 0, -1 }
    let gridSize = 1, 2

    (gridSize, robot)
    ||> (fun gridSize robot -> robot.Move gridSize)
    |> position
    |> should equal (0, 0)

[<Test>]
let ``It moves diagonally`` () =
    let robot = { Position = 0, 0; Velocity = 1, 1 }
    let gridSize = 2, 2

    (gridSize, robot)
    ||> (fun gridSize robot -> robot.Move gridSize)
    |> position
    |> should equal (1, 1)

[<Test>]
let ``It teleports horizontally`` () =
    let robot = { Position = 1, 0; Velocity = 1, 0 }
    let gridSize = 2, 1

    (gridSize, robot)
    ||> (fun gridSize robot -> robot.Move gridSize)
    |> position
    |> should equal (0, 0)

[<Test>]
let ``It teleports vertically`` () =
    let robot = { Position = 0, 1; Velocity = 0, 1 }
    let gridSize = 1, 2

    (gridSize, robot)
    ||> (fun gridSize robot -> robot.Move gridSize)
    |> position
    |> should equal (0, 0)

[<Test>]
let ``It teleports diagonally`` () =
    let robot = { Position = 1, 1; Velocity = 1, 1 }
    let gridSize = 2, 2

    (gridSize, robot)
    ||> (fun gridSize robot -> robot.Move gridSize)
    |> position
    |> should equal (0, 0)

[<Test>]
let ``It teleports backwards diagonally`` () =
    let robot = { Position = 0, 0; Velocity = -1, -1 }
    let gridSize = 2, 2

    (gridSize, robot)
    ||> (fun gridSize robot -> robot.Move gridSize)
    |> position
    |> should equal (1, 1)

[<Test>]
let ``It teleports diagonally at a non 45 degree angle`` () =
    let robot = { Position = 0, 0; Velocity = 1, 2 }
    let gridSize = 2, 2

    (gridSize, robot)
    ||> (fun gridSize robot -> robot.Move gridSize)
    |> position
    |> should equal (1, 0)

[<Test>]
let ``It parses the example input`` () =
    let example =
        "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"

    example
    |> parse
    |> should
        be
        (supersetOf
            [ { Position = 0, 4; Velocity = 3, -3 }
              { Position = 9, 5; Velocity = -3, -3 } ])

[<Test>]
let ``Reporting the number of robots at each position`` () =
    let robots =
        [ { Position = 0, 4; Velocity = 3, -3 }
          { Position = 9, 5; Velocity = -3, -3 }
          { Position = 9, 5; Velocity = 2, 1 } ]

    robots
    |> (fun robots ->
        robots
        |> Seq.fold
            (fun s r ->
                s
                |> Map.change r.Position (function
                    | Some count -> Some(count + 1)
                    | _ -> Some 1))
            Map.empty<int * int, int>)
    |> should
        equal
        Map[((0, 4), 1)
            ((9, 5), 2)]

[<Test>]
let ``Counting robots by quadrant`` () =
    let gridSize = 3, 3

    let robots =
        [ { Position = 0, 2; Velocity = 1, 1 }
          { Position = 1, 1; Velocity = 1, 1 }
          { Position = 2, 0; Velocity = 1, 1 } ]

    (gridSize, robots)
    ||> (fun (width, height) robots ->
        robots |> countByQuadrant (width, height))
    |> should equal (Map [ (NorthEast, 1); (SouthWest, 1) ])

[<Test>]
let ``Solving the example`` () =
    let example =
        "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"

    example
    |> parse
    |> solvePart1 (11, 7) 
    |> should equal 12

[<Test>]
let ``to string`` () =
    let gridSize = 2, 2

    let robots =
        [ { Position = 0, 0; Velocity = 1, 1 }
          { Position = 1, 1; Velocity = 1, 1 } ]

    (gridSize, robots)
    ||> toString
    |> should equal "#.\r\n.#"
