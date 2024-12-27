module Day25Tests

open NUnit.Framework
open FsUnit
open AOC2024.Day25

[<Test>]
let ``First example key and lock don't fit`` () =
    let lock = Lock [ 0; 5; 3; 4; 3 ]
    let key = Key [ 5; 0; 2; 1; 3 ]

    (lock, key) |> fits |> should be False

[<Test>]
let ``Second example key and lock do fit`` () =
    let lock = Lock [ 1; 2; 0; 5; 3 ]
    let key = Key [ 4; 3; 4; 0; 2 ]

    (lock, key) |> fits |> should be True

[<Test>]
let ``Solving the example`` () =
    let example =
        "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"

    example
    |> parse
    |> countFits
    |> should equal 3
