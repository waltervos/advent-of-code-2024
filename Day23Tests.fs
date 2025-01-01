module Day23Tests

open NUnit.Framework
open FsUnit
open AOC2024.Day23
open AOC2024

let example =
    "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"

[<Test>]
let ``Solving the example`` () =
    let connections = example |> parse

    connections
    |> findConnections startingWithT
    |> should
        equal
        [ "co,de,ta"
          "co,ka,ta"
          "de,ka,ta"
          "qp,td,wh"
          "tb,vc,wq"
          "tc,td,wh"
          "td,wh,yn" ]

[<Test>]
let ``Solving the example, part 2`` () =
    let connections = example |> parse

    connections |> findConnections' |> should equal ["co,de,ka,ta"]

[<Test>]
let ``Finding co-ka-ta`` () =
    "
ka-co
ta-co
ta-ka"
    |> parse
    |> findConnections startingWithT
    |> should equal [ "co,ka,ta" ]

[<Test>]
let ``Finding td-wh-yn`` () =
    "
wh-td
wh-yn
td-yn"
    |> parse
    |> findConnections startingWithT
    |> should equal [ "td,wh,yn" ]
