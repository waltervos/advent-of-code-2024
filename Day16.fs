namespace AOC2024

type Turn =
    | North'
    | East'
    | South'
    | West'

type Node =
    | Position of int * int
    | Turn of Direction

type Edge = Edge of Node * Node

module Day16 =
    let path =
        let position1 = Position(1, 2)
        let position2 = Position(2, 2)
        let turn = Turn South
        let position3 = Position(2, 3)

        { Nodes =
            Set(
                seq {
                    position1
                    position2
                    turn
                    position3
                }
            )
          Edges =
            Set(
                seq {
                    position1, position2
                    position2, turn
                    turn, position3
                }
            ) }
    let parse puzzle =
        let grid = puzzle |> Library.toGrid'

        let mutable graph = Graph.empty
        for y in 0 .. Array2D.length1 grid - 1 do
            for x in 0 .. Array2D.length2 grid - 1 do
                match grid[y,x] with
                | '.' -> 
                    graph <- graph |> Graph.addNode (Position(x,y))
                | _ -> ()
        
        
        printfn "%A" graph.Nodes
        

    let main =
        (fun () ->
            let day = 16
            Library.getInputForDay day |> parse |> ignore
            "")
