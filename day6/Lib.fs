module Lib
open FSharp.Collections.ParallelSeq

module Puzzle = begin
    type Point = int * int
    let (++) ((row1, col1)) ((row2, col2)) : Point =
        (row1 + row2, col1 + col2)

    type Rotation =
        | Left
        | Right

    type Direction = 
        | North
        | East
        | West
        | South
    with 
        member this.turn rotation =
            match (this, rotation) with
            | North, Left | South, Right -> West
            | South, Left | North, Right -> East
            | East, Left  | West, Right  -> North
            | West, Left  | East, Right  -> South

        member this.asMovement = 
            match this with
            | North -> (-1, 0)
            | South -> (1,  0)
            | East  -> (0,  1)
            | West  -> (0, -1)

    type Grid = { 
        obstacles : Set<Point>
        width : int
        height : int
    }
    with
        member this.contains ((row, col) : Point) =
            row >= 0 && row < this.width &&
            col >= 0 && col < this.height

        member this.isEmptySpace p =
            not (this.obstacles.Contains p)

        member this.allPoints() : Point seq =
            seq {
                for row in 0 .. (this.height - 1) do
                    for col in 0 .. (this.width - 1) do
                        yield (row, col)
            }

    type Guard = {
        location : Point
        facing : Direction
    } 
    with
        static member atLocation (p : Point) : Guard =
            {
                location = p
                facing = North
            }
            
        member this.spaceInFront = 
            this.location ++ this.facing.asMovement

        member this.tick (grid : Grid) : Guard =
            if grid.isEmptySpace this.spaceInFront then
                { this with location = this.spaceInFront }
            else
                { this with facing = this.facing.turn Right }

    let parse (input : string seq) =
        let mutable guard = None in
        let mutable obstacles = Set.empty in
        let mutable row = 0 in
        let mutable col = 0 in
        for line in input do begin
            col <- 0
            for c in line do begin
                match c with
                | '^' -> guard <- Some (Guard.atLocation (row, col))
                | '#' -> obstacles <- Set.add (row, col) obstacles
                | _ -> ()
                col <- col + 1
            end
            row <- row + 1
        end;
        let grid = {
            obstacles = obstacles
            width = col
            height = row
        } in
        (Option.get guard, grid)

    let runUntilGuardExit (guard : Guard) (grid : Grid) =
        let rec step' steps guard =
            if not (grid.contains guard.location) then
                steps
            else
                let newGuard = guard.tick grid in
                let newSteps = Set.add guard.location steps in
                step' newSteps newGuard
        in
        step' (Set.singleton guard.location) guard

    let doesCauseLoop (guard : Guard) (grid : Grid) (newObstacleLocation : Point) = 
        let gridWithObstacle = {
            grid with obstacles = Set.add newObstacleLocation grid.obstacles
        } in
        let rec step' (seenStates : Set<Guard>) (guard : Guard) =
            if not (gridWithObstacle.contains guard.location) then
                false
            elif seenStates.Contains guard then
                true
            else
                let newGuard = guard.tick gridWithObstacle in
                let newStates = Set.add guard seenStates in
                step' newStates newGuard
        in
        step' Set.empty guard

    let findLoopCausingObstaclePositions (guard : Guard) (grid : Grid) : Point list =
        grid.allPoints()
        |> Seq.filter ((<>) guard.location)
        |> Seq.filter (not << grid.obstacles.Contains)
        |> PSeq.filter (doesCauseLoop guard grid)
        |> PSeq.toList

    let part1 (guard : Guard) (grid : Grid) =
        runUntilGuardExit guard grid
        |> Set.count 

    let part2 (guard : Guard) (grid : Grid) =
        findLoopCausingObstaclePositions guard grid
        |> Seq.length
end
