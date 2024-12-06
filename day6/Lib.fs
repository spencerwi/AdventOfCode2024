module Lib
open FSharp.Collections.ParallelSeq

module Puzzle = begin
    type Point = int * int
    let (++) (row1, col1) (row2, col2) : Point =
        (row1 + row2, col1 + col2)

    type Direction = 
        | North
        | East
        | West
        | South
    with 
        member this.turn() =
            match this with
            | South -> West
            | North -> East
            | West  -> North
            | East  -> South

        member this.asMovement() = 
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
            this.location ++ this.facing.asMovement()

        member this.spaceToTheRight =
            this.location ++ this.facing.turn().asMovement()

        member this.tick (grid : Grid) : Guard =
            if grid.isEmptySpace this.spaceInFront then
                { this with location = this.spaceInFront }
            else
                { 
                    location = this.spaceToTheRight
                    facing = this.facing.turn()
                }

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

    type RunResult = 
        | Exited of Set<Point>
        | Looped

    let run (guard : Guard) (grid : Grid) =
        let rec step' (seenStates : Set<Guard>) guard =
            if not (grid.contains guard.location) then
                let uniqueSteps = 
                    seenStates
                    |> Set.map _.location
                Exited uniqueSteps
            elif seenStates.Contains guard then
                Looped
            else
                let newGuard = guard.tick grid in
                let newSeenStates = Set.add guard seenStates in
                step' newSeenStates newGuard
        in
        step' Set.empty guard

    let doesCauseLoop (guard : Guard) (grid : Grid) (newObstacleLocation : Point) = 
        let gridWithObstacle = {
            grid with obstacles = Set.add newObstacleLocation grid.obstacles
        } in
        match run guard gridWithObstacle with
        | Looped -> true
        | _ -> false

    let solve (guard : Guard) (grid : Grid) =
        let (Exited normalGuardPath) = run guard grid in
        let part1 = Set.count normalGuardPath in
        // An obstacle placed in a spot the guard will never see doesn't affect the guard's path.
        // That means we can cut down the number of points to check by only checking the guard's actual path.
        let loopCausingPositions = 
            normalGuardPath
            |> Seq.filter ((<>) guard.location) // don't check the start position
            |> PSeq.filter (doesCauseLoop guard grid)
        in
        let part2 = PSeq.length loopCausingPositions in
        (part1, part2)


end
