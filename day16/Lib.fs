module Lib
open System
open System.Collections.Generic

module Puzzle = begin

    type Direction =
        | North
        | South
        | West
        | East
    with
        static member parse =
            function
            | '^' -> North
            | 'v' -> South
            | '<' -> West
            | '>' -> East
            | other -> failwith $"Unrecognized direction char: {other}"

        static member values =
            [ North ; South; West; East ]

        member this.turnsToReach other =
            match this, other with
            | (_, _) when this = other -> 0
            | (North, South) | (South, North) -> 2
            | (West, East) | (East, West) -> 2
            | (_, _) -> 1
            
    type Point = int * int
    let neighbor (x, y) = 
        function
        | North -> (x, y - 1)
        | South -> (x, y + 1)
        | West -> (x - 1, y)
        | East -> (x + 1, y)

    let neighborsOf (point : Point) =
        seq {
            (North, neighbor point North)
            (West, neighbor point West)
            (East, neighbor point East)
            (South, neighbor point South)
        }


    type GridEntity =
        | Box
        | Wall
        | Start
        | Goal
        | EmptySpace
    with 
        static member toChar = 
            function
            | Box -> 'O'
            | Wall -> '#'
            | Start -> 'S'
            | Goal -> 'E'
            | EmptySpace -> '.'


    type Grid = {
        walls : Set<Point>
        start : Point
        goal : Point
        width : int
        height : int
    }
    with
        static member parse (input : string array) : Grid =
            let mutable walls = Set.empty in
            let mutable start = None in
            let mutable goal = None in
            for y in 0 .. (input.Length - 1) do begin
                for x in 0 .. (input[y].Length - 1) do begin
                    match input[y][x] with
                    | '#' -> 
                        walls <- Set.add (x, y) walls
                    | 'S' ->
                        start <- Some (x, y) 
                    | 'E' ->
                        goal <- Some (x, y)
                    | _ -> ()
                end
            end;
            {
                walls = walls
                start = Option.get start
                goal = Option.get goal
                width = input.Length
                height = input[0].Length
            }

        member this.Item 
            with get (p : Point) : GridEntity =
                if this.start = p then Start
                elif this.goal = p then Goal
                elif this.walls.Contains p then Wall
                else EmptySpace

        member this.contains (x, y) : bool =
            x >= 0 && x < this.width &&
            y >= 0 && y < this.height

        member this.tryGet (p : Point) = 
            if not (this.contains p) then None
            else Some this[p]

        member this.isWalkableSpace (p : Point) = 
            match this.tryGet p with
            | None | Some Wall -> false 
            | _ -> true

        override this.ToString() : string =
            seq { 
                for y in 0..this.height - 1 do
                    yield new String([|
                        for x in 0..this.width - 1 do
                            yield GridEntity.toChar this[(x, y)]
                    |])
            } |> String.concat "\n"

    type WalkState = {
        location : Point
        facing : Direction
    }
    with
        static member initialAt location =
            { location = location; facing = East }

        member this.availableNextSteps (grid : Grid) =
            neighborsOf this.location
            |> Seq.choose (fun (dir, newLocation) -> 
                if grid.isWalkableSpace newLocation then
                    let turnCount = int64 <| this.facing.turnsToReach dir in
                    let cost = (turnCount * 1000L) + 1L in
                    let newState = {location = newLocation; facing = dir} in
                    Some (cost, newState)
                else
                    None
            )
            
    /// <description>
    /// Dijkstra's algorithm, recycled from AoC 2022 day 12 and adapted, because why figure this out from scratch again?
    /// </description>
    let bestPath grid  =
        // Let's dijkstra this business, yo
        
        let initialState = WalkState.initialAt grid.start in
        // We'll keep track of how far each point in the grid is from the source by using a separate grid of the same size
        let mutable distancesFromSource = Map.ofList [(initialState, 0L)] in
        let getDistance state = 
            if distancesFromSource.ContainsKey state then
                distancesFromSource[state]
            else
                Int64.MaxValue
        in

        // Now, we want to walk from the source "outwards" and trace each shortest path. 
        // Sometimes, we'll see a cell again but on a shorter path. No worries, dawg, just 
        // update its distance-from-the-source and go check it out again to see if anything's 
        // changed about it.
        let unsettled = new Queue<WalkState>([initialState]) in
        while unsettled.Count > 0 do
            let current = unsettled.Dequeue() in
            for (cost, newState) in (current.availableNextSteps grid) do
                let existingDistance = getDistance newState in
                let distanceThroughCurrent = 
                    (getDistance current) + cost
                in
                if distanceThroughCurrent < existingDistance then
                    distancesFromSource <-distancesFromSource.Add (newState, distanceThroughCurrent);
                    unsettled.Enqueue newState
            done
        done;
        // Finally, we can answer the question: 
        // How long is the shortest path between start and end?
        distancesFromSource.Keys
        |> Seq.filter (fun state -> state.location = grid.goal)
        |> Seq.map (fun state -> distancesFromSource[state])
        |> Seq.min
                
    let part1 (input: string array) =
        let grid = Grid.parse input in
        bestPath grid 

    let part2 (input: string seq) =
        "the right answer"
end
