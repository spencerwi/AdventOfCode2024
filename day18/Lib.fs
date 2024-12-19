module Lib
open System
open System.Collections.Generic
open System.Text.RegularExpressions

module Puzzle = begin
    type Point = int * int
    let (++) (x1, y1) (x2, y2) =
        (x1 + x2, y1 + y2)

    let neighbors (p : Point) = 
        [
            p ++ (0, -1) // up
            p ++ (0, 1) // down
            p ++ (-1, 0) // left
            p ++ (1, 0) // right
        ]

    let pointRegex = Regex(@"(?<x>\d+),(?<y>\d+)", RegexOptions.Compiled)
    let parsePoint (input : string) =
        let groups = pointRegex.Match(input).Groups in
        (int groups["x"].Value, int groups["y"].Value)

    type Grid = {
        mutable walls: Set<Point>
        width : int
        height : int
    }
    with
        static member ofSize (width, height) = 
            {
                walls = Set.empty
                width = width
                height = height
            }

        member this.addWallAt (p : Point) = 
            this.walls <- this.walls.Add p

        member this.hasWallAt (p : Point) =
            this.walls.Contains p

        member this.contains (x, y) : bool =
            x >= 0 && x < this.width &&
            y >= 0 && y < this.height

        member this.walkableNeighborsOf (p : Point) : Point seq =
            neighbors p
            |> Seq.filter this.contains
            |> Seq.filter (not << this.hasWallAt)

        member this.clone() =
            { walls = this.walls; width = this.width; height = this.height }

    let shortestPathToExit (grid : Grid) : int =
        let goal = (grid.width - 1, grid.height - 1) in
        /// Dijkstra's algorithm again
        
        let initialLocation = (0, 0) in
        // We'll keep track of how far each point in the grid is from the source 
        let mutable distancesFromSource = Map.ofList [(initialLocation, 0)] in
        let getDistance location = 
            if distancesFromSource.ContainsKey location then
                distancesFromSource[location]
            else
                Int32.MaxValue
        in

        // Now, we want to walk from the source "outwards" and trace each shortest path. 
        // Sometimes, we'll see a cell again but on a shorter path. No worries, dawg, just 
        // update its distance-from-the-source and go check it out again to see if anything's 
        // changed about it.
        let unsettled = new Queue<Point>([initialLocation]) in
        while unsettled.Count > 0 do
            let current = unsettled.Dequeue() in
            for newLocation in (grid.walkableNeighborsOf current) do
                let existingDistance = getDistance newLocation in
                let distanceThroughCurrent = 
                    (getDistance current) + 1
                in
                if distanceThroughCurrent < existingDistance then
                    distancesFromSource <- distancesFromSource.Add (newLocation, distanceThroughCurrent);
                    unsettled.Enqueue newLocation
            done
        done;
        // Finally, we can answer the question: 
        // How long is the shortest path between start and end?
        distancesFromSource[goal]

    let part1 gridSize (blocks : Point seq) =
        let grid = Grid.ofSize gridSize in
        Seq.iter grid.addWallAt blocks;
        shortestPathToExit grid

    let part2 (input: string seq) =
        "the right answer"
end
