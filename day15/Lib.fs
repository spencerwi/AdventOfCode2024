module Lib
open System

module Puzzle = begin

    type Direction =
        | Up
        | Down
        | Left
        | Right
    with
        static member parse =
            function
            | '^' -> Up
            | 'v' -> Down
            | '<' -> Left
            | '>' -> Right
            | other -> failwith $"Unrecognized direction char: {other}"
            
    type Point = int * int
    let neighbor (x, y) = 
        function
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)

    type GridEntity =
        | Box
        | Wall
        | Robot
        | EmptySpace
    with 
        static member toChar = 
            function
            | Box -> 'O'
            | Wall -> '#'
            | Robot -> '@'
            | EmptySpace -> '.'

    type Grid = {
        mutable boxes : Set<Point>
        walls : Set<Point>
        mutable robot : Point
        width : int
        height : int
    }
    with
        static member parse (input : string array) : Grid =
            let mutable boxes = Set.empty in
            let mutable walls = Set.empty in
            let mutable robot = None in
            for y in 0 .. (input.Length - 1) do begin
                for x in 0 .. (input[y].Length - 1) do begin
                    match input[y][x] with
                    | '#' -> 
                        walls <- Set.add (x, y) walls
                    | 'O' ->
                        boxes <- Set.add (x, y) boxes
                    | '@' ->
                        robot <- Some (x, y)
                    | _ -> ()
                end
            end;
            {
                boxes = boxes
                walls = walls
                robot = Option.get robot
                width = input.Length
                height = input[0].Length
            }

        member this.Item 
            with get (p : Point) : GridEntity =
                if this.robot = p then Robot
                elif this.boxes.Contains p then Box
                elif this.walls.Contains p then Wall
                else EmptySpace

        member this.contains (x, y) : bool =
            x >= 0 && x < this.width &&
            y >= 0 && y < this.height

        member this.tryGet (p : Point) = 
            if not (this.contains p) then None
            else Some this[p]

        override this.ToString() : string =
            seq { 
                for y in 0..this.height - 1 do
                    yield new String([|
                        for x in 0..this.width - 1 do
                            yield GridEntity.toChar this[(x, y)]
                    |])
            } |> String.concat "\n"
                

    let rec move (grid : Grid) (entityLocation : Point) (direction : Direction) = 
        let neighborLocation = (neighbor entityLocation direction) in
        match (grid.tryGet entityLocation, grid.tryGet neighborLocation) with
        | (None, _) -> false // Can't move something outside the grid
        | (Some Wall, _) -> false // Walls don't move
        | (_, None) -> false // Can't move into the abyss!
        | (_, Some Wall) -> false // Nothing can move into a wall
        | (Some Robot, Some EmptySpace) -> 
            grid.robot <- neighborLocation; // Move into an empty space
            true
        | (Some Box, Some EmptySpace) -> 
            grid.boxes <- grid.boxes.Remove entityLocation;
            grid.boxes <- grid.boxes.Add neighborLocation;
            true
        | (Some Robot, Some Box) ->
            if (move grid neighborLocation direction) then
                grid.robot <- neighborLocation;
                true
            else
                false
        | (Some Box, Some Box) ->
            if (move grid neighborLocation direction) then
                grid.boxes <- grid.boxes.Remove entityLocation;
                grid.boxes <- grid.boxes.Add neighborLocation;
                true
            else
                false
            
    let parseInput (input : string array) : Grid * (Direction array) =
        let gridLines = 
            input
            |> Array.takeWhile (not << String.IsNullOrWhiteSpace)
        in
        let movements = 
            input
            |> Array.skipWhile (not << String.IsNullOrWhiteSpace)
            |> Array.skip 1
            |> String.concat ""
            |> _.ToCharArray()
            |> Array.map Direction.parse
        in
        (
            Grid.parse gridLines,
            movements
        )

    let run (grid : Grid) (instructions : Direction array) = 
        for instruction in instructions do begin
            move grid grid.robot instruction |> ignore
        end;
        query {
            for (boxX, boxY) in grid.boxes do
                sumBy ((100 * boxY) + boxX)
        }

    let part1 (input: string array) =
        let grid, instructions = parseInput input in
        run grid instructions

    let part2 (input: string seq) =
        "the right answer"
end
