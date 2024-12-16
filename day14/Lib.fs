module Lib
open System
open System.Text.RegularExpressions

module Puzzle = begin
    type Point = (int * int)
    type Speed = Point
    let (++) (x1, y1) (x2, y2) = 
        (x1 + x2, y1 + y2)
    let (<**>) (x, y) (multiplier) = 
        (x * multiplier, y * multiplier)

    let wrap coord limit =
        if coord < 0 then 
            let wrapAmount = -1 * ((abs coord) % limit) in
            (limit + wrapAmount) % limit
        else
            coord % limit

    let (%%) (pointX, pointY) (limitX, limitY) = 
        (wrap pointX limitX, wrap pointY limitY)

    type Robot = {
        location : Point
        velocity : Speed
    }
    with
        static member parse (input : string) =
            let robotRegex = Regex(@"p=(?<px>[0-9\-]+),(?<py>[0-9\-]+) v=(?<vx>[0-9\-]+),(?<vy>[0-9\-]+)", RegexOptions.Compiled) in
            let matchGroups = robotRegex.Match(input).Groups in
            let location = (
                int matchGroups["px"].Value,
                int matchGroups["py"].Value
            ) in
            let velocity = (
                int matchGroups["vx"].Value,
                int matchGroups["vy"].Value
            ) in
            { location = location; velocity = velocity }

        member this.tick limits (seconds : int) : Robot =
            let newLocation = 
                (this.location ++ (this.velocity <**> seconds)) %% limits 
            in
            {this with location = newLocation}

    type Quadrant = 
        | UpperLeft 
        | UpperRight
        | LowerLeft
        | LowerRight
    with
        static member values() =
            [ UpperLeft; UpperRight; LowerLeft; LowerRight ]

    type TileMap = {
        robots: Robot array
        width : int
        height : int
    }
    with 
        member this.tick (seconds : int) =
            let updatedRobots = 
                this.robots
                |> Array.map (fun robot ->
                    robot.tick (this.width, this.height) seconds
                )
            in
            {this with robots = updatedRobots}

        member this.quadrantOf (px, py) =
            let midpointX = this.width / 2 in
            let midpointY = this.height / 2 in
            if (px = midpointX || py = midpointY) then 
                None
            else
                match (px / (midpointX + 1), py / (midpointY + 1)) with
                | (0, 0) -> Some UpperLeft
                | (1, 0) -> Some UpperRight
                | (0, 1) -> Some LowerLeft
                | (1, 1) -> Some LowerRight
                | _ -> 
                    None

        member this.quadrantCounts : Map<Quadrant, int> =
            let mutable counts = 
                Quadrant.values() 
                |> Seq.map (fun q -> (q, 0))
                |> Map.ofSeq
            in
            for robot in this.robots do begin
                this.quadrantOf robot.location
                |> Option.iter (fun q ->
                    counts <- Map.add q (counts[q] + 1) counts
                )
            end;
            printfn "Counts by quadrant: %A" counts;
            counts

        member this.safetyFactor : int =
            this.quadrantCounts
            |> Map.values
            |> Seq.fold (*) 1

        member this.robotsAt (x, y) : int=
            this.robots
            |> Seq.filter (fun robot -> robot.location = (x, y))
            |> Seq.length

        member this.toQuadrantString : string = 
            let lines = seq {
                for y in 0..this.height - 1 do
                    yield new String([|
                        if y = this.height / 2 then
                            yield ' '
                        else
                            for x in 0..this.width - 1 do
                                if x = this.width / 2 then
                                    yield ' '
                                else
                                    yield 
                                        match this.robotsAt (x, y) with
                                        | 0 -> '.'
                                        | n -> (char '0' + char n)
                    |])
            } in 
            String.concat "\n" lines

        override this.ToString() : string = 
            let lines = seq {
                for y in 0..this.height - 1 do
                    yield new String([|
                        for x in 0..this.width - 1 do
                            yield 
                                match this.robotsAt (x, y) with
                                | 0 -> '.'
                                | n -> (char '0' + char n)
                    |])
            } in 
            String.concat "\n" lines

        member this.isShowingPicture : bool =
            // This is not a good way to do this
            Regex(@"\d{8}", RegexOptions.Compiled).IsMatch(this.ToString())

    let part1 (tileMap : TileMap) =
        (tileMap.tick 100)
        |> (fun map -> 
            printfn "%s" (map.toQuadrantString);
            map
        )
        |> _.safetyFactor

    let part2 (tileMap: TileMap) =
        let mutable mapState = tileMap in
        let mutable seconds = 0 in
        while not mapState.isShowingPicture do begin
            mapState <- mapState.tick 1;
            seconds <- seconds + 1;
        end
        seconds
end
