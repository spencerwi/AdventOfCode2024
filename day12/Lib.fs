module Lib
open System.Collections.Generic

module Puzzle = begin
    type Point = {
        row : int
        col : int
    }
    with
        static member make (row, col) = { row = row; col = col }
        member this.neighbors() : Point seq =
            [
                {this with row = this.row - 1} // up
                {this with col = this.col - 1} // left
                {this with col = this.col + 1} // right
                {this with row = this.row + 1} // down
            ]

    type Region = {
        mutable points : Set<Point>
    }
    with
        static member empty = { 
            points = Set.empty 
        }
        member this.contains = this.points.Contains
        member this.add p = this.points <- this.points.Add p

        member this.neighborsOf (p : Point) = 
            p.neighbors()
            |> Seq.filter (this.contains)
        member this.area() = int64 this.points.Count

        member this.perimeter() : int64 =
            this.points
            |> Seq.sumBy (fun point -> 
                point.neighbors()
                |> Seq.filter (not << this.contains)
                |> Seq.length
            )
            |> int64

        member this.price() = this.area() * this.perimeter()

    type GardenPlan = {
        cells : char[,]
    }
    with
        static member parse (input : string array) : GardenPlan =
            let grid = 
                array2D [|
                    for line in input do
                        yield line.ToCharArray()
                |]
            in { cells = grid }

        member this.Item
            with get (p : Point) = this.cells[p.row, p.col]

        member this.height = Array2D.length1 this.cells
        member this.width = Array2D.length2 this.cells

        member this.contains (p : Point) =
            p.row >= 0 && p.row < this.height &&
            p.col >= 0 && p.col < this.width

        member this.neighborsOf (p : Point) : Point seq =
            p.neighbors()
            |> Seq.filter this.contains

        member this.regionForPoint (p : Point) : Region =
            let targetChar = this[p] in
            let regionSoFar = Region.empty in
            let mutable visitQueue = new Queue<Point>() in
            visitQueue.Enqueue p;
            while visitQueue.Count > 0 do begin
                let currentPosition = visitQueue.Dequeue() in
                if this[currentPosition] = targetChar && not (regionSoFar.contains currentPosition) then
                    regionSoFar.add currentPosition;
                    this.neighborsOf currentPosition
                    |> Seq.iter visitQueue.Enqueue
            end
            regionSoFar

        member this.allRegions() : Set<Region> =
            let mutable regions : Set<Region> = Set.empty in
            for row in 0 .. this.height - 1 do begin
                for col in 0 .. this.width - 1 do begin
                    let currentPoint = Point.make (row, col) in
                    let alreadySeen =
                        regions
                        |> Set.exists (fun r -> r.contains currentPoint)
                    in
                    if not alreadySeen then
                        let thisRegion = (this.regionForPoint currentPoint) in
                        regions <- regions.Add thisRegion
                end
            end;
            regions

        member this.totalPrice() : int64 =
            this.allRegions()
            |> Seq.sumBy _.price()

    let part1 (garden: GardenPlan) =
        garden.totalPrice()

    let part2 (input: string seq) =
        "the right answer"
end
