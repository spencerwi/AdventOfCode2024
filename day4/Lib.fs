module Lib

module Puzzle = begin

    let isXmas = 
        function
        | 'X'::'M'::'A'::'S'::_ -> true
        | 'S'::'A'::'X'::'S'::_ -> true
        | _ -> false

    let isMas = 
        function 
        | 'M'::'A'::'S'::_ -> true
        | _ -> false

    type Direction =
        | UP_LEFT
        | UP
        | UP_RIGHT
        | RIGHT
        | DOWN_RIGHT
        | DOWN
        | DOWN_LEFT
        | LEFT
    with 
        static member values = [
            UP_LEFT ; UP ; UP_RIGHT ; RIGHT ; DOWN_RIGHT ; DOWN ; DOWN_LEFT ; LEFT
        ]

    type Point = {
        row : int
        col : int
    } 
        with 
            member this.step = 
                function
                | UP_LEFT -> { row = this.row - 1 ; col = this.col - 1 }
                | UP -> { this with row = this.row - 1 }
                | UP_RIGHT -> { row = this.row - 1 ; col = this.col + 1 }
                | RIGHT -> { this with col = this.col + 1 }
                | DOWN_RIGHT -> { row = this.row + 1 ; col = this.col + 1 }
                | DOWN -> { this with row = this.row + 1 }
                | DOWN_LEFT -> { row = this.row + 1 ; col = this.col - 1 }
                | LEFT -> { this with col = this.col - 1 }

    type Grid = 
        { cells : char[,] }
    with
        static member parse (input : string seq) =
            {
                cells = array2D [|
                    for line in input do
                        yield line.ToCharArray()
                |]
            }

        member this.Item
            with get (point : Point) = this.cells[point.row, point.col]

        member this.height = Array2D.length1 this.cells
        member this.width = Array2D.length2 this.cells

        member this.contains (point : Point) : bool =
            point.row >= 0 && 
            point.row < this.height &&
            point.col >= 0 &&
            point.col < this.width

        member this.castRay (point : Point) (direction : Direction) : char list =
            [
                let mutable current = point
                while this.contains current do
                    yield this[current]
                    current <- current.step direction
            ]

        member this.find (target : char) : seq<Point> =
            seq {
                for row in 0 .. this.height - 1 do
                    for col in 0 .. this.width - 1 do
                        if this.cells[row, col] = target then 
                            yield {row = row; col = col}
            }

        member this.countXmasesFromX (point : Point) =
            if (this[point] <> 'X') then 0
            else
                Direction.values
                |> Seq.map (this.castRay point)
                |> Seq.filter isXmas
                |> Seq.length

        member this.isXShapedMasFromA (point : Point) =
            if (this[point] <> 'A') then false
            else 
                let diagonalMasOptions = [
                    this.castRay (point.step UP_LEFT) DOWN_RIGHT;
                    this.castRay (point.step UP_RIGHT) DOWN_LEFT;
                    this.castRay (point.step DOWN_LEFT) UP_RIGHT;
                    this.castRay (point.step DOWN_RIGHT) UP_LEFT;
                ] in
                let numberOfMases = 
                    diagonalMasOptions
                    |> Seq.filter isMas
                    |> Seq.length
                in
                numberOfMases >= 2

    let part1 (grid : Grid) =
        grid.find 'X'
        |> Seq.map grid.countXmasesFromX
        |> Seq.sum

    let part2 (grid: Grid) =
        grid.find 'A'
        |> Seq.filter grid.isXShapedMasFromA 
        |> Seq.length
end
