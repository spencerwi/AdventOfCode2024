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
        | 'S'::'A'::'M'::_ -> true
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
            member this.apply ((rowDiff, colDiff)) =
                {row = this.row + rowDiff; col = this.col + colDiff}

            member this.step direction = 
                let diff = 
                    match direction with
                    | UP_LEFT       -> (-1, -1)
                    | UP            -> (-1,  0)
                    | UP_RIGHT      -> (-1,  1)
                    | RIGHT         -> ( 0,  1)
                    | DOWN_RIGHT    -> ( 1,  1)
                    | DOWN          -> ( 1,  0)
                    | DOWN_LEFT     -> ( 1, -1)
                    | LEFT          -> ( 0, -1)
                in
                this.apply diff

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
        member this.allPoints = seq {
            for row in 0..(this.height - 1) do
                for col in 0..(this.width - 1) do
                    yield {row = row; col = col}
        }

        member this.contains (point : Point) : bool =
            point.row >= 0 && 
            point.row < this.height &&
            point.col >= 0 &&
            point.col < this.width

        member this.castRay (length : int) (point : Point) (direction : Direction) : char list =
            [
                let mutable current = point in
                let mutable stepCount = 1 in
                while this.contains current && stepCount <= length do
                    yield this[current]
                    current <- current.step direction
                    stepCount <- stepCount + 1
            ]

        member this.find (target : char) : seq<Point> =
            this.allPoints
            |> Seq.filter (fun point -> this[point] = target)

        member this.countXmasesFromX (point : Point) =
            if (this[point] <> 'X') then 0
            else
                Direction.values
                |> Seq.map (this.castRay 4 point)
                |> Seq.filter isXmas
                |> Seq.length

        member this.isXShapedMasFromA (point : Point) =
            if (this[point] <> 'A') then false
            else 
                query {
                    let raysToCast = [ 
                        ((point.step UP_LEFT), DOWN_RIGHT);
                        ((point.step UP_RIGHT), DOWN_LEFT)
                    ] in
                    for (origin, direction) in raysToCast do
                    let ray = (this.castRay 3 origin direction) in
                    all (isMas ray)
                }

    let part1 (grid : Grid) =
        grid.find 'X'
        |> Seq.map grid.countXmasesFromX
        |> Seq.sum

    let part2 (grid: Grid) =
        grid.find 'A'
        |> Seq.filter grid.isXShapedMasFromA 
        |> Seq.length
end
