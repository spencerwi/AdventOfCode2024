module Lib

module Puzzle = begin
    type Point = {
        row : int
        col : int
    }
    with
        member this.neighbors() =
            seq {
                { this with row = this.row - 1 } // Up
                { this with col = this.col - 1 } // Left
                { this with col = this.col + 1 } // Right
                { this with row = this.row + 1 } // Down
            }


    type TopoMap = {
        cells : int[,]
    }
    with 
        static member parse (input : string seq) =
            { 
                cells = 
                    array2D [|
                        for line in input do
                            yield 
                                line.ToCharArray()
                                |> Array.map (int << string)
                    |]
                }

        member this.Item 
            with get (point) = this.cells[point.row, point.col]

        member this.width = Array2D.length1 this.cells
        member this.height = Array2D.length2 this.cells

        member this.contains (point : Point) = 
            point.row >= 0 && point.row < this.height &&
            point.col >= 0 && point.col < this.width

        member this.neighborsOf(point : Point) : Point seq =
            point.neighbors()
            |> Seq.filter this.contains

        member this.trailheads : Point seq =
            seq {
                for row in 0 .. this.height - 1 do
                    for col in 0 .. this.width - 1 do
                        if this.cells[row, col] = 0 then
                            yield {row = row; col = col}
            }

        member this.walk (trailhead : Point) (onCompletion : Point list -> Point  -> unit) =
            let rec walk' (tracedSteps : Point list) (currentStep : Point) =
                let updatedPath = currentStep :: tracedSteps in
                if this[currentStep] = 9 then
                    onCompletion (List.rev updatedPath) currentStep
                else
                    let availableNextSteps =
                        this.neighborsOf currentStep
                        |> Seq.filter (fun next -> not <| List.contains next tracedSteps)
                        |> Seq.filter (fun next -> this[next] = this[currentStep] + 1)
                    in
                    availableNextSteps
                    |> Seq.iter (walk' updatedPath)
            in
            walk' [] trailhead


    let solve (input: TopoMap) =
        input.trailheads
        |> Seq.map (fun trailhead -> 
            let mutable seenPaths = Set.empty in
            let mutable seen9s = Set.empty in
            input.walk trailhead (fun path destination -> 
                seenPaths <- Set.add path seenPaths;
                seen9s <- Set.add destination seen9s
            )
            (seen9s.Count, seenPaths.Count)
        )
        |> Seq.fold (fun (scoresSum, ratingsSum) (score, rating) -> 
            (scoresSum + score, ratingsSum + rating)
        ) (0, 0)

end
