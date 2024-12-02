module Lib
open System

module Puzzle = begin
    type ToleranceRange = {
        min : int
        max : int
    } with
        member this.allows x =
            this.min <= x && x <= this.max

    type Direction = 
        | Increasing 
        | Decreasing
        | Same
    with 
        static member ofDiff (difference : int) : Direction =
            if difference > 0 then Increasing
            elif difference < 0 then Decreasing
            else Same

    type Report = {
        levels : int list
    } with 
        static member parse(input : string) : Report =
            let levels = 
                input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                |> Seq.map int
                |> List.ofSeq
            in
            { levels = levels }

        /// <returns>
        /// A new <c>Report</c> that's a copy of this one with the level reading
        ///  at <c>idx</c> removed
        /// </returns>
        member this.withoutIndex idx : Report =
            { levels = List.removeAt idx this.levels }

        /// <returns>
        /// A <c>Seq</c> containing all possible variations of this 
        ///  <c>Report</c> if you were to remove one single level reading
        /// </returns>
        member this.variations () : Report seq =
            seq {
                for idx in 0 .. (this.levels.Length - 1) do
                    yield this.withoutIndex idx
            }

        member this.isMonotonic() : bool =
            let directionOfFirstStep = Direction.ofDiff (this.levels[1] - this.levels[0]) in
            this.levels
            |> Seq.pairwise
            |> Seq.forall (fun (current, next) -> 
                let directionOfThisStep = (Direction.ofDiff (next - current)) in
                directionOfThisStep = directionOfFirstStep
            )

        member this.stepsAreTolerable (toleranceRange : ToleranceRange) : bool =
            this.levels
            |> Seq.pairwise
            |> Seq.forall (fun (current, next) ->
                let difference = abs (next - current) in
                toleranceRange.allows difference
            )

    type SafetyResult =
        | Safe
        | SafeIfOneMistakeAllowed
        | Unsafe

    let checkSafety (toleranceRange : ToleranceRange) (report: Report) : SafetyResult = 
        let isSafe (report : Report) =
            report.isMonotonic() && report.stepsAreTolerable toleranceRange
        in
        if (isSafe report) then
            Safe
        elif Seq.exists isSafe (report.variations()) then
            SafeIfOneMistakeAllowed
        else
            Unsafe

    /// <summary>
    /// Just for a convenient, readable fold in the solve method
    /// </summary>
    type PuzzleCounter = {
        part1 : int
        part2 : int
    }
        with 
            static member empty = 
                { part1 = 0; part2 = 0 }

            static member update counter = 
                function 
                | Safe -> 
                    { 
                        part1 = counter.part1 + 1
                        part2 = counter.part2 + 1 
                    }
                | SafeIfOneMistakeAllowed -> 
                    { counter with part2 = counter.part2 + 1 }                
                | Unsafe -> counter

    let solve (toleranceRange : ToleranceRange) (reports : Report seq) =
        reports 
        |> Seq.map (checkSafety toleranceRange)
        |> Seq.fold PuzzleCounter.update (PuzzleCounter.empty)
end
