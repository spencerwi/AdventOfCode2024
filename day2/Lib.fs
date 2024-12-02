module Lib
open System

module Puzzle = begin
    type ToleranceRange = {
        min : int
        max : int
    } with
        member this.contains x =
            this.min <= x && x <= this.max

    type Direction = 
        | Increasing 
        | Decreasing
        | Same

    type SafetyResult =
        | Safe
        | SafeIfOneMistakeAllowed
        | Unsafe

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

        member this.withoutIndex idx : Report =
            {
                levels = List.removeAt idx this.levels
            }

        member this.variations () : Report seq =
            seq {
                for idx in 0 .. (this.levels.Length - 1) do
                    yield this.withoutIndex idx
            }


    let checkSafety (toleranceRange : ToleranceRange) (report: Report) : SafetyResult = 
        let reportIsSafe (report : Report) =
            let steps = seq {
                for (current, next) in Seq.pairwise report.levels do
                    let difference = next - current in
                    let direction = 
                        if difference > 0 then Increasing
                        elif difference < 0 then Decreasing
                        else Same
                    in
                    yield (direction, abs difference)
            }
            let (initialDirection, _) = Seq.head steps in
            let stepIsSafe (stepDirection, stepDifference) = 
                stepDirection = initialDirection &&
                toleranceRange.contains stepDifference
            in
            Seq.forall stepIsSafe steps
        in
        if (reportIsSafe report) then
            Safe
        else
            let isSafeIfOneMistakeIsRemoved =
                report.variations()
                |> Seq.exists (fun variation -> reportIsSafe variation)
            in
            if isSafeIfOneMistakeIsRemoved then
                SafeIfOneMistakeAllowed
            else
                Unsafe

    /// Just for a convenient, readable fold in the solve method
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
