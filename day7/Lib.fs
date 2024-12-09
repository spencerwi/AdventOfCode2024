module Lib
open System

module Puzzle = begin

    type OperationSet =
        | Part1 
        | Part2 
    with 
        member this.apply a b =
            match this with
            | Part1 -> [ a + b; a * b ]
            | Part2 -> [ a + b; a * b; int64 (string a + string b)]

    type Equation = {
        testValue : int64
        numbers : int64 list
    }
        with 
            static member parse (input : string) =
                let testValueStr::numbersStrs = 
                    input.Split([|':'; ' '|], StringSplitOptions.RemoveEmptyEntries)
                    |> List.ofArray
                in
                {
                    testValue = int64 testValueStr
                    numbers = List.map int64 numbersStrs 
                }

            member this.isValid (ops : OperationSet) =
                let rec check' = 
                    function
                    | [] -> false
                    | [_] -> false
                    | a::b::rest ->
                        let candidates = ops.apply a b in
                        if rest.IsEmpty then
                            candidates
                            |> Seq.contains this.testValue
                        else 
                            candidates 
                            |> Seq.map (fun c -> c::rest)
                            |> Seq.exists check'
                in
                check' this.numbers

    let part1 (equations: Equation seq) =
        equations
        |> Seq.filter (fun e -> e.isValid Part1)
        |> Seq.sumBy _.testValue


    let part2 (equations: Equation seq) =
        equations
        |> Seq.filter (fun e -> e.isValid Part2)
        |> Seq.sumBy _.testValue
end
