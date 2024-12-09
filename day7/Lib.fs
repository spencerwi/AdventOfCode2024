module Lib
open System

module Puzzle = begin

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

            member this.isValid =
                let rec check' = 
                    function
                    | [] -> false
                    | [_] -> false
                    | a::b::rest ->
                        let product = a * b in
                        let sum = a + b in
                        if rest.IsEmpty then
                            (this.testValue = product) || (this.testValue = sum) 
                        else 
                            check' (product::rest) || check' (sum::rest)
                in
                check' this.numbers

            member this.isValidWithConcat =
                let rec check' = 
                    function
                    | [] -> false
                    | [_] -> false
                    | a::b::rest ->
                        let product = a * b in
                        let sum = a + b in
                        let concatenated = int64 (string a + string b) in
                        if rest.IsEmpty then 
                            (this.testValue = product) || 
                            (this.testValue = sum) || 
                            (this.testValue = concatenated)
                        else
                            check' (product::rest) || 
                            check' (sum::rest) || 
                            check' (concatenated::rest)
                in
                check' this.numbers

    let part1 (equations: Equation seq) =
        equations
        |> Seq.filter _.isValid
        |> Seq.sumBy _.testValue


    let part2 (equations: Equation seq) =
        equations
        |> Seq.filter _.isValidWithConcat
        |> Seq.sumBy _.testValue
end
