module Lib
open System.Text.RegularExpressions

module Puzzle = begin

    type State =
        {
            sum : int64
            multiplyEnabled : bool
        }
        with 
            static member initial = 
                { sum = 0; multiplyEnabled = true }

    type Command = 
        | Multiply of int64 * int64
        | DisableMultiply 
        | EnableMultiply

    let eval (state : State) =
        function
        | Multiply (a, b) when state.multiplyEnabled -> 
            {state with sum = state.sum + (a * b)}
        | DisableMultiply when state.multiplyEnabled = true -> 
            {state with multiplyEnabled = false }
        | EnableMultiply when state.multiplyEnabled = false -> 
            {state with multiplyEnabled = true}
        | _ -> state

    let evalOnlyMultiply (state : State) =
        function 
        | Multiply (a, b) -> 
            {state with sum = state.sum + (a * b)}
        | _ -> state


    let parseCommands (input : string) : Command seq =
        let disableRegex = @"don't\(\)" in 
        let enableRegex = @"do\(\)" in
        let multiplyRegex = @"mul\((?<num1>\d{1,3}),(?<num2>\d{1,3})\)" in
        let commandRegex = Regex(disableRegex + "|" + enableRegex + "|" + multiplyRegex, RegexOptions.Compiled) in
        seq {
            for found in (commandRegex.Matches input) do
                if found.Value.StartsWith "mul" then
                    let num1 = int64 (found.Groups["num1"].Value) in
                    let num2 = int64 (found.Groups["num2"].Value) in
                    yield Multiply (num1, num2)
                elif found.Value = "don't()" then
                    yield DisableMultiply
                elif found.Value = "do()" then
                    yield EnableMultiply
        }

    let part1 (input : Command seq) : int64 =
        input
        |> Seq.fold evalOnlyMultiply State.initial
        |> _.sum

    let part2 (input: Command seq) : int64 =
        input
        |> Seq.fold eval State.initial
        |> _.sum
end
