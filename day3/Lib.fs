module Lib
open System.Text.RegularExpressions

module Puzzle = begin

    type MultiplyCommand = MultiplyCommand of int64 * int64
    let eval (MultiplyCommand (a, b)) =
        a * b


    let findValidMultiplyCommands (input : string) : MultiplyCommand seq =
        let multiplyRegex = Regex(@"mul\((?<num1>\d{1,3}),(?<num2>\d{1,3})\)", RegexOptions.Compiled)
        seq {
            for found in (multiplyRegex.Matches input) do
                let num1 = int64 (found.Groups["num1"].Value) in
                let num2 = int64 (found.Groups["num2"].Value) in
                yield MultiplyCommand (num1, num2)
        }


    let part1 (input: string) : int64 =
        findValidMultiplyCommands input
        |> Seq.map eval
        |> Seq.sum

    let part2 (input: string) =
        "the right answer"
end
