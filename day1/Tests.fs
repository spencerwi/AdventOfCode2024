module Tests
open Lib

open FsUnit
open NUnit.Framework

let sample_input_raw = """
3   4
4   3
2   5
1   3
3   9
3   3
"""

let sample_input = sample_input_raw.Trim().Split "\n"
let parsed_sample_input = (
    [3;4;2;1;3;3],
    [4;3;5;3;9;3]
)

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should parse input properly`` ()=
        Puzzle.parseLists sample_input
        |> should equal parsed_sample_input

    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 (fst parsed_sample_input) (snd parsed_sample_input)
        |> should equal 11

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 (fst parsed_sample_input) (snd parsed_sample_input)
        |> should equal 31
