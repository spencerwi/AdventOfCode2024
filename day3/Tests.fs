module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

let sample_input = parseCommands sample_input_raw

[<TestFixture>]
type ``Tests for commands`` ()=
    [<Test>]
    member this.``It should parse commands properly`` ()=
        parseCommands sample_input_raw
        |> should equalSeq [
            Multiply (2, 4);
            DisableMultiply;
            Multiply (5, 5);
            Multiply (11, 8);
            EnableMultiply;
            Multiply (8, 5);
        ]

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        part1 sample_input
        |> should equal 161

    [<Test>]
    member this.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal 48
