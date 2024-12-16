module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
Fill me in from the problem description!
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        1 |> should equal 1 // TODO

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 sample_input
        |> should equal "the right answer"

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal "the right answer"
