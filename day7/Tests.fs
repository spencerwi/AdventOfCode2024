module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing tests`` ()=
    [<Test>]
    member _.``It should parse input correctly`` ()=
        sample_input
        |> Seq.map Equation.parse
        |> should equalSeq [
            { testValue = 190; numbers = [10; 19] }
            { testValue = 3267; numbers = [81; 40; 27] }
            { testValue = 83; numbers = [17; 5] }
            { testValue = 156; numbers = [15; 6] }
            { testValue = 7290; numbers = [6; 8; 6; 15] }
            { testValue = 161011; numbers = [16; 10; 13] }
            { testValue = 192; numbers = [17; 8; 14] }
            { testValue = 21037; numbers = [9; 7; 18; 13] }
            { testValue = 292; numbers = [11; 6; 16; 20] }
        ]

[<TestFixture>]
type ``Tests for solution`` ()=
    let equations = Seq.map Equation.parse sample_input

    [<Test>]
    member _.``It can check equations properly without concatenation`` ()=
        equations
        |> Seq.filter _.isValid
        |> should equalSeq [
            { testValue = 190; numbers = [10; 19] }
            { testValue = 3267; numbers = [81; 40; 27] }
            { testValue = 292; numbers = [11; 6; 16; 20] }
        ]

    [<Test>]
    member _.``It can check equations properly with concatenation`` ()=
        equations
        |> Seq.filter _.isValidWithConcat
        |> should equalSeq [
            { testValue = 190; numbers = [10; 19] }
            { testValue = 3267; numbers = [81; 40; 27] }
            { testValue = 156; numbers = [15; 6] }
            { testValue = 7290; numbers = [6; 8; 6; 15] }
            { testValue = 192; numbers = [17; 8; 14] }
            { testValue = 292; numbers = [11; 6; 16; 20] }
        ]

    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 equations
        |> should equal 3749

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 equations
        |> should equal 11387
