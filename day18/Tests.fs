module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        1 |> should equal 1 // TODO

[<TestFixture>]
type ``Tests for solution`` ()=
    let blocks = sample_input |> Seq.map parsePoint
    [<Test>]
    member _.``It should solve part 1`` ()=
        let first12Blocks = 
            blocks
            |> Seq.take 12
        in
        part1 (7, 7) first12Blocks
        |> should equal 22

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal "the right answer"
