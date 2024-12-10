module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing tests`` ()=
    [<Test>]
    member _.``It should parse the input correctly`` ()=
        TopoMap.parse sample_input
        |> should equal { cells = 
                array2D [|
                    [| 8; 9; 0; 1; 0; 1; 2; 3; |]
                    [| 7; 8; 1; 2; 1; 8; 7; 4; |]
                    [| 8; 7; 4; 3; 0; 9; 6; 5; |]
                    [| 9; 6; 5; 4; 9; 8; 7; 4; |]
                    [| 4; 5; 6; 7; 8; 9; 0; 3; |]
                    [| 3; 2; 0; 1; 9; 0; 1; 2; |]
                    [| 0; 1; 3; 2; 9; 8; 0; 1; |]
                    [| 1; 0; 4; 5; 6; 7; 3; 2; |]
                |]
        }

let topoMap = TopoMap.parse sample_input

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member _.``It should solve both parts`` ()=
        solve topoMap 
        |> should equal (36, 81)

