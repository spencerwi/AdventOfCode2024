module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing tests`` ()=
    [<Test>]
    member this.``It should parse the input map properly`` ()=
        BroadcastMap.parse sample_input
        |> should equal {
            width = 12
            height = 12
            antennas = Map.ofList [
                ('0', [(1, 8); (2, 5); (3, 7); (4, 4)])
                ('A', [(5,6); (8, 8); (9, 9)])
            ]
        }

[<TestFixture>]
type ``Tests for solution`` ()=
    let broadcastMap = BroadcastMap.parse sample_input

    [<Test>]
    member _.``It should find antinodes properly`` ()=
        let antinodeSampleMap = 
            BroadcastMap.parse [|
                ".........."
                "...#......"
                "#........."
                "....a....."
                "........a."
                ".....a...."
                "..#......."
                "......#..."
                ".........."
                ".........."
            |]
            in
            antinodeSampleMap.antinodesFor 'a'
            |> should equivalent antinodeSampleMap.antennas['#']
    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 broadcastMap
        |> should equal 14

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal "the right answer"
