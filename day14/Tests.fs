module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing tests`` ()=
    [<Test>]
    member _.``It should parse robot locations correctly`` ()=
        sample_input
        |> Array.map Robot.parse
        |> should equalSeq [|
            {location = (0,4); velocity = (3,-3)}
            {location = (6,3); velocity = (-1, -3)}
            {location = (10,3); velocity = (-1, 2)}
            {location = (2,0); velocity = (2, -1)}
            {location = (0,0); velocity = (1, 3)}
            {location = (3,0); velocity = (-2, -2)}
            {location = (7,6); velocity = (-1, -3)}
            {location = (3,0); velocity = (-1, -2)}
            {location = (9,3); velocity = (2, 3)}
            {location = (7,3); velocity = (-1, 2)}
            {location = (2,4); velocity = (2, -3)}
            {location = (9,5); velocity = (-3, -3)}
        |]

let robots = Array.map Robot.parse sample_input 
let tileMap = {
    robots = robots
    width = 11
    height = 7
}

[<TestFixture>]
type ``Tests for robots`` ()=
    [<Test>]
    member _.``It should tick properly`` ()=
        let exampleRobot = {location = (2,4); velocity = (2, -3)} in
        let limits = (tileMap.width, tileMap.height) in
        exampleRobot.tick limits 1
        |> _.location
        |> should equal (4, 1);
        exampleRobot.tick limits 2
        |> _.location
        |> should equal (6, 5)
        exampleRobot.tick limits 3
        |> _.location
        |> should equal (8, 2)

[<TestFixture>]
type ``Tests for TileMaps`` ()=
    [<Test>]
    member _.``It should determine the quadrant for a point correctly`` ()=
        let cornerPoints = [
            (0,0) 
            (0, tileMap.height - 1)
            (tileMap.width - 1, 0)
            (tileMap.width - 1, tileMap.height - 1)
        ] in
        let expectedQuadrants = [
            Some UpperLeft
            Some LowerLeft
            Some UpperRight
            Some LowerRight
        ] in
        cornerPoints
        |> Seq.map tileMap.quadrantOf
        |> should equalSeq expectedQuadrants
        let middlePoints = [
            (0, tileMap.height / 2)
            (tileMap.width / 2, 0)
            (tileMap.width / 2, tileMap.height / 2)
        ] in
        middlePoints
        |> Seq.map tileMap.quadrantOf
        |> should equalSeq [None; None; None]


[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 tileMap
        |> should equal 12
