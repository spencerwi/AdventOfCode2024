module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing tests`` ()=
    [<Test>]
    member _.``It should parse input properly`` ()=
        let expectedGuard = {
            location = (6, 4);
            facing = North
        } in
        let expectedGrid = {
            obstacles = Set.ofList [
                (0, 4);
                (1, 9);
                (3, 2);
                (4, 7);
                (6, 1);
                (7, 8);
                (8, 0);
                (9, 6)
            ];
            width = 10;
            height = 10
        } in 
        parse sample_input
        |> should equal (expectedGuard, expectedGrid)

[<TestFixture>]
type ``Guard tests`` ()=
    let (guard, grid) = parse sample_input in

    [<Test>]
    member _.``It should step forward into an empty space`` ()=
        guard.tick grid
        |> should equal {
            location = (5, 4);
            facing = North
        }

    [<Test>]
    member _.``It should turn and continue in the new direction when it comes up to a wall ``() =
        // Run the guard up into the wall and confirm it turns
        let mutable current = guard in
        for _ in 1..6 do begin
            current <- current.tick grid
        end
        current |> should equal {
            location = (1, 5);
            facing = East
        }

[<TestFixture>]
type ``Tests for solver methods`` ()=
    let (guard, grid) = parse sample_input in

    [<Test>]
    member _.``It should solve both parts`` ()=
        solve guard grid
        |> should equal (41, 6)
