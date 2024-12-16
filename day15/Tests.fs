module Tests
open System
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        let grid, movements = parseInput sample_input in
        grid.width |> should equal 8;
        grid.height |> should equal 8;
        grid.robot |> should equal (2, 2);
        grid.boxes |> should equivalent [
            (3, 1); (5, 1); 
            (4, 2);
            (4, 3);
            (4, 4);
            (4, 5);
        ];
        grid.walls |> should equivalent (seq {
            for x in 0..7 do
                yield (x, 0)
                yield (x, 7)
            for y in 1..6 do
                yield (0, y)
                yield (7, y)
            yield (1, 2)
            yield (2, 4)
        });
        movements |> should equalSeq [|
            Left; Up; Up; Right; Right; Right;
            Down; Down; Left; Down; Right; Right;
            Down; Left; Left
        |]

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member _.``It should be able to apply moves`` ()=
        let grid, movements = parseInput sample_input in
        move grid grid.robot movements[0] |> ignore;
        grid.ToString()
        |> (fun s -> s.Split("\n", StringSplitOptions.RemoveEmptyEntries))
        |> should equalSeq [|
            "########"
            "#..O.O.#"
            "##@.O..#"
            "#...O..#"
            "#.#.O..#"
            "#...O..#"
            "#......#"
            "########"
        |]

    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 sample_input
        |> should equal 2028

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal "the right answer"
