module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>] 
    member _.``It should parse the grid correctly`` ()=
        let grid = Grid.parse sample_input in
        grid |> should equal { 
            cells = array2D [|
                [| 'M';'M';'M';'S';'X';'X';'M';'A';'S';'M' |];
                [| 'M';'S';'A';'M';'X';'M';'S';'M';'S';'A' |];
                [| 'A';'M';'X';'S';'X';'M';'A';'A';'M';'M' |];
                [| 'M';'S';'A';'M';'A';'S';'M';'S';'M';'X' |];
                [| 'X';'M';'A';'S';'A';'M';'X';'A';'M';'M' |];
                [| 'X';'X';'A';'M';'M';'X';'X';'A';'M';'A' |];
                [| 'S';'M';'S';'M';'S';'A';'S';'X';'S';'S' |];
                [| 'S';'A';'X';'A';'M';'A';'S';'A';'A';'A' |];
                [| 'M';'A';'M';'M';'M';'X';'M';'M';'M';'M' |];
                [| 'M';'X';'M';'X';'A';'X';'M';'A';'S';'X' |];
            |]
        }

    [<Test>]
    member _.``It should check array boundaries correctly`` ()=
        let grid = Grid.parse sample_input in
        let testCases = [
            {row = 0; col = 0};
            {row = -1; col = 0};
            {row = 0; col = -1};
            {row = 10; col = 0};
            {row = 0; col = 10}
        ] in
        testCases
        |> Seq.map grid.contains
        |> should equal [
            true;
            false;
            false;
            false;
            false
        ]

    [<Test>]
    member _.``It should cast rays from a point correctly`` ()=
        let grid = Grid.parse sample_input in
        let topLeftCorner = {row = 0; col = 0} in
        let bottomRightCorner = {row = grid.height - 1; col = grid.width - 1} in
        grid.castRay 100 topLeftCorner UP
        |> should equalSeq [grid[topLeftCorner]];
        grid.castRay 100 topLeftCorner UP_LEFT
        |> should equalSeq [grid[topLeftCorner]];
        grid.castRay 100 topLeftCorner LEFT
        |> should equalSeq [grid[topLeftCorner]];
        let firstRow = List.ofArray (grid.cells[0, 0..]) in
        grid.castRay 100 topLeftCorner RIGHT
        |> should equalSeq firstRow;
        grid.castRay 100 topLeftCorner DOWN_RIGHT 
        |> should equalSeq (
            grid.castRay 100 bottomRightCorner UP_LEFT
            |> List.rev
        );
        grid.castRay 3 topLeftCorner DOWN_RIGHT 
        |> should equalSeq [
            grid[topLeftCorner]; 
            grid[topLeftCorner.step DOWN_RIGHT];
            grid[(topLeftCorner.step DOWN_RIGHT).step DOWN_RIGHT]
        ]


    [<Test>]
    member _.``It should solve part 1`` ()=
        let grid = Grid.parse sample_input in
        part1 grid
        |> should equal 18

    [<Test>]
    member _.``It should solve part 2`` ()=
        let grid = Grid.parse sample_input in
        part2 grid
        |> should equal 9
