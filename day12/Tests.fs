module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Tests for GardenPlan`` ()=
    [<Test>]
    member _.``It should parse correctly`` ()=
        GardenPlan.parse sample_input 
        |> should equal {
            cells = array2D [|
                [| 'R'; 'R'; 'R'; 'R'; 'I'; 'I'; 'C'; 'C'; 'F'; 'F' |]
                [| 'R'; 'R'; 'R'; 'R'; 'I'; 'I'; 'C'; 'C'; 'C'; 'F' |]
                [| 'V'; 'V'; 'R'; 'R'; 'R'; 'C'; 'C'; 'F'; 'F'; 'F' |]
                [| 'V'; 'V'; 'R'; 'C'; 'C'; 'C'; 'J'; 'F'; 'F'; 'F' |]
                [| 'V'; 'V'; 'V'; 'V'; 'C'; 'J'; 'J'; 'C'; 'F'; 'E' |]
                [| 'V'; 'V'; 'I'; 'V'; 'C'; 'C'; 'J'; 'J'; 'E'; 'E' |]
                [| 'V'; 'V'; 'I'; 'I'; 'I'; 'C'; 'J'; 'J'; 'E'; 'E' |]
                [| 'M'; 'I'; 'I'; 'I'; 'I'; 'I'; 'J'; 'J'; 'E'; 'E' |]
                [| 'M'; 'I'; 'I'; 'I'; 'S'; 'I'; 'J'; 'E'; 'E'; 'E' |]
                [| 'M'; 'M'; 'M'; 'I'; 'S'; 'S'; 'J'; 'E'; 'E'; 'E' |]
            |]
        }

    [<Test>]
    member _.``It should identify a region correctly`` ()=
        let gardenPlan = GardenPlan.parse sample_input in
        gardenPlan.regionForPoint (Point.make (0, 0))
        |> _.points
        |> should equivalent (Set.ofList [
            {row = 0; col = 0}; {row = 0; col = 1}; {row = 0; col = 2}; {row = 0; col = 3}
            {row = 1; col = 0}; {row = 1; col = 1}; {row = 1; col = 2}; {row = 1; col = 3}
            {row = 2; col = 2}; {row = 2; col = 3}; {row = 2; col = 4}
            {row = 3; col = 2}
        ])

let gardenPlan = GardenPlan.parse sample_input

[<TestFixture>]
type ``Tests for Region`` ()=
    let region = gardenPlan.regionForPoint (Point.make (0,0))
    [<Test>]
    member _.``It should calculate area correctly`` ()=
        region.area() |> should equal 12L

    [<Test>]
    member _.``It should have found perimeter correctly`` ()=
        region.perimeter() |> should equal 18L

    [<Test>]
    member _.``It should calculate price correctly`` ()=
        region.price() |> should equal 216L


[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 gardenPlan
        |> should equal 1930

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal "the right answer"
