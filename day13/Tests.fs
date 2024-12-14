module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing tests`` ()=
    [<Test>]
    member _.``It should parse input into claw machines`` ()=
        parseInput sample_input
        |> should equalSeq [
            { aMovement = (94, 34); bMovement = (22, 67); prizeLocation = (8400, 5400)}
            { aMovement = (26, 66); bMovement = (67, 21); prizeLocation = (12748, 12176)}
            { aMovement = (17, 86); bMovement = (84, 37); prizeLocation = (7870, 6450)}
            { aMovement = (69, 23); bMovement = (27, 71); prizeLocation = (18641, 10279)}
        ]

let clawMachines = parseInput sample_input

[<TestFixture>]
type ``Tests for ClawMachine`` ()=
    [<Test>]
    member _.``It should solve the minimum cost correctly``() =
        clawMachines
        |> Seq.head
        |> (fun machine -> machine.minimumCost 0L)
        |> should equal (Some 280L)

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 clawMachines
        |> should equal 480

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 clawMachines
        |> should greaterThan 480 
