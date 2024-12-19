module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Parsing Tests`` ()=
    [<Test>]
    member _.``It should parse input`` ()=
        let computer = Computer.parse sample_input in
        computer.a |> should equal 729;
        computer.b |> should equal 0;
        computer.c |> should equal 0;
        computer.instructionPtr |> should equal 0;
        computer.outputs |> should be Empty;
        computer.program |> should equal [|0; 1; 5; 4; 3; 0|]

[<TestFixture>]
type ``Computer tests`` ()=
    let emptyComputer = {
        a = 0; b = 0; c = 0; instructionPtr = 0; outputs = []; program = [||]
    }

    [<Test>]
    member _.``It should Bst correctly``()=
        let computer = {emptyComputer with c = 9; program = [|2;6|]} in
        computer.run()
        |> fst
        |> _.b
        |> should equal 1

    [<Test>]
    member _.``It should handle the second example correctly``()=
        let computer = {emptyComputer with a = 10; program = [|5;0;5;1;5;4|]} in
        computer.run()
        |> snd
        |> should equalSeq [0;1;2]

    [<Test>]
    member _.``It should handle the third example correctly``()=
        let computer = {emptyComputer with a = 2024; program = [|0;1;5;4;3;0|]} in
        let endState, outputs = computer.run() in
        outputs |> should equalSeq [4;2;5;6;7;7;7;7;3;1;0]
        endState.a |> should equal 0

    [<Test>]
    member _.``It should Bxl correctly``()=
        let computer = {emptyComputer with b = 29; program = [|1;7|]} in
        computer.run()
        |> fst
        |> _.b
        |> should equal 26

    [<Test>]
    member _.``It should Bxc correctly``()=
        let computer = {emptyComputer with b = 2024; c = 43690; program = [|4;0|]} in
        computer.run()
        |> fst
        |> _.b
        |> should equal 44354

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 sample_input
        |> should equal "4,6,3,5,6,3,5,2,1,0"

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 sample_input
        |> should equal "the right answer"
