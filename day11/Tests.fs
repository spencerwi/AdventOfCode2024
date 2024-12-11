module Tests
open Lib

open FsUnit
open NUnit.Framework


let sample_input = "125 17"

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member _.``It should solve part 1`` ()=
        let (part1, _) = Puzzle.solve sample_input in
        part1 |> should equal 55312
