module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member _.``It should parse correctly`` ()=
        let expectedRules = seq {
            {after = 53; before = 47}
            {after = 13; before = 97}
            {after = 61; before = 97}
            {after = 47; before = 97}
            {after = 29; before = 75}
            {after = 13; before = 61}
            {after = 53; before = 75}
            {after = 13; before = 29}
            {after = 29; before = 97}
            {after = 29; before = 53}
            {after = 53; before = 61}
            {after = 53; before = 97}
            {after = 29; before = 61}
            {after = 13; before = 47}
            {after = 47; before = 75}
            {after = 75; before = 97}
            {after = 61; before = 47}
            {after = 61; before = 75}
            {after = 29; before = 47}
            {after = 13; before = 75}
            {after = 13; before = 53}
        }
        let expectedUpdates = seq {
            { pages = [|75;47;61;53;29|] }
            { pages = [|97;61;53;29;13|] }
            { pages = [|75;29;13|] }
            { pages = [|75;97;47;61;53|] }
            { pages = [|61;13;29|] }
            { pages = [|97;13;75;29;47|] }
        }
        parseInput sample_input
        |> should equal (
            expectedRules, expectedUpdates
        )

    [<Test>]
    member _.``It should test validity correctly`` ()=
        let rules, updates = parseInput sample_input in
        updates
        |> Seq.map (fun update -> update.isValid rules)
        |> should equalSeq [
            true;
            true;
            true;
            false;
            false;
            false
        ]
        

    [<Test>]
    member this.``It should solve part 1`` ()=
        let rules, updates = parseInput sample_input
        part1 rules updates
        |> should equal 143

    [<Test>]
    member this.``It should solve part 2`` ()=
        let rules, updates = parseInput sample_input
        part2 rules updates
        |> should equal 123
