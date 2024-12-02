module Tests
open Lib
open Puzzle

open FsUnit
open NUnit.Framework

let sample_input_raw = """
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"""

let sample_input = sample_input_raw.Trim().Split "\n"

[<TestFixture>]
type ``Tests for Report`` ()=
    [<Test>]
    member _.``It should parse properly`` ()=
        let expected = {
            levels = [7; 6; 4; 2; 1]
        }
        let actual = Report.parse sample_input[0] in
        actual |> should equal expected

    [<Test>]
    member _.``It should generate variations properly`` ()=
        let expected = seq {
            { levels = [6; 4; 2; 1] };
            { levels = [7; 4; 2; 1] };
            { levels = [7; 6; 2; 1] };
            { levels = [7; 6; 4; 1] };
            { levels = [7; 6; 4; 2] };
        } in
        let baseReport = (Report.parse sample_input[0]) in
        let actual = baseReport.variations() in
        actual |> should equal expected

    [<Test>]
    member _.``It should report safety properly`` ()=
        let toleranceRange = {
            min = 1
            max = 3
        } in
        let expected = [
            Safe; Unsafe; Unsafe; SafeIfOneMistakeAllowed; SafeIfOneMistakeAllowed; Safe
        ] in
        let actual = 
            sample_input 
            |> Seq.map Report.parse 
            |> Seq.map (checkSafety toleranceRange)
        in
        actual |> should equal expected

[<TestFixture>]
type ``Tests for solution`` ()=
    let reports = 
        sample_input 
        |> Seq.map Report.parse
        |> List.ofSeq
    let toleranceRange = {min = 1; max = 3}

    [<Test>]
    member _.``It should solve part 1`` ()=
        let counts = solve toleranceRange reports in
        counts.part1 |> should equal 2

    [<Test>]
    member _.``It should solve part 2`` ()=
        let counts = solve toleranceRange reports in
        counts.part2 |> should equal 4
