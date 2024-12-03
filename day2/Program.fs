open System
open Lib.Puzzle

let read_stdin_lines () : string array =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Array.ofSeq

[<EntryPoint>]
let main args =
    let input = read_stdin_lines() in
    let toleranceRange = ToleranceRange.Default in
    let reports = 
        input
        |> Seq.map Report.parse
        |> List.ofSeq
    in
    let counts = solve toleranceRange reports in
    printfn "Part 1: %A" counts.part1
    printfn "Part 2: %A" counts.part2
    0
