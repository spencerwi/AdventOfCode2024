﻿open System
open Lib 

let read_stdin_lines () : string array =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Array.ofSeq

[<EntryPoint>]
let main args =
    let input = read_stdin_lines()[0] in
    let diskMap = Puzzle.Parsing.parseDiskMap input in
    printfn "Part 1: %A" (Puzzle.part1 diskMap);
    printfn "Part 2: %A" (Puzzle.part2 diskMap);
    0
