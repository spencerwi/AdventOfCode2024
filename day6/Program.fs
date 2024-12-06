﻿open System
open Lib 

let read_stdin_lines () : string array =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Array.ofSeq

[<EntryPoint>]
let main args =
    let input = read_stdin_lines() in
    let guard, grid = Puzzle.parse input
    printfn "Part 1: %A" (Puzzle.part1 guard grid);
    printfn "Part 2: %A" (Puzzle.part2 guard grid);
    0
