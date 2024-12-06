open System
open Lib 

let read_stdin_lines () : string array =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Array.ofSeq

[<EntryPoint>]
let main args =
    let input = read_stdin_lines() in
    let guard, grid = Puzzle.parse input
    let part1, part2 = Puzzle.solve guard grid
    printfn "Part 1: %A" part1;
    printfn "Part 2: %A" part2;
    0
