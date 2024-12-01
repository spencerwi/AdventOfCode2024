open System
open Lib 

let read_stdin_lines () : string array =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Array.ofSeq

[<EntryPoint>]
let main args =
    let (list1, list2) = 
        read_stdin_lines() 
        |> Puzzle.parseLists
    printfn "Part 1: %A" (Puzzle.part1 list1 list2);
    printfn "Part 2: %A" (Puzzle.part2 list1 list2);
    0
