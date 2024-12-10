open System
open Lib 

let read_stdin_lines () : string array =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Array.ofSeq

[<EntryPoint>]
let main args =
    let input = read_stdin_lines() in
    let topoMap = Puzzle.TopoMap.parse input in
    let (part1, part2) = Puzzle.solve topoMap
    printfn "Part 1: %A" part1 ;
    printfn "Part 2: %A" part2;
    0
