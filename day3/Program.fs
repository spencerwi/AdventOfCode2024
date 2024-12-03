open System
open Lib 

let read_stdin_lines () : string =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> String.concat ""

[<EntryPoint>]
let main args =
    let input = read_stdin_lines() in
    let commands = Puzzle.parseCommands input
    printfn "Part 1: %A" (Puzzle.part1 commands);
    printfn "Part 2: %A" (Puzzle.part2 commands);
    0
