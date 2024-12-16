open System
open Lib.Puzzle

let read_stdin_lines () : string array =
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Array.ofSeq

[<EntryPoint>]
let main args =
    let input = read_stdin_lines() in
    let robots = Array.map Robot.parse input in
    let tileMap = {
        robots = robots
        width = 101
        height = 103
    }
    if Array.contains "part1" args then
        printfn "Part 1: %A" (part1 tileMap);
    elif Array.contains "part2" args then
        printfn "Part 2: %A" (part2 tileMap);
    else
        printfn "For this one, you have to pass `part1` or `part2` as an argument, since part2 is so weird and open-ended"
    0
