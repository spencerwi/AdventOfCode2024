module Lib

module Puzzle = begin
    type Point = (int * int)
    let (++) (row1, col1) (row2, col2) =
        (row1 + row2, col1 + col2)
    let (--) (row2, col2) (row1, col1) =
        (row2 - row1, col2 - col1)

    let allPairings input =
        input 
        |> Seq.mapi (fun i x -> 
            input 
            |> Seq.skip (i + 1) 
            |> Seq.map (fun y -> (x, y))
        )
        |> Seq.concat

    let rec greatestCommonDivisor a b =
        if b = 0 then abs a
        else greatestCommonDivisor b (a % b)

    let simplify (row, col) =
        let gcd = greatestCommonDivisor row col
        (row / gcd, col / gcd)
        

    type AntinodesType = 
        | Part1
        | Part2

    type BroadcastMap = {
        antennas : Map<char, Point list>
        width : int
        height : int
    }
    with 
        static member parse (input : string array) =
            let allAntennaLocations = seq {
                for row, line in Seq.indexed input do
                    for col, c in Seq.indexed line do
                        if c <> '.' then yield (c, (row, col))
            } in
            let groupedAntennaLocations =  
                allAntennaLocations
                |> Seq.groupBy fst
                |> Seq.map (fun (key, values) -> 
                    let justPoints = 
                        values
                        |> Seq.map (fun ((_, (row, col))) -> (row, col))
                        |> List.ofSeq
                    in
                    (key, justPoints)
                )
                |> Map.ofSeq
            in
            { 
                antennas = groupedAntennaLocations 
                height = input.Length
                width = input[0].Length
            }

        member this.contains (row, col) : bool =
            row >= 0 && row < this.height &&
            col >= 0 && col < this.width

        member this.antinodes p1 p2 : Point seq =
            let diff = p2 -- p1 in
            [ 
                p1 ++ diff ++ diff;
                p1 -- diff
            ]
            |> Seq.filter this.contains

        member this.colinearPoints p1 p2 : Point seq =
            let diff = simplify (p2 -- p1) in
            seq {
                yield! seq {
                    let mutable current = p1 -- diff in
                    while this.contains current do
                        yield current
                        current <- current -- diff
                }
                yield p1
                yield! seq {
                    let mutable current = p1 ++ diff in
                    while this.contains current do
                        yield current
                        current <- current ++ diff
                }
            }

        member this.antinodesFor (antinodesType : AntinodesType) (c : char) : Point seq =
            let antinodeFinder = 
                match antinodesType with
                | Part1 -> (fun (a, b) -> this.antinodes a b)
                | Part2 -> (fun (a, b) -> this.colinearPoints a b)
            in
            match this.antennas.TryFind c with
            | None -> Seq.empty
            | Some [_] -> Seq.empty
            | Some points ->
                points
                |> allPairings
                |> Seq.collect antinodeFinder
                |> Seq.filter this.contains
        
        member this.allAntinodes antinodesType : Set<Point> =
            seq {
                for freq in this.antennas.Keys do 
                    yield! this.antinodesFor antinodesType freq
            } |> Set.ofSeq

    let part1 (broadcastMap : BroadcastMap) =
        broadcastMap.allAntinodes Part1
        |> Set.count

    let part2 (broadcastMap : BroadcastMap) =
        broadcastMap.allAntinodes Part2
        |> Set.count
end
