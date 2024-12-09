module Lib

module Puzzle = begin
    type Point = (int * int)
    let (++) (row1, col1) (row2, col2) =
        (row1 + row2, col1 + col2)
    let (--) (row2, col2) (row1, col1) =
        (row2 - row1, col2 - col1)

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

        member this.antinodesFor (c : char) : Point seq =
            match this.antennas.TryFind c with
            | None -> Seq.empty
            | Some [_] -> Seq.empty
            | Some points ->
                points
                |> Seq.allPairs points
                |> Seq.filter (fun (a, b) -> a <> b)
                |> Seq.distinctBy (fun (a, b) -> 
                    List.sort [a; b]
                )
                |> Seq.collect (fun (p1, p2) -> 
                    this.antinodes p1 p2 
                )
                |> Seq.filter this.contains
        
        member this.allAntinodes() : Point seq =
            seq {
                for freq in this.antennas.Keys do 
                    yield! this.antinodesFor freq
            }

    let part1 (broadcastMap : BroadcastMap) =
        broadcastMap.allAntinodes()
        |> Set.ofSeq
        |> Set.count

    let part2 (input: string seq) =
        "the right answer"
end
