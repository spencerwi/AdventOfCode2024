module Lib
open System

module Puzzle = begin

    type Stone = int64
    let updateStone (stone : Stone) : Stone list =
        if stone = 0 then 
            [1L]
        else
            let digits = string stone in
            if digits.Length % 2 = 0 then
                let midpoint = digits.Length / 2 in
                let left = digits[0..(midpoint - 1)] in
                let right = digits[midpoint  ..] in
                [ int64 left ; int64 right ]
            else
                [stone * 2024L]

    type Stones = {
        mutable stoneCounts : Map<Stone, int64>
    }
    with 
        static member  parse (input : string) : Stones = 
            let counts =
                input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                |> Seq.map int64
                |> Seq.countBy id
                |> Seq.map (fun (stone, count) -> (stone, int64 count))
                |> Map.ofSeq
            in {stoneCounts = counts}

        member this.getCount (stone : Stone) : int64 =
            match Map.tryFind stone this.stoneCounts with
            | Some x -> x
            | None -> 0

        member this.setCount (stone : Stone) (newCount : int64) =
            this.stoneCounts <- Map.add stone newCount this.stoneCounts

        member this.incrementBy (stone : Stone) (increment : int64) = 
            this.setCount stone ((this.getCount stone) + increment)

        member this.decrementBy (stone : Stone) (decrement : int64) =
            if (this.getCount stone) = decrement then
                this.stoneCounts <- this.stoneCounts.Remove stone
            else
                this.setCount stone ((this.getCount stone) - decrement)

        member this.blink() =
            let currentStones = Map.toSeq this.stoneCounts in
            for (inputStone, inputStoneCount) in currentStones do
                let outputStones = updateStone inputStone in
                this.decrementBy inputStone inputStoneCount
                for outputStone in outputStones do
                    this.incrementBy outputStone inputStoneCount

        member this.totalStoneCount() : int64 =
            this.stoneCounts.Values
            |> Seq.sum

    let solve (input: string) =
        let mutable stones = Stones.parse input in
        for _ in 1..25 do begin
            stones.blink()
        end
        let part1 = stones.totalStoneCount() in
        for _ in 26..75 do begin
            stones.blink()
        end
        let part2 = stones.totalStoneCount() in
        (part1, part2)
end
