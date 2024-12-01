module Lib
open System

module Puzzle = begin
    let parseLists (input: string seq) : (int list * int list) =
            input
            |> Seq.map (fun s -> s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) )
            |> Seq.map (fun [|a;b|] -> (int a, int b))
            |> List.ofSeq
            |> List.unzip


    let distanceBetween a b =
        abs (b - a)

    let part1 (list1 : int list) (list2 : int list) =
        let sortedList1 = List.sort list1 in
        let sortedList2 = List.sort list2 in
        let distances = 
            List.zip sortedList1 sortedList2 
            |> List.map (fun (a, b) -> distanceBetween a b)
        in
        List.sum distances

    let part2 (list1 : int list) (list2 : int list) =
        let list2Occurrences = 
            list2
            |> Seq.countBy id
            |> Map.ofSeq
        in
        let similarityScore x =
            let count = 
                list2Occurrences.TryFind x
                |> Option.defaultValue 0
            in
            x * count
        in
        list1 
        |> Seq.sumBy similarityScore
end
