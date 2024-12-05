module Lib
open System

module Puzzle = begin

    type OrderingRule = {
        before : int
        after : int
    }
        with 
            static member parse (line : string) : OrderingRule =
                let [|before; after|] = 
                    line.Split([|'|'|], StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map int
                in
                { before = before; after = after }

    type PageUpdate = {
        pages : int list
    }
        with 
            static member parse (line : string) : PageUpdate =
                let pages = 
                    line.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map int
                    |> List.ofSeq
                in
                { pages = pages }

            member this.passesRule (rule : OrderingRule) : bool =
                let maybeBeforeIdx = 
                    this.pages
                    |> List.tryFindIndex ((=) rule.before) in
                let maybeAfterIdx = 
                    this.pages 
                    |> List.tryFindIndex ((=) rule.after) in
                match maybeBeforeIdx, maybeAfterIdx with
                | (Some beforeIdx, Some afterIdx) when beforeIdx > afterIdx -> false
                | _ -> true
                

            member this.isValid (rules : OrderingRule seq) : bool =
                rules
                |> Seq.forall (this.passesRule)

            member this.middle : int =
                let target = (this.pages.Length / 2) in
                this.pages[target]



    let parseInput (input : string seq) : (OrderingRule seq * PageUpdate seq) =
        let rulesSection =
            input
            |> Seq.takeWhile (not << String.IsNullOrEmpty)
        let updatesSection =
            input
            |> Seq.skipWhile (not << String.IsNullOrEmpty)
            |> Seq.skip 1
        in
        let rules = 
            rulesSection
            |> Seq.map OrderingRule.parse
        in
        let updates = 
            updatesSection
            |> Seq.map PageUpdate.parse 
        in
        (rules, updates)

    let part1 (rules : OrderingRule seq) (updates : PageUpdate seq) =
        query {
            for update in updates do
            where (update.isValid rules)
            sumBy (update.middle)
        }

    let part2 (input: string seq) =
        "the right answer"
end
