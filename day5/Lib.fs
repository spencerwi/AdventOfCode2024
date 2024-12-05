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
        pages : int array
    }
        with 
            static member parse (line : string) : PageUpdate =
                let pages = 
                    line.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map int
                    |> Array.ofSeq
                in
                { pages = pages }

            member this.passesRule (rule : OrderingRule) : bool =
                let maybeBeforeIdx = 
                    this.pages
                    |> Array.tryFindIndex ((=) rule.before) in
                let maybeAfterIdx = 
                    this.pages 
                    |> Array.tryFindIndex ((=) rule.after) in
                match maybeBeforeIdx, maybeAfterIdx with
                | (Some beforeIdx, Some afterIdx) when beforeIdx > afterIdx -> false
                | _ -> true
                

            member this.isValid (rules : OrderingRule seq) : bool =
                rules
                |> Seq.forall (this.passesRule)

            member this.middle : int =
                let target = (this.pages.Length / 2) in
                this.pages[target]

    let rec reorder (rules : OrderingRule seq) (update : PageUpdate) : PageUpdate =
        let maybeFirstBrokenRule = 
            rules 
            |> Seq.tryFind (not << update.passesRule)
        in
        match maybeFirstBrokenRule with
        | None -> update
        | Some brokenRule -> 
            let indexOfAfter = Array.IndexOf(update.pages, brokenRule.after) in
            let lowestAllowableSpot = 
                rules
                |> Seq.filter (fun rule -> rule.after = brokenRule.after)
                |> Seq.map _.before
                |> Seq.map (fun before -> Array.IndexOf(update.pages, before))
                |> Seq.filter (fun idx -> idx >= 0)
                |> Seq.max
                |> ((+) 1)
            let reorderedPages = 
                update.pages
                |> Array.insertAt lowestAllowableSpot brokenRule.after
                |> Array.removeAt indexOfAfter
            in
            reorder rules { pages = reorderedPages }



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

    let part2 (rules : OrderingRule seq) (updates : PageUpdate seq) =
        query {
            for update in updates do
            where (not (update.isValid rules))
            let reorderedUpdate = reorder rules update in
            sumBy (reorderedUpdate.middle)
        }
end
