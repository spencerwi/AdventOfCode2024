module Lib
open System

let tryIndexOf (element : 'a) (sequence : 'a seq) : int option =
    Seq.tryFindIndex ((=) element) sequence

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
                let maybeBeforeIdx = tryIndexOf rule.before this.pages
                let maybeAfterIdx = tryIndexOf rule.after this.pages
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
                |> Seq.choose (fun before -> tryIndexOf before update.pages)
                |> Seq.max
                |> ((+) 1)
            // Now we move the "after" to a safe spot, by first inserting it in
            //  the new location, then removing it from the old location.
            // We do it in that order because, if the rule is broken, it means
            //  the "after" is actually before a "before", meaning that it needs
            //  to be moved *forward* in the list.
            // If we removed and then inserted, we'd be shifting our insert 
            //  location.
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
