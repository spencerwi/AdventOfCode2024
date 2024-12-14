module Lib
open System.Text.RegularExpressions

let tryToWholeNumber (f : float) : int64 option =
    let threshold = 0.001 in
    let rounded = round f in
    let distanceFromWholeNumber = abs (rounded - f) in
    if distanceFromWholeNumber <= threshold then
        Some (int64 rounded)
    else
        None

module Puzzle = begin
    type Point = (int64 * int64)
    type Movement = Point
    let (++) (x, y) (xDiff, yDiff) = 
        (x + xDiff, y + yDiff)

    let movementRegex = Regex(@"Button [A,B]: X\+(?<xDiff>\d+), Y\+(?<yDiff>\d+)", RegexOptions.Compiled)
    let prizeRegex = Regex(@"Prize: X=(?<xLocation>\d+), Y=(?<yLocation>\d+)", RegexOptions.Compiled)

    type ClawMachine = {
        aMovement : Movement
        bMovement : Movement
        prizeLocation : Point
    }
    with
        static member parse (spec : string array) : ClawMachine =
            let parseMovement (line : string) : Movement =
                let matchGroups = movementRegex.Match(line).Groups in
                (
                    int64 matchGroups["xDiff"].Value,
                    int64 matchGroups["yDiff"].Value
                )
            in
            let prizeLocationMatchGroups = prizeRegex.Match(spec[2]).Groups in
            let prizeLocation = (
                int64 prizeLocationMatchGroups["xLocation"].Value,
                int64 prizeLocationMatchGroups["yLocation"].Value
            )
            in
            { 
                aMovement = parseMovement spec[0]
                bMovement = parseMovement spec[1]
                prizeLocation = prizeLocation
            }

        member this.aCost = 3L
        member this.bCost = 1L

        member this.minimumCost (adjustment : int64) : int64 option =
             // equation 1: i(aDx) + j(bDx) = prizeX
             // equation 2: i(aDy) + j(bDy) = prizeY
             let (ax, ay) = this.aMovement in
             let (bx, by) = this.bMovement in
             let mutable (prizeX, prizeY) = this.prizeLocation in
             prizeX <- prizeX + adjustment;
             prizeY <- prizeY + adjustment;
             // the matrix would be
             //   [ax  bx]
             //   [ay  by]
             // that's a 2x2 matrix; we're gonna use Cramer's rule.
             // that means we need the determinant, which for this 2x2 matrix is
             let determinant = float ((ax * by) - (bx * ay)) in
             // First, though, if the determinant is zero, there's no solution
             if determinant = 0 then
                 None
             else
                 // now, we take the determinants of two matrices:
                 //  1. one to solve for a, which we'll call A
                 //  2. one to solve for b, which we'll call B
                 // A is formed by swapping the a coefficients out for the constants:
                 //  [prizeX  bx]
                 //  [prizeY  by]
                 // B is formed likewise by swapping the b coefficients out:
                 //  [ax  prizeX]
                 //  [ay  prizeY]
                 // Let's take the determinants of these:
                 let detA = float ((prizeX * by) - (bx * prizeY)) in
                 let detB = float ((ax * prizeY) - (prizeX * ay)) in
                 // Now to solve: a = detA/determinant, and b = detB/determinant
                 let floatSolutionForA = (detA / determinant) in 
                 let floatSolutionForB = (detB / determinant) in
                 // We can only press the button an integer number of times.
                 // If the solution requires that we press the button a fractional number of times, it's not a workable solution.
                 // Floating point errors are annoying; we can't just cast to int64 directly. We have to check "is this a whole number *within a specific margin of error*?"
                 match tryToWholeNumber floatSolutionForA, tryToWholeNumber floatSolutionForB with
                 | Some aPresses, Some bPresses -> 
                     let cost = (aPresses * 3L) + bPresses in
                     Some cost
                 | _ -> None

    let parseInput (input : string seq) : ClawMachine seq =
        input
        |> Seq.chunkBySize 4
        |> Seq.map ClawMachine.parse

    let part1 (clawMachines : ClawMachine seq) =
        clawMachines
        |> Seq.choose (fun machine -> machine.minimumCost 0L)
        |> Seq.sum

    let part2 (clawMachines : ClawMachine seq) =
        clawMachines
        |> Seq.choose (fun machine -> machine.minimumCost 10000000000000L)
        |> Seq.sum
end
