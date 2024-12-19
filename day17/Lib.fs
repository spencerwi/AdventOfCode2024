module Lib
open System
open System.Text.RegularExpressions

module Puzzle = begin
    type OpCode =
        | Adv
        | Bxl
        | Bst
        | Jnz
        | Bxc
        | Out
        | Bdv
        | Cdv
    with
        static member fromInt = 
            function
            | 0 -> Adv
            | 1 -> Bxl
            | 2 -> Bst
            | 3 -> Jnz
            | 4 -> Bxc
            | 5 -> Out
            | 6 -> Bdv
            | 7 -> Cdv
            | other -> failwith $"Invalid opcode: {other}"

    type Computer = {
        a : int
        b : int
        c : int
        instructionPtr : int
        program : int array
        outputs : int list
    }
    with 
        static member parse (input : string array) : Computer =
            let registerRegex = Regex(@"Register (A|B|C): (?<initialValue>\d+)", RegexOptions.Compiled) in
            let extractRegisterValue line =
                int registerRegex.Match(line).Groups["initialValue"].Value in 
            let program = 
                input[4]
                |> _.Replace("Program: ", "")
                |> _.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map int
            in
            {
                a = extractRegisterValue input[0]
                b = extractRegisterValue input[1]
                c = extractRegisterValue input[2]
                program = program
                instructionPtr = 0
                outputs = []
            }

        member this.evalComboOperand = 
            function
            | 0 -> 0
            | 1 -> 1
            | 2 -> 2
            | 3 -> 3
            | 4 -> this.a
            | 5 -> this.b
            | 6 -> this.c
            | other -> failwith $"Invalid combo operand {other}"

        member this.eval (opcode : OpCode) (operand : int) =
            match opcode with
            | Adv -> 
                let comboOperand = this.evalComboOperand operand in
                {this with 
                    a = (this.a) / (pown 2 (int comboOperand))
                    instructionPtr = this.instructionPtr + 2
                }
            | Bxl -> 
                {this with 
                    b = this.b ^^^ operand
                    instructionPtr = this.instructionPtr + 2
                }
            | Bst -> 
                let comboOperand = this.evalComboOperand operand in
                {this with 
                    b = comboOperand % 8
                    instructionPtr = this.instructionPtr + 2
                }
            | Jnz when this.a = 0 -> {this with instructionPtr = this.instructionPtr + 2}
            | Jnz -> {this with instructionPtr = operand}
            | Bxc -> 
                {this with 
                    b = this.b ^^^ this.c
                    instructionPtr = this.instructionPtr + 2
                }
            | Out -> 
                let comboOperand = this.evalComboOperand operand in
                {this with
                    outputs = this.outputs @ [comboOperand % 8]
                    instructionPtr = this.instructionPtr + 2
                }
            | Bdv -> 
                let comboOperand = this.evalComboOperand operand in
                {this with 
                    b = (this.a) / (pown 2 (int comboOperand))
                    instructionPtr = this.instructionPtr + 2
                }
            | Cdv -> 
                let comboOperand = this.evalComboOperand operand in
                {this with
                    c = (this.a) / (pown 2 (int comboOperand))
                    instructionPtr = this.instructionPtr + 2
                }

            member this.step() : Computer option =
                if this.instructionPtr >= (this.program.Length - 1) || this.instructionPtr < 0 then
                    None
                else
                    let opcode = OpCode.fromInt this.program[this.instructionPtr] in
                    let operand = this.program[this.instructionPtr + 1] in
                    Some (this.eval opcode operand)

            member this.run() : Computer * (int list) =
                let currentState = this in
                let rec step (computer : Computer) =  
                    match computer.step() with
                    | None -> (computer, computer.outputs)
                    | Some next -> step next
                in
                step currentState

    let part1 (input: string array) =
        Computer.parse input 
        |> _.run()
        |> snd
        |> Seq.map string
        |> String.concat ","

    let part2 (input: string seq) =
        "the right answer"
end