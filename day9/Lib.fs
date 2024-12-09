module Lib
open System

let intOfDigitChar c =
    int64 (string c)

let swapCharsByIndex (s : char array) idx1 idx2 =
    let char2 = s[idx2] in
    s[idx2] <- s[idx1];
    s[idx1] <- char2;

module Puzzle = begin
    // It turns out there are more than 9 files, so file IDs can be two digits. Let's just exploit char for this
    type FileId = char
    let fileIdToInt fid = 
        int64 (fid - '0')

    let intToFileId i = 
        char ((int '0') + i)

    type File = {
        id : FileId
        size : int64
    }
    with
        member this.ToString() : string =
            String.replicate (int this.size) (string this.id)

    type DiskMapSegment = 
        | FileDescriptor of File
        | FreeSpace of int64
    with
        member this.ToString() : string =
            match this with
            | FileDescriptor f -> f.ToString()
            | FreeSpace size -> String.replicate (int size) "."

    type DiskMap = DiskMapSegment array

    module Parsing = begin
        type ParserState = 
            | ParsingFile
            | ParsingFreeSpace
            with 
                member this.toggle() =
                    match this with
                    | ParsingFile -> ParsingFreeSpace
                    | ParsingFreeSpace -> ParsingFile

        let parseDiskMap (input : string) : DiskMap =
            let mutable fileIdCounter = 0 in
            let build parserState digit =
                match parserState with
                | ParsingFile -> 
                    let id = fileIdCounter in
                    fileIdCounter <- fileIdCounter + 1
                    FileDescriptor { id = intToFileId id; size = digit }
                | ParsingFreeSpace ->
                    FreeSpace digit
            in
            let mutable currentState = ParsingFile in
            [|
                for digitChar in input do
                    let number = intOfDigitChar digitChar in
                    yield build currentState number;
                    currentState <- currentState.toggle()
            |]
    end

    let diskMapToString (diskMap : DiskMap) : string =
        diskMap
        |> Seq.map _.ToString()
        |> String.concat ""

    let compact (diskMap : DiskMap) : string =
        let mutable currentState = (diskMapToString diskMap).ToCharArray() in
        let mutable rightmostFileBlockIdx = Array.findIndexBack ((<>) '.') currentState in
        let mutable leftmostFreeSpaceIdx = Array.IndexOf(currentState, '.') in
        while rightmostFileBlockIdx > leftmostFreeSpaceIdx do begin
            swapCharsByIndex currentState rightmostFileBlockIdx leftmostFreeSpaceIdx;
            rightmostFileBlockIdx <- Array.findIndexBack ((<>) '.') currentState;
            leftmostFreeSpaceIdx <- Array.IndexOf(currentState, '.')
        end
        String.Concat currentState
            

    let checksum diskMapStr : int64 =
        query {
            for idx, c in Seq.indexed diskMapStr do
            where (c <> '.')
            sumBy ((int64 idx) * (fileIdToInt c))
        }

    let part1 (input: DiskMap) =
        input
        |> compact
        |> checksum

    let part2 (input: string) =
        "the right answer"
end
