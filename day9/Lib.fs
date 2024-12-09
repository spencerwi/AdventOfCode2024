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
        char ((int64 '0') + i)

    type File = {
        id : FileId
        size : int64
    }
    with
        override this.ToString() : string =
            String.replicate (int this.size) (string this.id)

        member this.fitsInto = 
            function
            | FileDescriptor _ -> false
            | FreeSpace freeSpaceSize -> this.size <= freeSpaceSize

    and DiskMapSegment = 
        | FileDescriptor of File
        | FreeSpace of int64
    with
        override this.ToString() : string =
            match this with
            | FileDescriptor f -> f.ToString()
            | FreeSpace size -> String.replicate (int size) "."

        member this.isFile : bool =
            match this with
            | FileDescriptor _ -> true
            | _ -> false

        member this.isFreeSpace : bool =
            not this.isFile

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

    let compactBlockwise (diskMap : DiskMap) : string =
        let mutable currentState = (diskMapToString diskMap).ToCharArray() in
        let mutable rightmostFileBlockIdx = Array.findIndexBack ((<>) '.') currentState in
        let mutable leftmostFreeSpaceIdx = Array.IndexOf(currentState, '.') in
        while rightmostFileBlockIdx > leftmostFreeSpaceIdx do begin
            swapCharsByIndex currentState rightmostFileBlockIdx leftmostFreeSpaceIdx;
            rightmostFileBlockIdx <- Array.findIndexBack ((<>) '.') currentState;
            leftmostFreeSpaceIdx <- Array.IndexOf(currentState, '.')
        end
        String.Concat currentState

    let moveFile (diskMap : DiskMap) (fileId : FileId) : DiskMap =
        let fileIdx = 
            diskMap
            |> Seq.findIndex (function 
                | FileDescriptor f -> f.id = fileId
                | _ -> false
            )
        in
        let (FileDescriptor fileToMove) = diskMap[fileIdx] in
        let maybeMatchingFreeSpaceIdx =
            diskMap
            |> Array.tryFindIndex (fileToMove.fitsInto)
        in
        match maybeMatchingFreeSpaceIdx with
        | Some spaceIdx when spaceIdx < fileIdx ->
            let mutable updatedState = diskMap in
            let (FreeSpace freeSpaceSize) = updatedState[spaceIdx] in
            updatedState[spaceIdx] <- FileDescriptor fileToMove
            updatedState[fileIdx] <- FreeSpace fileToMove.size
            let remainingFreeSpaceSize = freeSpaceSize - fileToMove.size in
            if remainingFreeSpaceSize > 0 then begin
                updatedState <- Array.insertAt (spaceIdx + 1) (FreeSpace remainingFreeSpaceSize) updatedState
            end;
            updatedState
        | _ -> diskMap

    let compactFilewise (diskMap : DiskMap) : DiskMap =
        let lastFileId = 
            diskMap
            |> Seq.filter (_.isFile)
            |> Seq.map (fun (FileDescriptor f) -> f.id)
            |> Seq.maxBy fileIdToInt
        in
        let mutable currentState = diskMap in
        let mutable currentFileToMoveId = lastFileId in
        while currentFileToMoveId >= '0' do begin
            currentState <- moveFile currentState currentFileToMoveId;
            currentFileToMoveId <- (currentFileToMoveId - (char 1))
        end
        currentState

            

    let checksum diskMapStr : int64 =
        query {
            for idx, c in Seq.indexed diskMapStr do
            where (c <> '.')
            sumBy ((int64 idx) * (fileIdToInt c))
        }

    let part1 (input: DiskMap) =
        input
        |> compactBlockwise
        |> checksum

    let part2 (input: DiskMap) =
        input
        |> compactFilewise
        |> diskMapToString
        |> checksum
end
