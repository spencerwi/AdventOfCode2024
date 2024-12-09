module Tests
open Lib.Puzzle

open FsUnit
open NUnit.Framework

let sample_input = "2333133121414131402"

[<TestFixture>]
type ``Parsing tests`` ()=
    [<Test>]
    member _.``It should parse DiskMaps correctly`` ()=
        Parsing.parseDiskMap "12345" 
        |> should equalSeq [|
            FileDescriptor { id = '0'; size = 1}
            FreeSpace 2
            FileDescriptor { id = '1'; size = 3}
            FreeSpace 4
            FileDescriptor { id = '2'; size = 5}
        |]

    [<Test>]
    member _.``It should parse DiskMaps with > 10 files correctly`` ()=
        Parsing.parseDiskMap (String.replicate 11 "10") 
        |> should equalSeq [|
            FileDescriptor { id = '0'; size = 1}
            FreeSpace 0
            FileDescriptor { id = '1'; size = 1}
            FreeSpace 0
            FileDescriptor { id = '2'; size = 1}
            FreeSpace 0
            FileDescriptor { id = '3'; size = 1}
            FreeSpace 0
            FileDescriptor { id = '4'; size = 1}
            FreeSpace 0
            FileDescriptor { id = '5'; size = 1}
            FreeSpace 0
            FileDescriptor { id = '6'; size = 1}
            FreeSpace 0
            FileDescriptor { id = '7'; size = 1}
            FreeSpace 0
            FileDescriptor { id = '8'; size = 1}
            FreeSpace 0
            FileDescriptor { id = '9'; size = 1}
            FreeSpace 0
            FileDescriptor { id = intToFileId 10; size = 1}
            FreeSpace 0
        |]

[<TestFixture>]
type ``Tests for stringification`` ()=
    let diskMap = Parsing.parseDiskMap sample_input
    [<Test>]
    member _.``It stringifies a DiskMap correctly`` ()=
        diskMapToString diskMap
        |> should equal "00...111...2...333.44.5555.6666.777.888899"

    [<Test>]
    member _.``It stringifies a DiskMap with >10 files correctly`` ()=
        (String.replicate 11 "11")
        |> Parsing.parseDiskMap 
        |> diskMapToString 
        |> should equal "0.1.2.3.4.5.6.7.8.9.:."

[<TestFixture>]
type ``Tests for compaction`` ()=
    let diskMap = Parsing.parseDiskMap sample_input
    let twoDigitFileDiskMap = Parsing.parseDiskMap (String.replicate 11 "11")

    [<Test>]
    member _.``It blockwise-compacts a DiskMap correctly`` ()=
        compactBlockwise diskMap
        |> should equal "0099811188827773336446555566.............."

    [<Test>]
    member _.``It blockwise-compacts a DiskMap with >10 files correctly`` ()=
        twoDigitFileDiskMap
        |> compactBlockwise 
        |> should equal "0:192837465..........."

    [<Test>]
    member _.``It filewise-compacts a DiskMap correctly`` ()= 
        compactFilewise diskMap
        |> diskMapToString
        |> should equal "00992111777.44.333....5555.6666.....8888.."


[<TestFixture>]
type ``Tests for solution`` ()=
    let diskMap = Parsing.parseDiskMap sample_input

    [<Test>]
    member _.``It should solve part 1`` ()=
        part1 diskMap
        |> should equal 1928

    [<Test>]
    member _.``It should solve part 2`` ()=
        part2 diskMap
        |> should equal 2858
