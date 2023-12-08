app "day00"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.File,
        pf.Path,
        pf.Task.{ Task },
    ]
    provides [main] to pf

parseSeeds = \input ->
    List.dropFirst (Str.split input " ") 1
    |> List.keepOks Str.toI64

parseRange = \s ->
    nums = (Str.split s " " |> List.keepOks Str.toI64)

    when nums is
        [dest, src, l] -> { destination: dest, source: src, length: l }
        _ -> crash "nums: \(s)"

parseMap = \input ->
    List.dropFirst (Str.split input "\n") 1
    |> List.map parseRange

parse = \input ->
    groups = Str.split input "\n\n"

    when groups is
        [seeds, .. as rest] -> { seeds: parseSeeds seeds, maps: List.map rest parseMap }
        _ -> crash "failed to parse groups"

part1 = \input ->
    { seeds, maps } = parse input

    mappedSeeds = List.walk maps seeds \ss, map ->
        List.map ss \s ->
            when List.findFirst map \m -> s >= m.source && s <= m.source + m.length is
                Ok m -> s + m.destination - m.source
                Err _ -> s

    when List.min mappedSeeds is
        Ok v -> v
        Err _ -> crash "list empty"

mapSeedRange = \list ->
    when list is
        [from, l] -> { from: Num.toI64 from, to: from + (Num.toI64 l) + 1 }
        _ -> crash "wrong nbr of seeds"

processRow = \queue, row ->
    toRange = { from: row.source, to: row.source + row.length - 1 }

    queue.current
    |> List.walk { current: [], next: queue.next } \newQueue, fromRange ->
        { inside, outside } = fitRange fromRange toRange
        if List.isEmpty inside then
            { current: List.concat newQueue.current outside, next: newQueue.next }
        else
            next = convertRanges inside row
            { current: List.concat newQueue.current outside, next: List.concat newQueue.next next }

fitRange = \fromRange, toRange ->
    if fromRange.to < toRange.from || fromRange.from > toRange.to then
        { inside: [], outside: [fromRange] }
    else if fromRange.from >= toRange.from && fromRange.to <= toRange.to then
        { inside: [fromRange], outside: [] }
    else if fromRange.from < toRange.from && fromRange.to <= toRange.to then
        outside = [{ from: fromRange.from, to: toRange.from - 1 }]
        inside = [{ from: toRange.from, to: fromRange.to }]
        { outside, inside }
    else if fromRange.from >= toRange.from && fromRange.to > toRange.to then
        inside = [{ from: fromRange.from, to: toRange.to }]
        outside = [{ from: toRange.to + 1, to: fromRange.to }]
        { inside, outside }
    else
        inside = [{ from: toRange.from, to: toRange.to }]
        outside = [
            { from: fromRange.from, to: toRange.from - 1 },
            { from: toRange.to + 1, to: fromRange.to },
        ]
        { inside, outside }

convertRanges = \ranges, row ->
    ranges
    |> List.map \range -> {
        from: range.from - row.source + row.destination,
        to: range.to - row.source + row.destination,
    }

processTable = \ranges, maps ->
    newQueue = maps |> List.walk { current: ranges, next: [] } processRow

    List.concat newQueue.current newQueue.next

part2 = \input ->
    { seeds, maps } = parse input
    seedRanges = List.chunksOf seeds 2 |> List.map mapSeedRange

    list =
        List.walk maps seedRanges processTable
        |> List.map .from

    when List.min list is
        Ok v -> v
        Err _ -> crash "list empty"

run =
    input <- File.readUtf8 (Path.fromStr "input") |> Task.await

    p1 = part1 input
    p2 = part2 input

    Stdout.line
        """
        Part 1: \(Num.toStr p1)
        Part 2: \(Num.toStr p2)
        """

main : Task {} I32
main =
    run
    |> Task.onErr \e ->
        dbg
            e

        Stderr.line "Something went wrong!"

expect
    a = part1 simple
    a == 35

expect
    a = part2 simple
    a == 46

simple =
    """
    seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4
    """
