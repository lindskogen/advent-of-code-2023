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
        [dest, src, l] -> { dest: dest, src: src, len: l, diff: dest - src }
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
            when List.findFirst map \m -> s >= m.src && s <= m.src + m.len is
                Ok m -> s + m.dest - m.src
                Err _ -> s

    when List.min mappedSeeds is
        Ok v -> v
        Err _ -> crash "list empty"

mapSeedRange = \list ->
    when list is
        [from, l] -> { from: Num.toI64 from, len: Num.toI64 l }
        _ -> crash "wrong nbr of seeds"

mapSeedRangeBf = \list ->
    when list is
        [from, l] -> List.range { start: At (Num.toI64 from), end: Length (Num.toNat l) }
        _ -> crash "wrong nbr of seeds"

getMappedSeed = \s, m ->
    newFrom = m.diff + s.from
    newTo = newFrom + s.len
    maxMap = m.dest + m.len

    maxTo = Num.min newTo maxMap

    newLen = maxTo - newFrom

    { from: newFrom, len: newLen }

part2bf = \input ->
    { seeds, maps } = parse input
    seedRanges = List.join (List.chunksOf seeds 2 |> List.map mapSeedRangeBf)

    mappedSeeds = List.walk maps seedRanges \ss, map ->
        List.map ss \s ->
            when List.findFirst map \m -> s >= m.src && s <= m.src + m.len is
                Ok m -> s + m.dest - m.src
                Err _ -> s

    when List.min mappedSeeds is
        Ok v -> v
        Err _ -> crash "list empty"

part2 = \input ->
    { seeds, maps } = parse input
    seedRanges = List.chunksOf seeds 2 |> List.map mapSeedRange

    mappedSeeds = List.walk maps seedRanges \ss, map ->
        List.map ss \s ->
            when List.findFirst map \m -> s.from >= m.src && s.from + s.len <= m.src + m.len is
                Ok m -> getMappedSeed s m
                Err _ -> s

    when List.min (List.map mappedSeeds .from) is
        Ok v -> v
        Err _ -> crash "list empty"

run =
    input <- File.readUtf8 (Path.fromStr "input") |> Task.await

    p1 = part1 simple
    p2 = part2bf input

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
    a == 0

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
