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

unwrap = \res, msg ->
    when res is
        Ok v -> v
        Err _ -> crash "unwrapped value \(msg)"

parseLine = \line ->
    Str.split line " "
    |> List.keepOks Str.toI64

parse = \input ->
    lines = Str.split input "\n"
    List.map lines parseLine

difference : { prev : I64, nums : List I64 }, I64 -> { prev : I64, nums : List I64 }
difference = \{ prev, nums }, num ->
    { prev: num, nums: List.append nums (num - prev) }

fillStart : List (List I64) -> List (List I64)
fillStart = \table ->
    head = List.get table 0 |> unwrap "head"

    hhead = List.get head 0 |> unwrap "hhead"
    htail = List.dropFirst head 1

    diff =
        List.walk htail { prev: hhead, nums: [] } difference
        |> .nums

    if List.all diff \d -> d == 0 then
        List.prepend table diff
    else
        fillStart (List.prepend table diff)

single = \c, msg ->
    when c is
        [l] -> l
        _ -> crash "single: \(msg)"

extrapolateRecur = \{ edge, prev }, end, rows ->
    lastSameRow = when end is
        Last -> List.takeLast (List.get rows 0 |> unwrap "first" ) 1 |> single "last"
        First -> List.takeFirst (List.get rows 0 |> unwrap "first" ) 1 |> single "last"

    lastBeforeRow = prev
    newPrev =  when end is
        Last -> lastSameRow + lastBeforeRow
        First -> lastSameRow - lastBeforeRow
    

    newState = { edge: List.append edge newPrev, prev: newPrev }

    if List.len rows == 1 then
        newState
    else
        extrapolateRecur newState end (List.dropFirst rows 1)

extrapolate : List I64, [First, Last] -> I64
extrapolate = \row, end ->
    b = fillStart [row]

    c = extrapolateRecur { edge: [], prev: 0 } end b


    c.prev

part1 = \input ->
    a = parse input

    b = a |> List.map \r -> extrapolate r Last

    List.sum b

part2 = \input ->
    a = parse input

    b = a |> List.map \r -> extrapolate r First

    List.sum b

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
    a == 0

simple =
    """
    0 3 6 9 12 15
    1 3 6 10 15 21
    10 13 16 21 30 45
    """
