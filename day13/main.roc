app "day00"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.File,
        pf.Path,
        pf.Task.{ Task },
        array2d.Array2D.{ Array2D },
    ]
    provides [main] to pf

unwrap = \res, msg ->
    when res is
        Ok v -> v
        Err _ -> crash "unwrapped value \(msg)"

parse = \input ->
    input
    |> Str.split
        "\n\n"
    |> List.map
        \g -> Str.split g "\n" |> List.map Str.graphemes |> Array2D.fromLists (FitLongest ".")

countMismatches = \l1, l2 ->
    (List.map2 l1 l2 \a, b -> (a, b)) |> List.countIf \(a, b) -> a != b

compareLists : List (List Str), Nat, Nat -> Result Nat [OutOfRange]
compareLists = \lists, i1, i2 ->
    when (List.get lists i1, List.get lists i2) is
        (Ok v1, Ok v2) -> Ok (countMismatches v1 v2)
        _ -> Err OutOfRange

startRecur = \pattern, toleratedSmudges ->
    lists = Array2D.toLists pattern
    cols = (Array2D.shape pattern).dimX

    recur = \a, b, smudges, matchedBefore ->
        when compareLists lists a b is
            Err OutOfRange -> if matchedBefore then Ok smudges else Err NotFound
            Ok newCount ->
                newTotal = smudges + newCount
                if newTotal > toleratedSmudges then
                    Err TooManyMismatches
                else if a == 0 || b == cols then
                    Ok newTotal
                else
                    recur (a - 1) (b + 1) newTotal Bool.true

    List.range { start: At 0, end: At (cols - 1) }
    |> List.findFirst \fromIdx ->

        recur fromIdx (fromIdx + 1) 0 Bool.false == Ok toleratedSmudges

debug = \d ->
    dbg d

    d

findReflection = \pattern, toleratedSmudges ->
    r = startRecur pattern toleratedSmudges

    when r is
        Ok i ->
            (i + 1) * 100

        _ ->
            when startRecur (Array2D.transpose pattern) toleratedSmudges is
                Ok i ->
                    (i + 1)

                _ ->
                    dbg pattern

                    crash "failed to find reflection"

part1 = \input ->
    patterns = parse input

    List.map patterns (\p -> findReflection p 0) |> List.sum

part2 = \input ->
    patterns = parse input

    List.map patterns (\p -> findReflection p 1) |> List.sum

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
        dbg e

        Stderr.line "Something went wrong!"

expect
    a = part1 simple1
    a == 405

expect
    a = part1 simple2
    a == 1300

expect
    a = part2 simple1
    a == 400

simple1 =
    """
    #.##..##.
    ..#.##.#.
    ##......#
    ##......#
    ..#.##.#.
    ..##..##.
    #.#.##.#.

    #...##..#
    #....#..#
    ..##..###
    #####.##.
    #####.##.
    ..##..###
    #....#..#
    """

simple2 =
    """
    #.##..##...
    .##..#.#..#
    .##..#.#.##
    #.##..##...
    #..#####.##
    ....#.##.##
    #.##.##..##
    #.#..##.###
    ....#....##
    ##.#.#..##.
    ..#.#....##
    .##..######
    ##..#.#.##.
    ##..#.#.##.
    .##..######
    ..#.#....##
    ##.#.#..##.
    """
