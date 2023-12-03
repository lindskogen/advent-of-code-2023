app "day03"
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

parse = \input ->
    lines = Str.split input "\n"

    (
        List.mapWithIndex lines \l, i ->
            List.mapWithIndex (Str.graphemes l) \c, j ->
                ((Num.toI32 i, Num.toI32 j), c)
    )
    |> List.join
    |> Dict.fromList

strType = \c ->
    when c is
        "1" -> Nbr
        "2" -> Nbr
        "3" -> Nbr
        "4" -> Nbr
        "5" -> Nbr
        "6" -> Nbr
        "7" -> Nbr
        "8" -> Nbr
        "9" -> Nbr
        "0" -> Nbr
        "." -> Dot
        _ -> Sym

debug = \v ->
    dbg
        v

    v

StrWithPos : { col : I32, value : Str }

State : { nbr : Str, startIdx : [Some I32, None], collected : List StrWithPos }

Graph : Dict (I32, I32) Str

walkTheLine : Str -> List StrWithPos
walkTheLine = \line ->
    rest =
        Str.graphemes line
        |> List.walkWithIndex
            emptyState
            (\state, c, x ->
                when (strType c, state) is
                    (Nbr, { startIdx: None }) -> { state & startIdx: Some (Num.toI32 x), nbr: c }
                    (Nbr, { startIdx: Some _, nbr }) -> { state & nbr: Str.concat nbr c }
                    (_, { startIdx: Some i, nbr }) -> { state & nbr: "", startIdx: None, collected: List.append state.collected ({ col: i, value: nbr }) }
                    _ -> state
            )

    when rest is
        { startIdx: Some i, nbr } -> List.append rest.collected { col: i, value: nbr }
        _ -> rest.collected

neighbors : StrWithPos, I32 -> List (I32, I32)
neighbors = \{ col, value }, y ->
    f = col - 1
    t = col + (Str.countUtf8Bytes value |> Num.toI32)
    List.concat
        [(y, f), (y, t)]
        (
            List.joinMap
                (List.range { start: At f, end: At (t + 1) })
                \i -> [(y - 1, i), (y + 1, i)]
        )

part1 = \input ->
    g = parse input
    lines = Str.split input "\n"

    nums = List.map lines walkTheLine

    res = List.mapWithIndex nums \line, y ->
        List.keepIf line \num ->
            List.any (neighbors num (Num.toI32 y)) \coord -> Sym == (Dict.get g coord |> Result.map strType |> Result.withDefault Dot)

    res |> List.join |> List.map .value |> List.keepOks Str.toNat |> List.sum


part2 = \_input ->
    0

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
    r = part1 simple
    r == 4361

expect
    r = part1 simple2
    r == 4366

simple =
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """

simple2 =
    """
    467..114..
    ..5*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """

