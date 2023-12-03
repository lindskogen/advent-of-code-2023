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

getIndices = \f, t, x, y ->
    if x == f then
        [(y - 1, x), (y + 1, x), (y, x - 1), (y - 1, x - 1), (y + 1, x - 1)]
    else if x == t then
        [(y - 1, x), (y + 1, x), (y, x + 1), (y - 1, x + 1), (y + 1, x + 1)]
    else
        [(y - 1, x), (y + 1, x)]

findNeigbors = \g, f, nbr, y ->
    l = Num.toI32 (Str.countUtf8Bytes nbr)+1
    List.range { start: At f, end: At (f+l) }
    |> List.findFirst \x ->
        List.any (getIndices f (f + l) x y) \coord ->
            dbg (x, f, (f + l), coord, Dict.get g coord)
            r = Dict.get g coord |> Result.map strType |> Result.withDefault Dot
            r == Sym
    |> Result.isOk

part1 = \input ->
    g = parse input

    { collected } = Dict.walk g ({ nbr: "", startIdx: None, row: 0, collected: [] }) \st, (y, x), c ->
        dbg (st, c, y)

        state =
            when st is
                { nbr, startIdx: Some i } if st.row != y -> 
                    if findNeigbors g i nbr y then
                        { st & startIdx: None, nbr: "", collected: List.append st.collected nbr }
                    else
                        { st & startIdx: None, nbr: "" }

                _ -> st

        newState =
            when (state, strType c) is
                ({ startIdx: None }, Nbr) -> { state & nbr: c, startIdx: Some x, row: y }
                ({ nbr, startIdx: Some _ }, Nbr) -> { state & nbr: Str.concat nbr c }
                ({ nbr, startIdx: Some i }, _) if findNeigbors g i nbr y ->
                    { state & nbr: "", startIdx: None, collected: List.append state.collected nbr }

                _ -> { state & nbr: "", startIdx: None }

        dbg
            newState

        newState

    dbg
        collected

    oks = List.keepOks collected Str.toNat

    expect List.len oks == List.len collected

    oks |> List.sum

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

simple =
    """
    467..114..
    ...*......
    ..35..633.
    ......#..5
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """
