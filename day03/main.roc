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

    maxY = List.len lines
    maxX =
        when lines is
            [l1, ..] -> Str.countUtf8Bytes l1
            _ -> crash "Lines is empty"

    dict =
        (
            List.mapWithIndex lines \l, i ->
                List.mapWithIndex (Str.graphemes l) \c, j ->
                    ((Num.toI32 i, Num.toI32 j), c)
        )
        |> List.join
        |> Dict.fromList

    ((Num.toI32 maxX, Num.toI32 maxY), dict)

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
    l = Num.toI32 (Str.countUtf8Bytes nbr) + 1
    List.range { start: At f, end: At (f + l) }
    |> List.findFirst \x ->
        List.any (getIndices f (f + l) x y) \coord ->
            dbg
                (x, f, (f + l), coord, Dict.get g coord)

            r = Dict.get g coord |> Result.map strType |> Result.withDefault Dot
            r == Sym
    |> Result.isOk

State : { nbr : Str, startIdx : [Some I32, None], row : I32, collected : List Str }

Graph : Dict (I32, I32) Str

walkLine : State, Graph, (I32, I32) -> State
walkLine = \state, g, (x, y) ->
    c = Dict.get g (y, x) |> Result.withDefault "."
    nextC = Dict.get g (y, x + 1) |> Result.withDefault "."

    dbg
        (state, c, nextC)

    when (state, strType c, strType nextC) is
        ({ startIdx: None }, Nbr, _) -> { state & nbr: c, startIdx: Some x, row: y }
        ({ nbr, startIdx: Some _ }, Nbr, Nbr) -> { state & nbr: Str.concat nbr c }
        ({ nbr, startIdx: Some i }, Nbr, _) if findNeigbors g i (Str.concat nbr c) y ->
            { state & nbr: "", startIdx: None, collected: List.append state.collected (Str.concat nbr c) }

        _ -> { state & nbr: "", startIdx: None }

part1 = \input ->
    ((maxX, maxY), g) = parse input

    collected =
        List.joinMap
            (
                List.range { start: At 0, end: Length (Num.toNat maxY) }
            )
            \y ->
                (
                    List.walk
                        (List.range { start: At 0, end: Length (Num.toNat maxX) })
                        {
                            nbr: "",
                            startIdx: None,
                            row: 0,
                            collected: [],
                        }
                        \state, x -> walkLine state g (x, y))
                |> .collected

    # dbg
    #     collected

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
