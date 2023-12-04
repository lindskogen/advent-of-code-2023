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

    nums = List.map lines walkTheLine

    g =
        (
            List.mapWithIndex lines \l, i ->
                List.mapWithIndex (Str.graphemes l) \c, j ->
                    ((Num.toI32 i, Num.toI32 j), c)
        )
        |> List.join
        |> Dict.fromList

    (g, nums)

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
        s -> Sym s

debug = \v ->
    dbg
        v

    v

StrWithPos : { col : I32, value : Str }

State : { nbr : Str, startIdx : [Some I32, None], collected : List StrWithPos }

emptyState : State
emptyState = { nbr: "", startIdx: None, collected: [] }

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

isSym = \s ->
    when s is
        Sym _ -> Bool.true
        _ -> Bool.false

part1 = \input ->
    (g, nums) = parse input

    res = List.mapWithIndex nums \line, y ->
        List.keepIf line \num ->
            List.any (neighbors num (Num.toI32 y)) \coord -> isSym (Dict.get g coord |> Result.map strType |> Result.withDefault Dot)

    res |> List.join |> List.map .value |> List.keepOks Str.toNat |> List.sum

part2 = \input ->
    (g, nums) = parse input

    res =

        List.walkWithIndex nums (Dict.empty {}) \state, line, y ->
            List.walk line state \pos, num ->

                List.walkUntil (neighbors num (Num.toI32 y)) pos \collected, coord ->
                    when (Dict.get g coord |> Result.map strType |> Result.withDefault Dot) is
                        Sym "*" ->
                            Break
                                (
                                    Dict.update collected coord \found ->
                                        when found is
                                            Missing -> Present ([num.value])
                                            Present vs -> Present (List.append vs num.value)
                                )

                        Sym _ -> Break collected
                        _ -> Continue collected

    res
    |> Dict.walk 0 \acc, _, vs ->
        when vs is
            [_, _] -> acc + List.product (List.keepOks vs Str.toNat)
            _ -> acc

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
    r = part2 simple
    r == 467835

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

