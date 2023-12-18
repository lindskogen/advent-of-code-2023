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

Dir : [North, South, East, West]

unwrap = \res, msg ->
    when res is
        Ok v -> v
        Err _ -> crash "unwrapped value \(msg)"

indexFromDirAndPosWithStep = \{ x, y }, dir, step ->
    when dir is
        North -> { y, x: x - step }
        South -> { y, x: x + step }
        East -> { y: y + step, x }
        West -> { x, y: y - step }

parseDir = \str ->
    when str is
        "0" -> East
        "R" -> East
        "1" -> South
        "D" -> South
        "2" -> West
        "L" -> West
        "3" -> North
        "U" -> North
        _ -> crash "invalid dir \(str)"

parse = \input ->
    Str.split input "\n"
    |> List.map \l ->
        when Str.split l " " is
            [d, n, _] -> { dir: parseDir d, steps: Str.toNat n |> unwrap "invalid step count" }
            _ -> crash "invalid line: \(l)"

parseHex = \str ->
    Str.walkUtf8 str 0 \acc, s ->
        acc
        * 16
        +
        when s is
            '0' -> 0
            '1' -> 1
            '2' -> 2
            '3' -> 3
            '4' -> 4
            '5' -> 5
            '6' -> 6
            '7' -> 7
            '8' -> 8
            '9' -> 9
            'a' -> 10
            'b' -> 11
            'c' -> 12
            'd' -> 13
            'e' -> 14
            'f' -> 15
            _ -> crash "out of range"

expect parseHex "ff" == 255
expect parseHex "70c71" == 461937
expect parseHex "0dc57" == 56407
expect parseHex "01523" == 5411

parseInstr2 = \str ->
    hex = Str.graphemes str
    num = List.takeFirst hex 5 |> Str.joinWith "" |> parseHex
    d = List.get hex 5 |> unwrap "no 6th char"
    { dir: parseDir d, steps: num }

parse2 = \input ->
    Str.split input "\n"
    |> List.map \l ->
        when Str.split l " " is
            [_, _, str] -> parseInstr2 (str |> Str.replaceEach "(" "" |> Str.replaceEach ")" "" |> Str.replaceEach "#" "")
            _ -> crash "invalid line: \(l)"

addAllVertices : List { dir : Dir, steps : Nat }, { x : I64, y : I64 } -> List { x : I64, y : I64 }
addAllVertices = \instrs, initialPos ->
    List.walk instrs { points: [], pos: initialPos } \{ points, pos }, { dir, steps } ->
        nextPos = indexFromDirAndPosWithStep pos dir (Num.toI64 steps)
        { points: List.append points nextPos, pos: nextPos }
    |> .points

shoelaceFormula : List { x : I64, y : I64 } -> I64
shoelaceFormula = \vertices ->
    n = List.len vertices

    area = List.walkWithIndex vertices 0 \sum, { x: x1, y: y1 }, index ->
        { x: x2, y: y2 } = List.get vertices ((index + 1) % n) |> unwrap "index issue"

        sum + (x1 * y2 - x2 * y1)

    (Num.abs area) // 2

trenchLength = \frame ->
    n = List.len frame

    List.walkWithIndex frame 0 \sum, { x: x1, y: y1 }, index ->
        { x: x2, y: y2 } = List.get frame ((index + 1) % n) |> unwrap "index issue"

        sum + Num.abs (x1 - x2) + Num.abs (y1 - y2)

part1 = \input ->
    instrs = parse input

    initialPos = { x: 0i64, y: 0i64 }

    frame = addAllVertices instrs initialPos

    len = trenchLength frame

    shoelaceFormula frame + (len // 2) + 1

part2 = \input ->
    instrs = parse2 input

    initialPos = { x: 0i64, y: 0i64 }

    frame = addAllVertices instrs initialPos

    len = trenchLength frame

    shoelaceFormula frame + len // 2 + 1

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
    a = part1 simple
    a == 62

expect
    a = part2 simple
    a == 952408144115

simple =
    """
    R 6 (#70c710)
    D 5 (#0dc571)
    L 2 (#5713f0)
    D 2 (#d2c081)
    R 2 (#59c680)
    D 2 (#411b91)
    L 5 (#8ceee2)
    U 2 (#caa173)
    L 1 (#1b58a2)
    U 2 (#caa171)
    R 2 (#7807d2)
    U 3 (#a77fa3)
    L 2 (#015232)
    U 2 (#7a21e3)
    """
