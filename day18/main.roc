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

indexFromDirAndPos = \{ x, y }, dir ->
    when dir is
        North -> { y, x: x - 1 }
        South -> { y, x: x + 1 }
        East -> { y: y + 1, x }
        West -> { x, y: y - 1 }

parseDir = \str ->
    when str is 
        "R" -> East
        "L" -> West
        "U" -> North
        "D" -> South
        "0" -> East
        "1" -> South
        "2" -> West
        "3" -> North
        _ -> crash "invalid dir \(str)"

parse = \input ->
    Str.split input "\n" 
    |> List.map \l -> 
        when Str.split l " " is
            [d, n, _] -> { dir: parseDir d, steps: Str.toNat n |> unwrap "invalid step count" }
            _ -> crash "invalid line: \(l)"

parseHex = \str ->
    len = Str.countUtf8Bytes str

    Str.walkUtf8WithIndex str 0 \acc, s, i ->
        acc + 16 * (len - i) * when s is
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

parseInstr2 = \str ->
    hex = Str.graphemes str
    num = List.takeFirst hex 5 |> Str.joinWith "" |> parseHex
    d = List.get hex 5 |> unwrap "no 6th char"
    { dir: parseDir d, steps: num }

parse2 = \input ->
    Str.split input "\n" 
    |> List.map \l -> 
        when Str.split l " " is
            [_, _, str] -> parseInstr2 (str |>  Str.replaceEach "(" "" |>  Str.replaceEach ")" "" |>  Str.replaceEach "#" "")
            _ -> crash "invalid line: \(l)"

addAllInstrs = \instrs, initialPos ->
    List.walk instrs {points: Set.single initialPos, pos: initialPos} \parentState, instr ->
        List.walk (List.repeat instr.dir instr.steps) parentState \{ points, pos }, dir ->
            nextPos = indexFromDirAndPos pos dir
            { points: Set.insert points nextPos, pos: nextPos }

calculateShape = \frame ->
    Set.walk frame { maxX: 0i32, maxY: 0i32 } \{ maxX, maxY }, key ->
        { 
            maxY: if key.y > maxY then key.y else maxY, 
            maxX: if key.x > maxX then key.x else maxX, 
        }

debugFrame = \frame, shape ->
    xs = List.range { start: At 0, end: At shape.maxX }
    ys = List.range { start: At 0, end: At shape.maxY }

    _ = List.walk xs {} \_, x ->
        r = List.walk ys [] \list, y ->
                List.append list (if Set.contains frame {x, y} then "#" else ".")
            |> Str.joinWith ""
        dbg r
        {}
    {}

moveToInsideFrame = \{ x, y } ->
    {x: x + 1, y: y + 1}

floodFill = \frame, node ->
    if Set.contains frame node then 
        frame
    else
        List.walk  [North, South, East, West] (Set.insert frame node) \fr, dir ->
            floodFill fr (indexFromDirAndPos node dir)


part1 = \input ->
    instrs = parse input 

    initialPos = { x: 0i32, y: 0i32 }

    frame = addAllInstrs instrs initialPos |> .points

    shape = calculateShape frame
    
    lagoon = floodFill frame (moveToInsideFrame initialPos)

    Set.len lagoon



part2 = \input ->
    instrs = parse2 input 

    initialPos = { x: 0i32, y: 0i32 }

    frame = addAllInstrs instrs initialPos |> .points

    dbg "frame done"

    shape = calculateShape frame

    dbg "shape done"

    lagoon = floodFill frame (moveToInsideFrame initialPos)

    dbg "fill done"

    Set.len lagoon

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
