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

Dir : [North, South, East, West]

shoelaceFormula : List { x : I64, y : I64 } -> I64
shoelaceFormula = \vertices ->
    n = List.len vertices

    area = List.walkWithIndex vertices 0 \sum, { x: x1, y: y1 }, index ->
        { x: x2, y: y2 } = List.get vertices ((index + 1) % n) |> unwrap "index issue"

        sum + (x1 * y2 - x2 * y1)

    (Num.abs area)

invertDir = \d ->
    when d is
        North -> South
        South -> North
        East -> West
        West -> East

possibleDirs : Str -> List Dir
possibleDirs = \s ->
    when s is
        "S" -> [North, South, East, West]
        "L" -> [North, East]
        "J" -> [North, West]
        "7" -> [South, West]
        "F" -> [South, East]
        "|" -> [North, South]
        "-" -> [East, West]
        "." -> []
        _ -> crash "invalid char \(s)"

indexFromDirAndPos = \{ x, y }, dir ->
    when dir is
        North ->
            if x == 0 then
                Err OutOfBounds
            else
                Ok { y, x: x - 1 }

        South -> Ok { y, x: x + 1 }
        East -> Ok { y: y + 1, x }
        West ->
            if y == 0 then
                Err OutOfBounds
            else
                Ok { x, y: y - 1 }

parse = \input ->
    Str.split input "\n"
    |> List.map Str.graphemes
    |> Array2D.fromLists (FitLongest ".")

unwrap = \res, msg ->
    when res is
        Ok v -> v
        Err _ -> crash "unwrapped value \(msg)"

findStart = \g ->
    Array2D.findFirstIndex g \c -> c == "S"

State : { prev: Array2D.Index, corners: List {x: I64, y: I64}, dist : I64 }

indexToI64 = \idx ->
    {x: Num.toI64 idx.x, y: Num.toI64 idx.y }

walk : Array2D Str, State, Dir -> State
walk = \g, s, dir ->
    cameFrom = invertDir dir

    when indexFromDirAndPos s.prev dir is
        Ok pos ->
            c = Array2D.get g pos |> Result.withDefault "."

            if c == "S" then
                { corners: List.append s.corners (indexToI64 pos), prev: pos, dist: s.dist }
            else
                nextDir = possibleDirs c |> List.findFirst (\d -> d != cameFrom) |> unwrap "asas"

                walk g { prev: pos, corners: List.append s.corners (indexToI64 pos), dist: s.dist + 1 } nextDir

        Err _ -> crash "invalid pos"

firstAvailableDir = \g, pos ->
    (possibleDirs "S")
    |> List.findFirst \d ->
        when indexFromDirAndPos pos d is
            Err _ -> Bool.false
            Ok i ->
                destSym = Array2D.get g i |> Result.withDefault "."
                incomingDirs = possibleDirs destSym

                List.contains incomingDirs (invertDir d)
    |> unwrap "no first valid dir"

part1 = \input ->
    g = parse input
    start = findStart g |> unwrap "missing start position"
    dir = firstAvailableDir g start

    len = (walk g { dist: 1, prev: start, corners: [] } dir) 
        |> .dist 
    
    len // 2


collectPoints = \g ->
    start = findStart g |> unwrap "missing start position"
    dir = firstAvailableDir g start
    corners = walk g { dist: 1, prev: start, corners: [] } dir
        |> .corners

    List.append corners (indexToI64 start)


part2 = \input ->
    points = List.reverse (collectPoints (parse input))
    len = Num.toI64 (List.len points)

    (shoelaceFormula points - len + 3) // 2
    
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
    a == 8

expect
    a = part2 simple2
    a == 8

expect
    a = part2 simple3
    a == 10

simple =
    """
    ..F7.
    .FJ|.
    SJ.L7
    |F--J
    LJ...    
    """

simple2 =
    """
    .F----7F7F7F7F-7....
    .|F--7||||||||FJ....
    .||.FJ||||||||L7....
    FJL7L7LJLJ||LJ.L-7..
    L--J.L7...LJS7F-7L7.
    ....F-J..F7FJ|L7L7L7
    ....L7.F7||L7|.L7L7|
    .....|FJLJ|FJ|F7|.LJ
    ....FJL-7.||.||||...
    ....L---J.LJ.LJLJ...  
    """

simple3 =
    """
    FF7FSF7F7F7F7F7F---7
    L|LJ||||||||||||F--J
    FL-7LJLJ||||||LJL-77
    F--JF--7||LJLJ7F7FJ-
    L---JF-JLJ.||-FJLJJ7
    |F|F-JF---7F7-L7L|7|
    |FFJF7L7F-JF7|JL---7
    7-L-JL7||F7|L7F-7F7|
    L.L7LFJ|||||FJL7||LJ
    L7JLJL-JLJLJL--JLJ.L
    ....L---J.LJ.LJLJ...  
    """
