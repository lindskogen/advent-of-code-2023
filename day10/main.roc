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

State : { prev : Array2D.Index, dists : Dict Array2D.Index I64, dist : I64 }

debug = \d ->
    dbg d

    d

updateIfLower = \r, v ->
    when r is
        Present c -> if v < c then Present v else Present c
        Missing -> Present v

walk : Array2D Str, State, Dir -> State
walk = \g, s, dir ->
    cameFrom = invertDir dir

    when indexFromDirAndPos s.prev dir is
        Ok pos ->
            c = Array2D.get g pos |> Result.withDefault "."

            if c == "S" then
                { prev: pos, dist: s.dist, dists: Dict.update s.dists pos (\r -> updateIfLower r s.dist) }
            else
                nextDir = possibleDirs c |> List.findFirst (\d -> d != cameFrom) |> unwrap "asas"

                walk g { prev: pos, dists: Dict.update s.dists pos (\r -> updateIfLower r s.dist), dist: s.dist + 1 } nextDir

        Err _ -> crash "invalid pos"

part1 = \input ->
    g = parse input

    start = findStart g |> unwrap "missing start position"

    a =
        (possibleDirs "S")
        |> List.keepIf \d ->
            when indexFromDirAndPos start d is
                Err _ -> Bool.false
                Ok i ->
                    destSym = Array2D.get g i |> Result.withDefault "."
                    incomingDirs = possibleDirs destSym

                    List.contains incomingDirs (invertDir d)

    res = List.walk a (Dict.single start 0) \dists, dir ->
        (walk g { dists, dist: 1, prev: start } dir) |> .dists

    res |> Dict.values |> List.max |> unwrap "no max value"

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
        dbg e

        Stderr.line "Something went wrong!"

expect
    a = part1 simple
    a == 8

simple =
    """
    ..F7.
    .FJ|.
    SJ.L7
    |F--J
    LJ...    
    """
