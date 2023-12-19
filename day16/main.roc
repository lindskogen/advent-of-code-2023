app "day16"
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

Dir : [East, North, South, West]
WorkCache : Set (Array2D.Index, Dir)

unwrap = \res, msg ->
    when res is
        Ok v -> v
        Err _ -> crash "unwrapped value \(msg)"

parse = \input ->
    Str.split input "\n"
    |> List.map Str.toUtf8
    |> Array2D.fromLists (FitLongest '.')

parseDir = \g, pos, dir ->
    when Array2D.get g pos is
        Ok '.' -> Step dir
        Ok '|' ->
            when dir is
                East | West -> Split North South
                _ -> Step dir

        Ok '-' ->
            when dir is
                North | South -> Split East West
                _ -> Step dir

        Ok '/' ->
            when dir is
                North -> Step East
                South -> Step West
                East -> Step North
                West -> Step South

        Ok '\\' ->
            when dir is
                North -> Step West
                West -> Step North
                South -> Step East
                East -> Step South

        _ -> End

splitBeam : Array2D U8, WorkCache, Array2D.Index, Dir, Dir -> WorkCache
splitBeam = \g, energizedTiles, pos, d1, d2 ->
    newVisited =
        when indexFromDirAndPos pos d1 is
            Ok newPos -> followBeam g energizedTiles newPos d1
            Err _ -> energizedTiles

    when indexFromDirAndPos pos d2 is
        Ok newPos -> followBeam g newVisited newPos d2
        Err _ -> newVisited

followBeam : Array2D U8, WorkCache, Array2D.Index, Dir -> WorkCache
followBeam = \g, energizedTiles, pos, dir ->
    if Set.contains energizedTiles (pos, dir) then
        energizedTiles
    else
        nextDir = parseDir g pos dir

        when nextDir is
            Step d ->
                newVisited = Set.insert energizedTiles (pos, dir)
                when indexFromDirAndPos pos d is
                    Err _ -> newVisited
                    Ok newPos -> followBeam g newVisited newPos d

            Split d1 d2 ->
                newVisited = Set.insert energizedTiles (pos, dir)
                splitBeam g newVisited pos d1 d2

            End -> energizedTiles

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

        _ -> Err OutOfBounds

countUniq = \set ->
    set
    |> Set.map \(p, _) -> p
    |> Set.len

part1 = \input ->
    g = parse input
    pos = { x: 0, y: 0 }
    initialDir = East

    followBeam g (Set.empty {}) pos initialDir
    |> countUniq

part2 = \input ->
    g = parse input
    { dimY, dimX } = Array2D.shape g

    east =
        List.range { start: At 0, end: Before dimX }
        |> List.map (\i -> followBeam g (Set.empty {}) { x: i, y: 0 } East |> countUniq)
        |> List.max
        |> unwrap "East: no max"

    west =
        List.range { start: At 0, end: Before dimX }
        |> List.map (\i -> followBeam g (Set.empty {}) { x: i, y: (dimY - 1) } West |> countUniq)
        |> List.max
        |> unwrap "West: no max"

    north =
        List.range { start: At 0, end: Before dimY }
        |> List.map (\i -> followBeam g (Set.empty {}) { x: (dimX - 1), y: i } North |> countUniq)
        |> List.max
        |> unwrap "North: no max"

    south =
        List.range { start: At 0, end: Before dimY }
        |> List.map (\i -> followBeam g (Set.empty {}) { x: 0, y: i } South |> countUniq)
        |> List.max
        |> unwrap "South: no max"

    [east, west, north, south] |> List.max |> unwrap "Total: no max"

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
    a == 46

expect
    a = part2 simple
    a == 51

simple =
    """
    .|...\\....
    |.-.\\.....
    .....|-...
    ........|.
    ..........
    .........\\
    ..../.\\\\..
    .-.-/..|..
    .|....-|.\\
    ..//.|....
    """
