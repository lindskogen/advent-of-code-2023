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

parse = \input ->
    Str.split input "\n"
    |> List.map Str.graphemes
    |> Array2D.fromLists (FitLongest ".")

reflectOnMirror = \str, dir ->
    when str is
        "/" ->
            when dir is
                North -> East
                South -> West
                East -> North
                West -> South

        "\\" ->
            when dir is
                North -> West
                West -> North
                South -> East
                East -> South

        _ -> crash "invalid str \(str)"

Dir : [East, North, South, West]
WorkCache : Set (Array2D.Index, Dir)

splitBeam : Array2D Str, WorkCache, Array2D.Index, Dir, Dir -> WorkCache
splitBeam = \g, energizedTiles, pos, d1, d2 ->
    g1 = followBeam g (Set.insert energizedTiles (pos, d1)) pos d1
    followBeam g (Set.insert g1 (pos, d2)) pos d2

followBeam : Array2D Str, WorkCache, Array2D.Index, Dir -> WorkCache
followBeam = \g, energizedTiles, pos, dir ->

    when indexFromDirAndPos pos dir is
        Err _ -> energizedTiles
        Ok newPos ->
            if Set.contains energizedTiles (newPos, dir) then
                energizedTiles
            else
                when (Array2D.get g newPos, dir) is
                    (Ok ".", _) -> followBeam g (Set.insert energizedTiles (newPos, dir)) newPos dir
                    (Ok "/", _) -> followBeam g (Set.insert energizedTiles (newPos, dir)) newPos (reflectOnMirror "/" dir)
                    (Ok "\\", _) -> followBeam g (Set.insert energizedTiles (newPos, dir)) newPos (reflectOnMirror "\\" dir)
                    (Ok "|", North) -> followBeam g (Set.insert energizedTiles (newPos, dir)) newPos dir
                    (Ok "|", South) -> followBeam g (Set.insert energizedTiles (newPos, dir)) newPos dir
                    (Ok "-", East) -> followBeam g (Set.insert energizedTiles (newPos, dir)) newPos dir
                    (Ok "-", West) -> followBeam g (Set.insert energizedTiles (newPos, dir)) newPos dir
                    (Ok "|", East) ->
                        splitBeam g energizedTiles newPos South North

                    (Ok "|", West) ->
                        splitBeam g energizedTiles newPos South North

                    (Ok "-", North) ->
                        splitBeam g energizedTiles newPos East West

                    (Ok "-", South) ->
                        splitBeam g energizedTiles newPos East West

                    (Err _, _) ->
                        energizedTiles

                    (a, b) ->
                        dbg (a, b)

                        crash "unhandled case"

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

part1 = \input ->
    g = parse input
    pos = { x: 0, y: 0 }
    initialDir = East

    followBeam g (Set.single (pos, initialDir)) pos initialDir
    |> Set.map \(p, _) -> p
    |> Set.len

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
    a == 46

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
