app "day11"
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
    graph =
        Str.split input "\n"
        |> List.map Str.graphemes
        |> Array2D.fromLists (FitLongest ".")

    galaxies = Array2D.walk graph [] { direction: Forwards, orientation: Rows } \list, elem, index ->
        if elem == "#" then
            List.append list index
        else
            list

    emptyRows =
        Array2D.toLists graph
        |> List.walkWithIndex [] \list, row, index ->
            if List.all row \r -> r == "." then
                List.append list index
            else
                list

    emptyCols =
        Array2D.transpose graph
        |> Array2D.toLists
        |> List.walkWithIndex [] \list, col, index ->
            if List.all col \c -> c == "." then
                List.append list index
            else
                list

    indexedGalaxies = List.mapWithIndex galaxies \g, i ->
        { x: g.x, y: g.y, index: i + 1 }

    { galaxies: indexedGalaxies, emptyCols, emptyRows }

calculateDistance = \g, expansionRate ->
    r = List.walkWithIndex g.galaxies (Dict.empty {}) \ss, g1, outerIndex ->
        List.walk (List.dropFirst g.galaxies (outerIndex + 1)) ss \dict, g2 ->
            xs = Num.toI64 (List.range { start: At g1.x, end: At g2.x } |> List.countIf \i -> List.contains g.emptyRows i)
            ys = Num.toI64 (List.range { start: At g1.y, end: At g2.y } |> List.countIf \i -> List.contains g.emptyCols i)

            distX = Num.abs (Num.toI64 (g2.x) - Num.toI64 (g1.x))
            distY = Num.abs (Num.toI64 (g2.y) - Num.toI64 (g1.y))

            dist = (distX - xs) + (xs * expansionRate) + (distY - ys) + (ys * expansionRate)

            Dict.insert dict (g1.index, g2.index) dist

    Dict.values r |> List.sum

part1 = \input ->
    g = parse input

    calculateDistance g 2

part2 = \input ->
    g = parse input

    calculateDistance g 1000000

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
    a == 374

expect
    a = part1 simple
    a == 374

simple =
    """
    ...#......
    .......#..
    #.........
    ..........
    ......#...
    .#........
    .........#
    ..........
    .......#..
    #...#.....
    """
