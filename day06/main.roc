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

parseNums = \s ->
    Str.split s " " |> List.dropFirst 1
     |> List.keepOks \n ->(n |> Str.trim |> Str.toNat)

parse = \input ->
    s = Str.replaceEach (Str.replaceEach input "  " " ") "  " " "

    (ts, ds) = when Str.split s "\n" is
        [a, b] -> (parseNums a, parseNums b)
        _ -> crash "failed to split lines"
    
    List.map2 ts ds \t, d ->
        (t, d)

parse2 = \input ->
    trimmed = Str.replaceEach input " " ""

    lines = Str.split trimmed "\n" 
        |> List.joinMap (\s -> Str.split s ":" |> List.dropFirst 1) 
        |> List.keepOks Str.toNat

    (t, d) = when lines is
        [a, b] -> (a, b)
        _ -> crash "failed to split lines"

    (t, d)

countWays = \(t, d) ->
    List.range { start: After 0, end: Before t }
    |> List.countIf \i -> (t - i) * i > d

part1 = \input ->
    races = parse input
    List.product (List.map races countWays)

part2 = \input ->
    race = parse2 input

    countWays race

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
    a == 288

expect
    a = part2 simple
    a == 71503

simple =
    """
    Time:      7  15   30
    Distance:  9  40  200
    """

