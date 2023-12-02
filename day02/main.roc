app "day02"
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

parseReveals = \s ->
    reveals = Str.split (Str.trim s) ", "

    pairs = List.map reveals \part ->
        when Str.split (Str.trim part) " " is
            [s1, color] ->
                count = Str.toNat s1 |> Result.withDefault 0
                (color, count)

            _ -> crash "invalid"

    dict = Dict.fromList pairs

    {
        red: (Dict.get dict "red") |> Result.withDefault 0,
        blue: (Dict.get dict "blue") |> Result.withDefault 0,
        green: (Dict.get dict "green") |> Result.withDefault 0,
    }

power = \{ red, blue, green } ->
    red * blue * green

isPossible = \{ red, blue, green } ->
    red <= 12 && green <= 13 && blue <= 14

parseInput = \input ->
    lines = Str.split input "\n"
    List.map lines \line ->
        gameIdAndRest = Str.split line ": "
        gameId = Str.toNat ((List.get (Str.split (List.get gameIdAndRest 0 |> Result.withDefault "") " ") 1) |> Result.withDefault "") |> Result.withDefault 0
        reveals = List.map (Str.split (List.get gameIdAndRest 1 |> Result.withDefault "") "; ") parseReveals

        (gameId, reveals)

part1 = \input ->
    games = parseInput input

    List.keepIf games (\(_gameId, reveals) -> List.all reveals isPossible)
    |> List.map \(gameId, _) -> gameId
    |> List.sum

maxCube = \prev, elem -> {
    red: Num.max prev.red elem.red,
    green: Num.max prev.green elem.green,
    blue: Num.max prev.blue elem.blue,
}

minCubes = \reveals ->
    (
        List.walk
            reveals
            { red: 0, blue: 0, green: 0 }
            maxCube
    )
    |> power

part2 = \input ->
    games = parseInput input
    List.sum (List.map (List.map games \(_, reveals) -> reveals) minCubes)

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

expect part1 simple == 8
expect part2 simple == 2286

simple =
    """
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    """
