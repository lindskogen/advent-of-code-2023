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

GameSet : { red : Nat, blue : Nat, green : Nat }
Game : { gameId : Nat, reveals : List GameSet }

emptySet : GameSet
emptySet = { red: 0, blue: 0, green: 0 }

parseReveal = \part ->
    { before: s1, after: color } <- Str.splitFirst (Str.trim part) " " |> Result.try
    count <- Str.toNat s1 |> Result.try
    Ok (color, count)

parseReveals = \s ->
    reveals = Str.split (Str.trim s) ", "

    pairs <- (List.mapTry reveals parseReveal) |> Result.try

    a = List.walk pairs emptySet \acc, (color, count) ->
        when color is
            "red" -> { acc & red: count }
            "blue" -> { acc & blue: count }
            "green" -> { acc & green: count }
            _ -> crash "unknown color \(color)"
    Ok a

power : GameSet -> Nat
power = \{ red, blue, green } ->
    red * blue * green

isPossible : GameSet -> Bool
isPossible = \{ red, blue, green } ->
    red <= 12 && green <= 13 && blue <= 14

parseInput : Str -> Result (List Game) [InvalidNumStr, NotFound]
parseInput = \input ->
    lines = Str.split input "\n"
    List.mapTry lines \line ->
        { before: beforeGameId, after: rest } <- Str.splitFirst line ": " |> Result.try
        { after: gameIdStr } <- Str.splitFirst beforeGameId " " |> Result.try
        gameId <- Str.toNat gameIdStr |> Result.try
        reveals <- (List.mapTry (Str.split rest "; ") parseReveals) |> Result.try

        Ok { gameId, reveals }

part1 = \input ->
    games <- parseInput input |> Result.try

    List.keepIf games (\{ reveals } -> List.all reveals isPossible)
    |> List.map .gameId
    |> List.sum
    |> Ok

maxCube = \c1, c2 -> {
    red: Num.max c1.red c2.red,
    green: Num.max c1.green c2.green,
    blue: Num.max c1.blue c2.blue,
}

minCubes = \reveals ->
    List.walk reveals emptySet maxCube
    |> power

part2 = \input ->
    games <- parseInput input |> Result.try
    List.sum (List.map (List.map games .reveals) minCubes) |> Ok

run =
    input <- File.readUtf8 (Path.fromStr "input") |> Task.await

    p1 <- part1 input |> Task.fromResult |> Task.await
    p2 <- part2 input |> Task.fromResult |> Task.await

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

expect part1 simple == Ok 8
expect part2 simple == Ok 2286

simple =
    """
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    """
