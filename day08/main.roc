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

parseRow = \row ->
    when Str.split row " = (" is
        [id, rest] ->
            when Str.split rest ", " is
                [left, r] ->
                    right = Str.replaceEach r ")" ""
                    (id, {left, right})
                _ -> crash "split ,"
        _ -> crash "split = ("


parse = \input ->
    when Str.split input "\n\n" is
        [directions, rest] ->
            nodes = Str.split rest "\n"
            dirs = Str.graphemes directions
            d = Dict.fromList (List.map nodes parseRow)

            (dirs, d)
        _ -> crash "split nn"

unwrap = \res, msg ->
    when res is
        Ok v -> v
        Err _ -> crash "unwrapped value \(msg)"

walkDirs = \st, dirs, isEnd, g ->
    List.walkUntil dirs st \{ steps, loc }, dir ->
        next = step loc dir g
        if isEnd next then 
            Break {loc: next, steps: steps + 1}
        else 
            Continue {loc: next, steps: steps + 1}

loopDirs = \st, dirs, isEnd, g ->
    endState = walkDirs st dirs isEnd g

    if isEnd endState.loc then
        endState
    else
        loopDirs endState dirs isEnd g

step = \loc, dir, g ->
    when dir is
        "L" -> Dict.get g loc |> unwrap "L" |> .left 
        "R" -> Dict.get g loc |> unwrap "R" |> .right
        _ -> crash "unknown dir: \(dir)"

isEnd1 = \s ->
    s == "ZZZ"

isEnd2 = \s ->
    Str.endsWith s "Z"

part1 = \input ->
    (dirs, graph) = parse input

    initial = "AAA"

    loopDirs {steps: 0, loc: initial } dirs isEnd1 graph
        |> .steps



part2 = \input ->
    (dirs, graph) = parse input

    startingNodes = Dict.keys graph |> List.keepIf \s -> Str.endsWith s "A"

    dists = startingNodes |> List.map \n -> loopDirs {steps: 0, loc: n } dirs isEnd2 graph 
        |> .steps

    walkLcm dists

gcd: Nat, Nat -> Nat
gcd = \a, b ->
    if b == 0 then
        a
    else
        gcd b (a % b)

lcm: Nat, Nat -> Nat
lcm = \a, b ->
    Num.round (Num.toF64 (Num.abs (a * b)) / Num.toF64 (gcd a b))

walkLcm = \aas ->
    when aas is
        [prefix, .. as rest] ->
            List.walk rest prefix lcm
        _ -> 0


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
    a == 6

expect
    a = part2 simple2
    a == 6

simple =
    """
    LLR

    AAA = (BBB, BBB)
    BBB = (AAA, ZZZ)
    ZZZ = (ZZZ, ZZZ)
    """

simple2 =
    """
    LR

    11A = (11B, XXX)
    11B = (XXX, 11Z)
    11Z = (11B, XXX)
    22A = (22B, XXX)
    22B = (22C, 22C)
    22C = (22Z, 22Z)
    22Z = (22B, 22B)
    XXX = (XXX, XXX)
    """