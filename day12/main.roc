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

parseLine = \line ->
    { before, after } = Str.splitFirst line " " |> unwrap "missing space on line \(line)"
    chars = Str.toUtf8 before
    nums = Str.split after "," |> List.keepOks Str.toNat

    { chars, nums }

getNextChars = \chars, nums ->
    ccs = List.dropFirst chars nums

    when ccs is
        ['?', .. as rest] ->
            List.prepend rest '.'
        _ -> ccs

solveLine = \{ chars, nums } ->

    when chars is
        ['.', .. as rest] ->
            solveLine { chars: rest, nums }

        ['?', .. as rest] ->
            dotPath = solveLine { chars: List.prepend rest '.', nums }
            hashPath = solveLine { chars: List.prepend rest '#', nums }

            dotPath + hashPath

        ['#', .. as rest] ->
            when List.get nums 0 is
                Err _ -> 
                    0
                Ok firstNum ->
                    match = List.takeFirst chars firstNum



                    if List.len match == firstNum && List.all match \m -> m != '.' then
                        nextChars = getNextChars chars firstNum
                        if List.get nextChars 0 != Ok '#' then
                            solveLine { chars: nextChars, nums: List.dropFirst nums 1 }
                        else
                            0
                    else
                        0

        [] ->
            if List.len nums > 0 then
                0
            else
                1

        c ->
            crash "what is \(Inspect.toStr c)"

part1 = \input ->
    lines = Str.split input "\n" |> List.map parseLine

    List.map lines solveLine |> List.sum

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


expect solveLine (parseLine "???.### 1,1,3") == 1
expect solveLine (parseLine ".??..??...?##. 1,1,3") == 4
expect solveLine (parseLine "?#?#?#?#?#?#?#? 1,3,1,6") == 1
expect solveLine (parseLine "????.#...#... 4,1,1") == 1
expect solveLine (parseLine "????.######..#####. 1,6,5") == 4
expect solveLine (parseLine "?###???????? 3,2,1") == 10

expect
    a = part1 simple
    a == 21

simple =
    """
    ???.### 1,1,3
    .??..??...?##. 1,1,3
    ?#?#?#?#?#?#?#? 1,3,1,6
    ????.#...#... 4,1,1
    ????.######..#####. 1,6,5
    ?###???????? 3,2,1
    """
