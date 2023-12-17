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

parseLine = \line, repeatCount ->
    { before, after } = Str.splitFirst line " " |> unwrap "missing space on line \(line)"
    chars = Str.toUtf8 (Str.joinWith (List.repeat before repeatCount) "?")
    nums = Str.split after "," |> List.keepOks Str.toNat |> List.repeat repeatCount |> List.join

    { chars, nums }

getNextChars = \chars, nums ->
    ccs = List.dropFirst chars nums

    when ccs is
        ['?', .. as rest] ->
            List.prepend rest '.'
        _ -> ccs
    
solveLine: { chars: List U8, nums: List Nat }, Cache -> {value: Nat, map: Cache}
solveLine = \{chars, nums}, map ->
    insertNum = \value ->
        { value, map: Dict.insert map (chars, nums) value }

    when chars is
        ['.', .. as rest] ->
            cachedSolveLine { chars: rest, nums } map 

        ['?', .. as rest] ->
            dotPath = cachedSolveLine { chars: List.prepend rest '.', nums } map 
            hashPath = cachedSolveLine { chars: List.prepend rest '#', nums } dotPath.map 
            v = dotPath.value + hashPath.value
            {value: v, map: (Dict.insert hashPath.map (chars, nums) v) }
            

        ['#', .. as rest] ->
            when List.get nums 0 is
                Err _ -> 
                    insertNum 0
                Ok firstNum ->
                    match = List.takeFirst chars firstNum



                    if List.len match == firstNum && List.all match \m -> m != '.' then
                        nextChars = getNextChars chars firstNum
                        if List.get nextChars 0 != Ok '#' then
                            cachedSolveLine { chars: nextChars, nums: List.dropFirst nums 1 } map 
                        else
                            insertNum 0
                    else
                        insertNum 0

        [] ->
            if List.len nums > 0 then
                insertNum 0
            else
                insertNum 1

        c ->
            crash "what is \(Inspect.toStr c)"

Cache: Dict (List U8, List Nat) Nat

cachedSolveLine: { chars: List U8, nums: List Nat }, Cache -> { value: Nat, map: Cache }
cachedSolveLine = \{ chars, nums }, map ->
    when Dict.get map (chars, nums) is
        Ok v ->
            {value: v, map}
        _ -> 
            {map: newMap, value: v } = solveLine { chars, nums } map

            {value: v, map: (Dict.insert newMap (chars, nums) v) }

part1 = \input ->
    Str.split input "\n" 
    |> List.map \l -> parseLine l 1
    |> List.map \l -> (cachedSolveLine l (Dict.empty {}) |> .value)
    |> List.sum

part2 = \input ->
    Str.split input "\n" 
    |> List.map \l -> parseLine l 5
    |> List.map \l -> (cachedSolveLine l (Dict.empty {}) |> .value)
    |> List.sum

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


expect part1 "???.### 1,1,3" == 1
expect part1 ".??..??...?##. 1,1,3" == 4
expect part1 "?#?#?#?#?#?#?#? 1,3,1,6" == 1
expect part1 "????.#...#... 4,1,1" == 1
expect part1 "????.######..#####. 1,6,5" == 4
expect part1 "?###???????? 3,2,1" == 10

expect part2 "???.### 1,1,3" == 1
expect part2 ".??..??...?##. 1,1,3" == 16384
expect part2 "?#?#?#?#?#?#?#? 1,3,1,6" == 1
expect part2 "????.#...#... 4,1,1" == 16
expect part2 "????.######..#####. 1,6,5" == 2500
expect part2 "?###???????? 3,2,1" == 506250

expect
    a = part1 simple
    a == 21

expect
    a = part2 simple
    a == 525152

simple =
    """
    ???.### 1,1,3
    .??..??...?##. 1,1,3
    ?#?#?#?#?#?#?#? 1,3,1,6
    ????.#...#... 4,1,1
    ????.######..#####. 1,6,5
    ?###???????? 3,2,1
    """
