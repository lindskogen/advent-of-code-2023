app "day04"
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

parse = \s ->
    when Str.split s ": " is
        [cardIdStr, data] ->
            id =
                when Str.split cardIdStr "Card " is
                    [_, idStr] -> Str.trim idStr |> Str.toNat |> Result.withDefault 0
                    _ -> crash "id str: \(cardIdStr)"
            when Str.split data " | " is
                [wins, numbers] ->
                    parsedNums = Str.split numbers " " |> List.map Str.trim |> List.keepOks Str.toNat |> Set.fromList
                    parsedWins = Str.split wins " " |> List.map Str.trim |> List.keepOks Str.toNat |> Set.fromList
                    { id, nums: parsedNums, wins: parsedWins }

                _ -> crash ""

        _ -> crash ""

numberOfWins = \{ nums, wins } ->
    Set.intersection wins nums |> Set.len

part1 = \input ->
    cards = List.map (Str.split input "\n") parse

    cardScore = List.map cards \card ->
        count = numberOfWins card

        when count is
            0 -> 0
            c -> Num.powInt 2 (c - 1)

    List.sum cardScore

recur = \lookup, id ->
    cards = Dict.get lookup id |> Result.withDefault []
    1 + List.sum (List.map cards \c -> recur lookup c)

getOrCalculate : Dict a b, a, ({} -> b) -> { updatedCache : Dict a b, value : b }
getOrCalculate = \cache, id, calculateFn ->
    when Dict.get cache id is
        Ok v ->
            { updatedCache: cache, value: v }

        Err KeyNotFound ->
            v = calculateFn {}
            { updatedCache: Dict.insert cache id v, value: v }

recurCached = \lookup, id, cache ->
    getOrCalculate cache id \_ ->
        recur lookup id

part2 = \input ->
    cards = List.map (Str.split input "\n") parse

    maxLen = List.len cards

    cardIds = List.map cards .id |> List.reverse

    lookup = List.walk
        cards
        (Dict.empty {})
        \state, { id, nums, wins } ->

            count = numberOfWins { nums, wins }

            last = Num.min maxLen (id + count)

            additionalCards = List.range { start: After id, end: At last }

            Dict.insert state id additionalCards

    { numCards } = List.walk
        cardIds
        { numCards: 0, cache: Dict.empty {} }
        \state, id ->
            { value, updatedCache } = recurCached lookup id state.cache

            { state & numCards: (state.numCards + value), cache: updatedCache }

    numCards

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
    a == 13

expect
    a = part2 simple
    a == 30

simple =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """
