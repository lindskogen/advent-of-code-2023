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
                    parsedNums = Str.split numbers " " |> List.map Str.trim |> List.keepOks Str.toNat
                    parsedWins = Str.split wins " " |> List.map Str.trim |> List.keepOks Str.toNat |> Set.fromList
                    { id, nums: parsedNums, wins: parsedWins }

                _ -> crash ""

        _ -> crash ""

part1 = \input ->
    cards = List.map (Str.split input "\n") parse

    cardScore = List.map cards \{ nums, wins } ->
        count = List.countIf nums \n -> Set.contains wins n

        when count is
            0 -> 0
            1 -> 1
            c -> Num.powInt 2 (c - 1)

    List.sum cardScore

recur = \lookup, id ->
    cards = Dict.get lookup id |> Result.withDefault []
    1 + List.sum (List.map cards \c -> recur lookup c)

part2 = \input ->
    cards = List.map (Str.split input "\n") parse

    maxLen = List.len cards

    lookup = List.walk
        cards
        (Dict.empty {})
        \state, { id, nums, wins } ->

            count = List.countIf nums \n -> Set.contains wins n

            last = Num.min maxLen (id + count)

            additionalCards = List.range { start: After id, end: At last }

            Dict.insert state id additionalCards

    { numCards } = Dict.walk
        lookup
        { numCards: 0, cache: Dict.empty {} }
        \state, id, _ ->
            c = Dict.get state.cache id
            when c is
                Ok v -> { state & numCards: (state.numCards + v) }
                _ ->
                    extraCards = recur lookup id
                    { state & numCards: (state.numCards + extraCards), cache: Dict.insert state.cache id extraCards }

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
