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
        "input" as inputFile: Str
    ]
    provides [main] to pf

strength = \s, rule ->
    when s is
        "A" -> 14
        "K" -> 13
        "Q" -> 12
        "J" ->
            when rule is
                Wildcard -> 1
                Normal -> 11

        "T" -> 10
        "9" -> 9
        "8" -> 8
        "7" -> 7
        "6" -> 6
        "5" -> 5
        "4" -> 4
        "3" -> 3
        "2" -> 2
        _ -> crash "unknown strength"

typeStr = \t ->
    when t is
        FiveOfAKind -> 7
        FourOfAKind -> 6
        FullHouse -> 5
        ThreeOfAKind -> 4
        TwoPair -> 3
        OnePair -> 2
        HighCard -> 1

countCards = \ss ->
    List.walk (Str.graphemes ss) (Dict.empty {}) \state, s ->
        Dict.update state s \res ->
            when res is
                Missing -> Present 1
                Present c -> Present (c + 1)

optimizeHand = \counts ->
    jokers = Dict.get counts "J" |> Result.withDefault 0

    if jokers > 0 && Dict.len counts > 1 then
        l =
            Dict.remove counts "J"
            |> Dict.values
            |> List.sortDesc


        List.update l 0 \c -> c + jokers
    else
        counts
        |> Dict.values
        |> List.sortDesc

type = \ss, rule ->
    counts = countCards ss

    values =
        when rule is
            Wildcard -> optimizeHand counts
            Normal -> List.sortDesc (Dict.values counts)

    when values is
        [5] -> FiveOfAKind
        [4, 1] -> FourOfAKind
        [3, 2] -> FullHouse
        [3, ..] -> ThreeOfAKind
        [2, 2, ..] -> TwoPair
        [2, ..] -> OnePair
        _ -> HighCard

parseHand = \line, rule ->
    when Str.splitFirst line " " is
        Ok { before, after } ->
            { hand: before, type: type before rule, bid: Str.toNat after |> Result.withDefault 0 }

        _ -> crash "no line"

sortHands = \hands, rule ->
    List.sortWith hands \a, b ->
        if a.type == b.type then
            zipped = List.map2 (Str.graphemes a.hand) (Str.graphemes b.hand) \aa, bb -> (aa, bb)

            List.walkUntil zipped EQ \_st, (aa, bb) ->
                when Num.compare (strength aa rule) (strength bb rule) is
                    EQ -> Continue EQ
                    v -> Break v
        else
            Num.compare (typeStr a.type) (typeStr b.type)

part1 = \input ->
    lines = Str.split input "\n"
    hands = List.map lines (\l -> parseHand l Normal)

    sortedHands = sortHands hands Normal

    List.walkWithIndex sortedHands 0 \sum, hand, idx ->
        sum + (idx + 1) * hand.bid

part2 = \input ->
    lines = Str.split input "\n"
    hands = List.map lines (\l -> parseHand l Wildcard)

    sortedHands = sortHands hands Wildcard

    List.walkWithIndex sortedHands 0 \sum, hand, idx ->
        sum + (idx + 1) * hand.bid

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
    a == 6440
expect
    a = part2 simple
    a == 5905

expect
    a = part1 inputFile
    a == 251806792

expect
    a = part2 inputFile
    a == 252113488

simple =
    """
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483
    """
