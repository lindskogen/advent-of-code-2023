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

parseDest = \str ->
    when str is
        "A" -> Accepted
        "R" -> Rejected
        a -> Dest a

parseProp = \str ->
    when str is
        "x" -> ExtremelyCoolLooking
        "m" -> Musical
        "a" -> Aerodynamic
        "s" -> Shiny
        _ -> crash "Invalid prop: \(str)"

parseRule = \rule ->
    if Str.contains rule ">" then
        { before: condition, after: destination } = Str.splitFirst rule ":" |> unwrap "no colon in rule"
        { before: prop, after } = Str.splitFirst condition ">" |> unwrap "failed"

        Gt (parseProp prop) (Str.toNat after |> unwrap "not a number") (parseDest destination)
    else if Str.contains rule "<" then
        { before: condition, after: destination } = Str.splitFirst rule ":" |> unwrap "no colon in rule"
        { before: prop, after } = Str.splitFirst condition "<" |> unwrap "failed"

        Lt (parseProp prop) (Str.toNat after |> unwrap "not a number") (parseDest destination)
    else
        Fallback (parseDest rule)

parseWorkflow = \str ->
    { after, before: ruleId } = Str.splitFirst str "{" |> unwrap "invalid workflow line"

    rules = Str.replaceEach after "}" "" |> Str.split "," |> List.map parseRule

    (ruleId, rules)

parseVariables = \str ->
    { after } = Str.splitFirst str "=" |> unwrap "no equals in rating part"

    Str.toNat after |> unwrap "not a number"

parseRating = \str ->
    ratings = Str.replaceEach str "{" "" |> Str.replaceEach "}" ""

    when Str.split ratings "," |> List.map parseVariables is
        [x, m, a, s] -> { x, m, a, s }
        _ -> crash "wrong number of variables"

parse = \input ->
    { before, after } = Str.splitFirst input "\n\n" |> unwrap "no groups"

    workflows = Str.split before "\n" |> List.map parseWorkflow |> Dict.fromList

    ratings = Str.split after "\n" |> List.map parseRating

    { workflows, ratings }

getProp = \rating, prop ->
    when prop is
        ExtremelyCoolLooking -> rating.x
        Musical -> rating.m
        Aerodynamic -> rating.a
        Shiny -> rating.s

part1 = \input ->
    { workflows, ratings } = parse input

    filterRating = \rating, id ->
        next =
            when Dict.get workflows id is
                Ok rules ->
                    List.walkUntil rules (Dest id) \_, rule ->
                        when rule is
                            Gt prop num dest ->
                                if (getProp rating prop) > num then
                                    Break dest
                                else
                                    Continue (Dest id)

                            Lt prop num dest ->
                                if (getProp rating prop) < num then
                                    Break dest
                                else
                                    Continue (Dest id)

                            Fallback dest -> Break dest

                _ -> crash "what is this"

        when next is
            Accepted -> Bool.true
            Rejected -> Bool.false
            Dest d -> filterRating rating d

    List.keepIf ratings (\rating -> filterRating rating "in")
    |> List.walk 0 \acc, r ->
        acc + r.x + r.m + r.a + r.s

setProp = \ranges, prop, range ->
    when prop is
        ExtremelyCoolLooking -> { ranges & x: range }
        Musical -> { ranges & m: range }
        Aerodynamic -> { ranges & a: range }
        Shiny -> { ranges & s: range }

calculatePossibilities = \{ x, m, a, s } ->
    diff = \(start, end) ->
        Num.toU64 (end + 1 - start)

    (diff x) * (diff m) * (diff a) * (diff s)

part2 = \input ->
    { workflows } = parse input

    initialRanges = { x: (1, 4000), m: (1, 4000), a: (1, 4000), s: (1, 4000) }

    recur = \parentState, node ->
        List.walk (Dict.get workflows node |> unwrap "invalid node") parentState \state, rule ->
            when rule is
                Gt prop num dest ->
                    (s, e) = getProp state.ranges prop
                    if e > num then
                        rangeInsideRule = setProp state.ranges prop (num + 1, e)
                        rangeOutsideRule = setProp state.ranges prop (s, num)

                        when dest is
                            Accepted -> { ranges: rangeOutsideRule, total: state.total + (calculatePossibilities rangeInsideRule) }
                            Dest d ->
                                newTotal = (recur { state & ranges: rangeInsideRule } d) |> .total
                                { ranges: rangeOutsideRule, total: newTotal }

                            _ -> { state & ranges: rangeOutsideRule }
                    else
                        state

                Lt prop num dest ->
                    (s, e) = getProp state.ranges prop
                    if s < num then
                        rangeInsideRule = setProp state.ranges prop (s, num - 1)
                        rangeOutsideRule = setProp state.ranges prop (num, e)

                        when dest is
                            Accepted -> { ranges: rangeOutsideRule, total: state.total + (calculatePossibilities rangeInsideRule) }
                            Dest d ->
                                newTotal = (recur { state & ranges: rangeInsideRule } d) |> .total
                                { ranges: rangeOutsideRule, total: newTotal }

                            _ -> { state & ranges: rangeOutsideRule }
                    else
                        state

                Fallback Accepted ->
                    { state & total: state.total + (calculatePossibilities state.ranges) }

                Fallback (Dest d) -> recur state d
                _ -> state

    recur { total: 0, ranges: initialRanges } "in"
    |> .total

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
    a == 19114

expect
    a = part2 simple
    a == 167409079868000

simple =
    """
    px{a<2006:qkq,m>2090:A,rfg}
    pv{a>1716:R,A}
    lnx{m>1548:A,A}
    rfg{s<537:gd,x>2440:R,A}
    qs{s>3448:A,lnx}
    qkq{x<1416:A,crn}
    crn{x>2662:A,R}
    in{s<1351:px,qqz}
    qqz{s>2770:qs,m<1801:hdj,R}
    gd{a>3333:R,R}
    hdj{m>838:A,pv}

    {x=787,m=2655,a=1222,s=2876}
    {x=1679,m=44,a=2067,s=496}
    {x=2036,m=264,a=79,s=2244}
    {x=2461,m=1339,a=466,s=291}
    {x=2127,m=1623,a=2188,s=1013}
    """
