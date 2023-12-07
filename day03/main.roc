app "day03"
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

StrWithPos : { str : Str, f : I32, t : I32, y : I32 }
CharWithPos : { str : Str, x : I32, y : I32 }

State : { nums : List StrWithPos, symbols : List CharWithPos }

parse : Str -> State
parse = \input ->
    objects = { nums: [], symbols: [] }

    input
    |> Str.trim
    |> Str.split "\n"
    |> List.walkWithIndex objects walkTheLine

strType = \c ->
    when c is
        "1" -> Nbr
        "2" -> Nbr
        "3" -> Nbr
        "4" -> Nbr
        "5" -> Nbr
        "6" -> Nbr
        "7" -> Nbr
        "8" -> Nbr
        "9" -> Nbr
        "0" -> Nbr
        "." -> Dot
        s -> Sym s

Workstate : { state : State, y : I32, nbr : Str }

walkTheLine : State, Str, Nat -> State
walkTheLine = \st, line, y ->
    state = { state: st, y: Num.toI32 y, nbr: "" }

    Str.graphemes line
    |> List.walkWithIndex state parseChar
    |> addNumber (Num.toI32 (Str.countUtf8Bytes line))
    |> .state

parseChar : Workstate, Str, Nat -> Workstate
parseChar = \state, c, x ->
    when strType c is
        Nbr -> { state & nbr: Str.concat state.nbr c }
        _ ->
            state
            |> addNumber (Num.toI32 x)
            |> parseSymbol c (Num.toI32 x)

addNumber : Workstate, I32 -> Workstate
addNumber = \state, x ->
    when state.nbr is
        "" -> state
        _ ->
            len = Num.toI32 (Str.countUtf8Bytes state.nbr)
            num = { str: state.nbr, f: x - len, t: x - 1, y: state.y }
            oldState = state.state
            nums = List.append oldState.nums num
            newState = { oldState & nums }
            { state & state: newState, nbr: "" }

parseSymbol : Workstate, Str, I32 -> Workstate
parseSymbol = \state, c, x ->
    when strType c is
        Sym _ ->
            symbol = { str: c, x, y: state.y }
            oldState = state.state
            symbols = List.append oldState.symbols symbol
            newState = { oldState & symbols }
            { state & state: newState }

        Dot -> state
        _ -> crash "missed number \(c)"

numsNearSymbols : State -> List Nat
numsNearSymbols = \state ->
    state.nums
    |> List.keepIf \n -> isNearSymbol n state
    |> List.map .str
    |> List.keepOks Str.toNat

isNearSymbol : StrWithPos, State -> Bool
isNearSymbol = \n, state ->
    fromX = n.f - 1
    toX = n.t + 1
    fromY = n.y - 1
    toY = n.y + 1

    List.any state.symbols \sym ->
        sym.x >= fromX && sym.x <= toX && sym.y >= fromY && sym.y <= toY

gearRatios : State -> List Nat
gearRatios = \state ->
    state.symbols
    |> List.keepIf \s -> s.str == "*"
    |> List.map \s -> nearbyNumbers s state
    |> List.keepIf \nums -> List.len nums == 2
    |> List.map multiplyNums

nearbyNumbers : CharWithPos, State -> List StrWithPos
nearbyNumbers = \s, state ->
    fromX = s.x - 1
    toX = s.x + 1
    fromY = s.y - 1
    toY = s.y + 1

    List.keepIf state.nums \n ->
        n.t >= fromX && n.f <= toX && n.y >= fromY && n.y <= toY

multiplyNums = \nums ->
    nums
    |> List.map .str
    |> List.keepOks Str.toNat
    |> List.product

part1 = \input ->
    input
    |> parse
    |> numsNearSymbols
    |> List.sum

part2 = \input ->
    input
    |> parse
    |> gearRatios
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
        dbg
            e

        Stderr.line "Something went wrong!"

expect
    r = part1 simple
    r == 4361

expect
    r = part2 simple
    r == 467835

expect
    r = part1 simple2
    r == 4366

simple =
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """

simple2 =
    """
    467..114..
    ..5*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """

