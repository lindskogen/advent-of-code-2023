app "day01"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.File,
        pf.Path,
        pf.Task.{ Task },
    ]
    provides [main] to pf

isNum = \c ->
    when c is
        "1" -> Bool.true
        "2" -> Bool.true
        "3" -> Bool.true
        "4" -> Bool.true
        "5" -> Bool.true
        "6" -> Bool.true
        "7" -> Bool.true
        "8" -> Bool.true
        "9" -> Bool.true
        "0" -> Bool.true
        _ -> Bool.false

convertNum = \c ->
    Str.replaceEach c "one" "one1one" 
        |> Str.replaceEach "two" "two2two" 
        |> Str.replaceEach "three" "three3three" 
        |> Str.replaceEach "four" "four4four" 
        |> Str.replaceEach "five" "five5five" 
        |> Str.replaceEach "six" "six6six" 
        |> Str.replaceEach "seven" "seven7seven" 
        |> Str.replaceEach "eight" "eight8eight" 
        |> Str.replaceEach "nine" "nine9nine" 

getInts = \s ->
    list = Str.graphemes s

    aa = List.findFirst list isNum |> Result.withDefault "0" 
    bb = List.findLast list isNum |> Result.withDefault "0" 


    Str.toNat "\(aa)\(bb)" |> Result.withDefault 0

part1 = \input -> 
    Str.trim input |> Str.split "\n" |> List.map getInts |> List.sum


part2 = \input ->
    Str.trim input |> Str.split "\n" |> List.map convertNum |> List.map getInts |> List.sum

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
    a == 281


simple = 
    """
    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen
    """