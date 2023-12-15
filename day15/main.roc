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

hashStr = \str ->
    Str.walkScalars str 0 \state, char ->
        ((state + char) * 17) % 256
    |> Num.toNat

parseStep = \str ->
    when Str.splitFirst str "=" is
        Ok { before, after } -> OpSet { label: before, focalLength: (Str.toNat after |> unwrap "failed to parse focal length") }
        _ -> OpDec { label: Str.replaceEach str "-" "" }

sumBoxes = \boxes ->
    List.walkWithIndex boxes 0 \sum, box, boxIndex ->
        List.walkWithIndex box sum \acc, (_, focalLength), lensIndex ->
            acc + (1 + boxIndex) * (1 + lensIndex) * focalLength

findInBox = \box, label ->
    List.findFirstIndex box \(v, _) -> v == label

part1 = \input ->
    Str.split input "," |> List.map hashStr |> List.sum

part2 = \input ->
    Str.split input ","
    |> List.map parseStep
    |> List.walk (List.repeat [] 256) \boxes, step ->
        when step is
            OpDec { label } ->
                hash = hashStr label
                List.update boxes hash \p ->
                    when findInBox p label is
                        Ok index -> List.dropAt p index
                        _ -> p

            OpSet { label, focalLength } ->
                hash = hashStr label

                List.update boxes hash \p ->
                    when findInBox p label is
                        Ok index -> List.set p index (label, focalLength)
                        _ -> List.append p (label, focalLength)
    |> sumBoxes

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
    a == 1320

expect
    a = part2 simple
    a == 145

simple =
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
