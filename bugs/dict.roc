app "dict"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
    ]
    provides [main] to pf

# repro: for Dict.update multiplication overflow

main =

    list = [
        ({ x: 2, y: 0 }, 16),
        ({ x: 3, y: 0 }, 1),
        ({ x: 4, y: 0 }, 2),
        ({ x: 4, y: 1 }, 3),
        ({ x: 3, y: 1 }, 4),
        ({ x: 3, y: 2 }, 5),
        ({ x: 3, y: 3 }, 6),
        ({ x: 3, y: 4 }, 7),
        ({ x: 2, y: 4 }, 8),
        ({ x: 2, y: 3 }, 9),
        ({ x: 1, y: 3 }, 10),
        ({ x: 0, y: 3 }, 11),
        ({ x: 0, y: 2 }, 12),
        ({ x: 1, y: 2 }, 13),
        ({ x: 1, y: 1 }, 14),
        ({ x: 2, y: 1 }, 15),
        ({ x: 2, y: 0 }, 16),
        ({ x: 2, y: 1 }, 1),
        ({ x: 1, y: 1 }, 2),
        ({ x: 1, y: 2 }, 3),
        ({ x: 0, y: 2 }, 4),
        ({ x: 0, y: 3 }, 5),
        ({ x: 1, y: 3 }, 6),
        ({ x: 2, y: 3 }, 7),
        ({ x: 2, y: 4 }, 8),
        ({ x: 3, y: 4 }, 9),
        ({ x: 3, y: 3 }, 10),
        ({ x: 3, y: 2 }, 11),
        ({ x: 3, y: 1 }, 12),
        ({ x: 4, y: 1 }, 13),
        ({ x: 4, y: 0 }, 14),
        ({ x: 3, y: 0 }, 15),
    ]

    a = List.walk list (Dict.empty {}) \acc, (k, v) ->
        Dict.update acc k \val ->
            when val is
                Present p -> Present (if v < p then v else p)
                Missing -> Present v

    Stdout.line "res: \(Num.toStr (Dict.len a))"
