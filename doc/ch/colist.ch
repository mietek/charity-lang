
rf "conat.ch".


data C -> colist(A) = delist: C -> SF(A * C).


def list_2_colist: list(A) -> colist(A)

                 = l       => (| nil          => delist: ff
                               | cons (a, l') => delist: ss (a, l')
                               |)
                                  l.


def colist_2_list: colist(A) * nat -> list(A)

    = (l, n) => fn ( l
                   , {| zero: () => (fn: _                    => [])
                      | succ: f  => (fn: (delist: ff)         => []
                                       | (delist: ss (a, l')) => cons (a, fn (l', f))
                                    )
                      |}
                         n
                   ).


def cohd: colist(A)           -> SF(A)

        = (delist: ff)        => ff
        | (delist: ss (a, _)) => ss a.


def cotl: colist(A)           -> SF(colist(A))

        = (delist: ff)        => ff
        | (delist: ss (_, l)) => ss l.


def colength: colist(A) -> conat

            = l         => (| ff         => denat: ff
                            | ss (_, l') => denat: ss delist l'
                            |)
                               delist l.


def coappend: colist(A) * colist(A) -> colist(A)

    = (l1, l2) => (| (ff,          ff)          =>
                         delist: ff
                   | (ff,          ss (a, l2')) =>
                         delist: ss (a, (ff, delist l2'))
                   | (ss (a, l1'), l2')         =>
                         delist: ss (a, (delist l1', l2'))
                   |)
                      (delist l1, delist l2).


def merge{lt_A: A * A -> bool}: colist(A) * colist(A) -> colist(A)

    = (l1, l2) => (| (ss (a, l), ss (a', l')) => delist: { true  => ss (a, (delist l, ss (a', l')))
                                                         | false => ss (a', (ss (a, l), delist l'))
                                                         }
                                                           lt_A (a, a')

                   | (ss (a, l), ff)          => delist: ss (a, (delist l, ff))
                   | (ff,        ss (a, l))   => delist: ss (a, (ff,       delist l))
                   | (ff,        ff)          => delist: ff
                   |)
                      (delist l1, delist l2).


def while{f: A -> A, pred: A -> bool}: A -> colist(A)

    = a => (| a => delist: { true  => ss (a, f a)
                           | false => ff
                           }
                             pred a
            |)
               a.

