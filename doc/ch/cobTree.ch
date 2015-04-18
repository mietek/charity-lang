
data C -> cobTree(A, B) = debTree: C -> A + (B * (C * C)).


def fold_cobTree{B0: A -> C, B1: B * (C * C) -> C}: cobTree(A, B) * (nat * C) -> C

    = (t, (n, c)) => fn ( t
                        , {| zero: () => (fn: _ => c)
                           | succ: f  => (fn: t => { b0 a             => B0 a
                                                   | b1 (b, (t1, t2)) => B1 (b, (fn (t1, f), fn (t2, f)))
                                                   }
                                                     debTree t
                                         )
                           |}
                              n
                        ).

