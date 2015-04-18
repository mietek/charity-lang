
rf "colist.ch".
rf "cobTree.ch".


(***** MERGE SORT *****)


def split: list(A) -> list(A) * list(A)

    = l => {| nil : ()            => ([], [])
            | cons: (a, (l1, l2)) => (l2, cons (a, l1))
            |}
               l.


def build_m_tree: list(A) -> cobTree(list(A), 1)

    = l => (| []  => debTree: b0 []
            | [a] => debTree: b0 [a]
            | l   => debTree: b1 ((), split l)
            |)
               l.


def merge_sort{lt_A: A * A -> bool}: list(A) -> list(A)

    = l => { len  => colist_2_list ( fold_cobTree{ x      => x
                                                 , (_, p) => merge{lt_A} p
                                                 }
                                                   ( cobTree{ list_2_colist
                                                            , () => ()
                                                            }
                                                              build_m_tree l
                                                   , ( len
                                                     , (delist: ff)
                                                     )
                                                   )
                                   ,
                                     len
                                   )
           }
             length l.


(***** QUICK SORT *****)


def pivot{lt_A: A * A -> bool}: A * list(A) -> list(A) * list(A)

    = (a, l) => {| nil : () => ([], [])

                 | cons: (a', (less, rest)) =>
                             { true  => (cons (a', less), rest)
                             | false => (less, cons (a', rest))
                             }
                               lt_A (a', a)
                 |}
                    l.


def build_q_tree{lt_A: A * A -> bool}: list(A) -> cobTree (list(A), A)

    = l => (| []          => debTree: b0 []
            | [a]         => debTree: b0 [a]
            | cons (a, l) => debTree: b1 (a, pivot{lt_A} (a, l))
            |)
               l.


def quick_sort{lt_A: A * A -> bool}: list(A) -> list(A)

    = l => fold_cobTree{ x             => x
                       , (a, (l1, l2)) => append (l1, cons (a, l2))
                       }
                         (build_q_tree{lt_A} l, (length l, [])).

