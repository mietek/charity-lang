rf "inflist.ch".


data tower -> C = src | dst | aux: 1 -> C.


data C -> tower_subst = SRC | DST | AUX: C -> tower.


data C -> move(A) = from | to: C -> A.


def subst: tower_subst * list(move(tower)) -> list(move(tower))

    = (ts, lm) => list{move{ src => SRC ts
                           | dst => DST ts
                           | aux => AUX ts
                           }
                      }
                        lm.


def hanoi: 1 -> inflist(list(move(tower)))

    = () => (| lm => head: lm
             |       tail: append (subst ((SRC: src, DST: aux, AUX: dst), lm),
                                   cons ((from: src, to: dst),
                                         subst ((SRC: aux, DST: dst, AUX: src), lm)
                                        )
                                  )
             |)
                [(from: src, to: dst)].
