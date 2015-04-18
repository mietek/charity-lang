
rf "Lnat.ch".
rf "Llist.ch".


def min_max: Lnat * Lnat -> Lnat * Lnat

    = (m, n) => fn ( n
                   , {| Lzero: () => (fn: _        => (m, n))
                      | Lsucc: f  => (fn: Lzero    => (n, m)
                                        | Lsucc n' => fn (lid n', lid f)
                                     )
                      |}
                         m
                   ).

def max_min = pn => {(m, n) => (n, m)} min_max pn.
def new_min = pn => p0 min_max pn.
def new_max = pn => p1 min_max pn.


def new_zip: Llist(A) * Llist(B) -> Llist(A * B)

    = (l1, l2) => fn (l2
                     , {| Lnil : () => (fn: _                        => Lnil)
                        | Lcons: p  => (fn: Lnil                     => Lnil
                                          | Lcons (b, (lid: l2')) =>
                                                { (a, f) =>
                                                      Lcons ( (a, b)
                                                            , (lid: fn (l2', f))
                                                            )
                                                }
                                                  (p0 p, lid p1 p)
                                       )
                        |}
                           l1
                     ).


def take_drop: Lnat * list(A) -> list(A) * list(A)

    = (n, l) => fn (l
                    , {| Lzero: () => (fn: l'           => ([], l'))
                       | Lsucc: f  => (fn: nil          => ([], [])
                                         | cons (a, l') => { (t, d) =>
                                                                 (cons (a, t), d)
                                                           }
                                                             fn (l', lid f)
                                      )
                       |}
                          n
                   ).

def new_take = p => p0 take_drop p.
def new_drop = p => p1 take_drop p.


def new_eq: Lnat * Lnat -> bool

    = (m, n) => fn (n, {| Lzero: () => (fn: Lzero           => true
                                          | Lsucc _         => false
                                       )
                        | Lsucc: f  => (fn: Lzero           => false
                                          | Lsucc (lid: n') => fn (n', lid f)
                                       )
                        |}
                           m
                   ).

def new_neq = pn => not new_eq pn.


def bwhile{ initial: A -> C
          , iterate: C -> C
          , final  : C -> B
          , cond   : C -> bool
          }:
             A * Lnat -> SF(B)

    = (a, n) => SF{final} fn (initial a
                             , {| Lzero: () => (fn: _ => ff)
                                | Lsucc: f  => (fn: c =>
                                                   { true  => fn ( iterate c
                                                                 , lid f
                                                                 )
                                                   | false => ss c
                                                   }
                                                     cond c
                                               )
                                |}
                                   n
                             ).


def diff: Lnat * Lnat -> Lnat * Lnat

        = (m, n)      => max_min (n, Lsub (m, n)).


def gcd: Lnat * Lnat -> SF(Lnat)

    = pn => bwhile{max_min, diff, p0, new_neq} (pn, new_max pn).


(*****

EXERCISES
---------

1. Write

        new_takewhile{pred: A -> bool}: Llist(A) -> Llist(A)
        new_dropwhile{pred: A -> bool}: Llist(A) -> Llist(A)

   (hint: write take_drop_while for lazy lists, much as take_drop is written).

2. Write

        eq_Llist{eq_A: A * A -> bool}: Llist(A) * Llist(A) -> bool

3. Write the list indexing functions

        get: Lnat * list(A) -> list(A)
        set: Lnat * list(A) * A -> list(A)

*****)
