(******************************************************************************
 *
 * INDUCTIVES
 *
 *****************************************************************************)


data bool -> B = true | false: 1 -> B.


def not: bool  -> bool

       = true  => false
       | false => true.


def or: bool *  bool   -> bool

      = (false, false) => false
      | _              => true.


def and: bool * bool  -> bool

       = (true, true) => true
       | _            => false.


(*****************************************************************************)


data test -> T = T0 | T1 | T2 | T3 | T4 | T5: 1 -> T.


def eq_test: test * test -> bool

           = (T0,   T0)  => true
           | (T1,   T1)  => true
           | (T2,   T2)  => true
           | (T3,   T3)  => true
           | (T4,   T4)  => true
           | (T5,   T5)  => true
           | _           => false.


def lt_test: test * test -> bool

           = (T0,   T0)  => false
           | (T0,   _ )  => true
           | (T1,   T0)  => false
           | (T1,   T1)  => false
           | (T1,   _ )  => true
           | (T2,   T0)  => false
           | (T2,   T1)  => false
           | (T2,   T2)  => false
           | (T2,   _ )  => true
           | (T3,   T0)  => false
           | (T3,   T1)  => false
           | (T3,   T2)  => false
           | (T3,   T3)  => false
           | (T3,   _ )  => true
           | (T4,   T0)  => false
           | (T4,   T1)  => false
           | (T4,   T2)  => false
           | (T4,   T3)  => false
           | (T4,   T4)  => false
           | (T4,   _ )  => true
           | _           => false.


def le_test = pt => or (lt_test pt, eq_test pt).
def gt_test = pt => not le_test pt.
def ge_test = pt => not lt_test pt.


(*****************************************************************************)


data SF(A) -> C = ss: A -> C
                | ff: 1 -> C.


(*****************************************************************************)


data coprod(A, B) -> C = b0: A -> C
                       | b1: B -> C.


(*****************************************************************************)


data nat -> N = o: 1 -> N
              | s: N -> N.


def zero  = () => o.
def one   = () => s zero.
def two   = () => s one.
def three = () => s two.
def four  = () => s three.
def five  = () => s four.
def six   = () => s five.
def seven = () => s six.
def eight = () => s seven.
def nine  = () => s eight.
def ten   = () => s nine.


def isZero: nat -> bool

          = o   => true
          | _   => false.


def pred: nat  -> nat

        = o    => o
        | s(n) => n.


def add: nat * nat -> nat

       = (m,   n)  => {| o: () => n
                       | s: x  => s x
                       |}
                          m.

add (two, three).


def mul: nat * nat -> nat

       = (m,   n)  => {| o: () => o
                       | s: x  => add (n, x)
                       |}
                          m.

mul (two, three).


def pow: nat * nat -> nat

       = (m,   n)  => {| o: () => s o
                       | s: x  => mul (m, x)
                       |}
                          n.

pow (two, three).


def sub: nat * nat -> nat

       = (m,   n)  => {| o: () => m
                       | s: x  => pred x
                       |}
                          n.

sub (five, two).


def lt_nat = (m, n) => isZero sub (s m, n).
def le_nat = pn     => isZero sub pn.
def gt_nat = pn     => not le_nat pn.
def ge_nat = pn     => not lt_nat pn.


def eq_nat: nat * nat -> bool

          = (m,   n)  => { (s(o), s(o)) => true
                         | _            => false
                         }
                           {| o: ()     => (s m,    s n)
                            | s: (x, y) => (pred x, pred y)
                            |}
                               m.

eq_nat (six, six).
eq_nat (seven, seven).
eq_nat (six, seven).
eq_nat (seven, six).
eq_nat (two, nine).


def div_mod: nat * nat -> SF(nat * nat)

    = (m, o) => ff
    | (m, n) => ss p0 {| o: ()              => ((o,    m),          false)
                       | s: ((q, r), true)  => ((q,    r),          true)
                          | ((q, r), false) =>
                                { true  =>     ((q,    r),          true)
                                | false =>     ((s q,  sub (r, n)), false)
                                }
                                  lt_nat (r, n)
                       |}
                          m.

def div = pn => SF{p0} div_mod pn.
def mod = pn => SF{p1} div_mod pn.

div_mod (one, zero).
div_mod (six, two).
div (seven, three).
mod (seven, three).


(*****************************************************************************)


data list(A) -> L = nil : 1     -> L
                  | cons: A * L -> L.


def reverse: list(A) -> list(A)

           = l       => p0 {| nil : ()                    => ([],         l)
                            | cons: (_, (r, cons(a, l'))) => (cons(a, r), l')
                                  | _                     => ([],         l)
                            |}
                               l.

reverse [T0, T1, T2, T3, T4, T5].


def append: list(A) * list(A) -> list(A)

          = (l1,      l2)     => {| nil : () => l2
                                  | cons: p  => cons p
                                  |}
                                     l1.


def flatten: list(list(A)) -> list(A)

           = ll            => {| nil : () => []
                               | cons: p  => append p
                               |}
                                  ll.

flatten [[T0, T1], [], [T2], [T3, T4, T5]].


def filter{pred: A -> bool}: list(A) -> list(A)

    = l => {| nil : ()     => []
            | cons: (a, l) => { true  => cons(a, l)
                              | false => l
                              }
                                pred a
            |}
               l.

filter{t => not eq_test (t, T2)} [T0, T2, T1, T2, T2, T2,
                                  T3, T2, T4, T2, T5, T2].


def dropwhile{pred: A -> bool}: list(A) -> list(A)

    = l => reverse p0 {| nil : ()                              =>
                                   ([], (l, true))
                       | cons: (_, (_, ([],           _)))     =>
                                   ([], (l, true))
                       |       (_, (r, (cons (a, l'), false))) =>
                                   (cons (a, r), (l', false))
                       |       (_, (r, (cons (a, l'), true)))  =>
                                   { true  => (r,           (l', true))
                                   | false => (cons (a, r), (l', false))
                                   }
                                     pred a
                       |}
                          l.

dropwhile{t => lt_test (t, T3)} [T0, T1, T2, T3, T4, T5].


list{t => (T0, t)} [T1, T2, T3].


(*****************************************************************************)


data bTree(A, B) -> T = leaf: A           -> T
                      | node: B * (T * T) -> T.


def test_bTree_bool = () => node ( false
                                 ,
                                   ( leaf false
                                   ,
                                     node ( true
                                          ,
                                            ( leaf false
                                            , leaf false
                                            )
                                          )
                                   )
                                 ).


def traverse{order: A * (list(A) * list(A)) -> list(A)}: bTree(A, A) -> list(A)

    = t => {| leaf: a => [a]
            | node: n => order n
            |}
               t.


def postorder: A * (list(A) * list(A)) -> list(A)

             = (a, pl)                 => append (append pl, [a]).


traverse{postorder} bTree{ true => T1 | false => T0
                         , true => T3 | false => T2
                         }
                           test_bTree_bool.


def OR_bTree: bTree(bool, bool) -> bool

           = tb                => {| leaf: b       => b
                                   | node: (b, pb) => or (b, or pb)
                                   |}
                                      tb.

OR_bTree test_bTree_bool.


(******************************************************************************
 *
 * COINDUCTIVES (FIRST-ORDER)
 *
 *****************************************************************************)


data C -> conat = denat: C -> SF(C).


def nat_2_conat: nat -> conat

               = n   => (| o     => denat: ff
                         | s(n') => denat: ss n'
                         |)
                            n.


def cozero  = () => nat_2_conat zero.
def coone   = () => nat_2_conat one.
def cotwo   = () => nat_2_conat two.
def cothree = () => nat_2_conat three.
def cofour  = () => nat_2_conat four.
def cofive  = () => nat_2_conat five.
def cosix   = () => nat_2_conat six.
def coseven = () => nat_2_conat seven.
def coeight = () => nat_2_conat eight.
def conine  = () => nat_2_conat nine.
def coten   = () => nat_2_conat ten.


def coadd: conat * conat -> conat

         = (m,     n)    => (| (ss(m'), n')    => denat: ss (denat m', n')
                             | (ff,    ss(n')) => denat: ss (ff, denat n')
                             | (ff,    ff)     => denat: ff
                             |)
                                (denat m, denat n).


coadd (cotwo, cothree).


(*****************************************************************************)


data C -> colist(A) = delist: C -> SF(A * C).


def list_2_colist: list(A) -> colist(A)

                 = l       => (| nil          => delist: ff
                               | cons (a, l') => delist: ss (a, l')
                               |)
                                  l.


def test_colist_1 = () => (delist: ss (T0, (delist: ss (T1, (delist: ff))))).
def test_colist_2 = () => list_2_colist [T2, T3, T4, T5].

test_colist_1.
test_colist_2.


def coappend: colist(A) * colist(A) -> colist(A)

    = (l1, l2) => (| (ff,          ff)          =>
                         delist: ff
                   | (ff,          ss (a, l2')) =>
                         delist: ss (a, (ff, delist l2'))
                   | (ss (a, l1'), l2')         =>
                         delist: ss (a, (delist l1', l2'))
                   |)
                      (delist l1, delist l2).


coappend (test_colist_1, test_colist_2).


def truth: 1  -> colist(bool)

         = () => (| () => delist: ss (true, ())
                  |)
                     ().

colist{not} truth.
