data bool -> B = true | false: 1 -> B.
data test -> T = T0 | T1 | T2 | T3 | T4 | T5: 1 -> T.


def eq_test: test * test -> bool

           = (T0,   T0)  => true
           | (T1,   T1)  => true
           | (T2,   T2)  => true
           | (T3,   T3)  => true
           | (T4,   T4)  => true
           | (T5,   T5)  => true
           | _           => false.


data nat -> N = o: 1 -> N
              | s: N -> N.


data list(A) -> L = nil : 1     -> L
                  | cons: A * L -> L.


(******************************************************************************
 *
 * DATATYPES WITH HIGHER-ORDER
 *
 *****************************************************************************)


data C -> exp(A, B) = fn: C -> A => B.


fn ((), (fn: () => ())).                             % RECORD AND DESTRUCTION
fn ((), (| () => fn: () => () |) ()).                % UNFOLD AND DESTRUCTION
fn ((), exp{() => (), () => ()} (fn: () => ())).     % MAP    AND DESTRUCTION


def pred: nat -> nat

        = n   => fn (n, (fn: o    => o
                           | s(m) => m
                        )
                    ).

pred o.
pred s o.
pred s s o.


def nat_2_list: nat -> list(1)

              = n   => {| o: () => []
                        | s: l  => cons ((), l)
                        |}
                           n.


def length: list(A) -> nat

          = l       => {| nil : ()     => o
                        | cons: (_, n) => s n
                        |}
                           l.


def my_map_exp{pre: C -> A, post: B -> D}: exp(A, B) -> exp(C, D)

    = f => (fn: c => post fn (pre c, f)).


def comp: exp(A, B) * exp(B, C) -> exp(A, C)

        = (f,         g)        => (fn: a => fn (fn (a, f), g)).


def curry: exp(A * B, C) -> exp(A, exp(B, C))

         = f             => (fn: a => (fn: b => fn ((a, b), f))).


def decurry: exp(A, exp(B, C)) -> exp(A * B, C)

           = f                 => (fn: (a, b) => fn (b, fn (a, f))).


def foldl{Nil: 1 -> C, Cons: C * A -> C}: list(A) -> C

    = l => fn (Nil, {| nil : ()     => (fn: c => c)
                     | cons: (a, r) => (fn: c => fn (Cons (c, a), r))
                     |}
                        l
              ).


def new_reverse: list(A) -> list(A)

               = l       => foldl{nil, (l', a) => cons (a, l')} l.

new_reverse [].
new_reverse [T0].
new_reverse [T0, T1].
new_reverse [T0, T1, T2].


% ADD MORE TEST EXPRESSIONS
% MAP
% SHORT CIRCUITING


(*****************************************************************************)


data SF(A) -> C = ff: 1 -> C
                | ss: A -> C.


data C -> parExp(A, B) = parfn: C -> A => SF(B).


(*****************************************************************************)


data C -> circ(A, B) = proc: C -> A => B * C.


def ser: circ(B, C) * circ(A, B) -> circ(A, C)

    = (c2, c1) => (| (c2, c1) => proc: a => { (b, c1') => { (c, c2') => (c
                                                                        ,
                                                                         (c2'
                                                                         ,
                                                                          c1'
                                                                         )
                                                                        )
                                                          }
                                                            proc (b, c2)
                                            }
                                              proc (a, c1)
                   |)
                      (c2, c1).


def par: circ(A, B) * circ(C, D) -> circ(A * C, B * D)

    = (c1, c2) => (| (c1, c2) => proc: (a, c) => { ((b, c1'), (d, c2')) =>
                                                   ((b, d), (c1', c2'))
                                                 }
                                                   (proc (a, c1), proc (c, c2))
                   |)
                      (c1, c2).


(*****************************************************************************)


data C -> parCirc(A, B) = parproc: C -> A => SF(B * C).


(*****************************************************************************)


data C -> NDCirc(A, B) = ndproc: C -> A => list(B * C).


(*****************************************************************************)


data S -> stack(A) = push: S -> SF(A) => S
                   | pop : S -> S * SF(A).


% INCORPORATE test_stack.ch


def a_stack: 1  -> stack(A)

           = () => (| l => push: ff     => []
                               | ss (a) => cons (a, l)
                    |      pop : { []           => ([], ff)
                                 | cons (a, l') => (l', ss a)
                                 }
                                   l
                    |)
                       [].


(*****************************************************************************)


data Q -> queue(A) = enq: Q -> SF(A) => Q
                   | deq: Q -> Q * SF(A).


def append: list(A) * list(A) -> list(A)

          = (l1,      l2)     => {| nil : ()     => l2
                                  | cons: (a, l) => cons (a, l)
                                  |}
                                     l1.


def a_queue: 1  -> queue(A)

           = () => (| l => enq: ff     => []
                              | ss (a) => append (l, [a])
                    |      deq: { []           => ([], ff)
                                | cons (a, l') => (l', ss a)
                                }
                                  l
                    |)
                       [].
