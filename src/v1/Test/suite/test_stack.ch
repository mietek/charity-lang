data SF(A) -> C = ff: 1 -> C
                | ss: A -> C.


data list(A) -> L = nil : 1     -> L
                  | cons: A * L -> L.


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


(* SOME USEFUL BASIC DATATYPES: *)

data bool -> B = false: 1 -> B
               | true : 1 -> B.

data nat -> N = o: 1 -> N
              | s: N -> N.


(* BUILD A STACK VIA SOME PUSH OPERATIONS: *)

def push0 = () => push(ss o, a_stack).

def push1 = () => push(ss s o, push0).

def push2 = () => push(ss s s o, push1).


(*
 * NOW WE CAN EXAMINE THE THE STACK AT AN INDEXED DEPTH VIA SOME POP
 * OPERATIONS:
 *
 *)

def popStack: stack(A) * nat -> SF(A)

            = (st,       n)  => p1 {| o: ()         => pop st
                                    | s: (st, ss _) => pop st
                                       | (st, ff)   => (st, ff)
                                    |}
                                       n.


(* WE CAN MAP THE STACK: *)

def isEven: nat -> bool

          = n   => p0({| o: ()       => (true, false)
                       | s: (b1, b2) => (b2, b1)
                       |}
                          (n)
                     ).

def bool2nat: bool -> nat

            = false => o
            | true  => s(o).


def parity: stack(nat) -> stack(bool)

          = st         => stack{isEven & bool2nat}(st).


def p_stack = () => parity(push2).
