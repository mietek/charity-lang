%rf "list.ch".
%rf "exp.ch".

data list(A) -> L = nil : 1     -> L
                  | cons: A * L -> L.


data C -> exp(A, B) = fn: C -> A => B.


(* THE map_exp OPERATION IN TERMS OF RECORD: *)

def my_map_exp{pre: C -> A, post: B -> D}: exp(A, B) -> exp(C, D)

    = f => (fn: c => post fn (f, pre c)).


(* LIST FOLD LEFT IN TERMS OF FOLD RIGHT: *)

def foldl{Nil: 1 -> C, Cons: C * A -> C}: list(A) -> C

    = l => fn ({| nil : ()     => (fn: c => c)
                | cons: (a, r) => (fn: c => fn (r, Cons (c, a)))
                |}
                   l
              ,
               Nil
              ).


(* H-O O(n) LIST REVERSAL---POINTER INTUITION: *)

def new_reverse: list(A) -> list(A)

    = l => fn ({| nil : ()     => (fn: l' => l')
                | cons: (a, r) => (fn: l' => fn (r, cons (a, l')))
                |}
                   l
              ,
               []
              ).


(* ..OR JUST.. *)

def new_reverse': list(A) -> list(A)

                = l       => foldl{nil, (l', a) => cons (a, l')} l.
