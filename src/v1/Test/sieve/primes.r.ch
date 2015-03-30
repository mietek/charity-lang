rf "UTILS.ch".
rf "INT.ch".

data C -> Exp(A, B) = fn: C -> A => B.

data C -> Box(A) = unbox: C -> A.

data Lnat -> C = lo: 1 -> C
               | ls: Box(C) -> C.


def Lpred: Lnat          -> Lnat

        = lo            => lo
        | ls (unbox: n) => n.


data C -> inflist(A) = head: C -> A | tail: C -> C.

data success_or_fail(A) -> C = ss: A -> C| ff: 1 -> C.


def ints2 =
    () => (| n => head: n
           |      tail: succ_int n
           |) 2.

def nats2 =
    () => (| n => head: n
           |      tail: succ n
           |) succ succ zero.

def half_nats2 =
    () => (| (n,m) => head: n
           |          tail: (m,ls (unbox: n))
           |)(ls (unbox: lo), ls (unbox: lo)).

(*
def forage =
    ((S,B),N) =>
    {| lo: () => (((S,B),N), ff)
     | ls: (unbox: (((S,B),N),Val)) => { ss(V) => (((S,B),N),Val)
                                       | ff    => (((tail S, tail B), tail N)
                                                  ,{true  => ss head S
                                                   |false => ff
                                                   } head B
                                                  )
                                       } Val
     |} head N.
*)

def bwhile{initial: A -> C,
           iterate: C -> C,
           final  : C -> B,
           cond   : C -> bool
          }:
             A * Lnat -> B

    = (a, n) => fn({| lo: () => (fn: {c => final c})
                    | ls: r  => (fn: {c => { true  => fn(unbox r, iterate c)
                                           | false => final c
                                           }
                                             cond c
                                     }
                                )
                    |}
                       n
                  ,
                   initial a
                  ).


def iter = (((S, B), N), _) => (((tail S, tail B), tail N)
                               , { true  => ss head S
                                 | false => ff
                                 }
                                   head B
                               ).


def forage = ((S, B), N) => bwhile{ v => (v, ff)
                                  , iter
                                  , v => v
                                  , (_, ss(_)) => false
                                  | (_, ff)    => true
                                  }
                                    (((S, B), N), head N).

def sieve =
    T0 => (| (T,Val) => head: Val
           |            tail: forage T
           |) forage T0.


def cyc =
    (n,m) => { zero     => (succ zero, pred m)
             | succ(m1) => { zero     => (zero, succ n)
                           | succ(m2) => (succ n, pred m)
                           } m1
             } m.


def AND =
    L => {| nil : () => true
          | cons: v  => and v
          |} L.

def tail_prime_pred =
    ((S,N),F) =>
   {true  => ((list{cyc} cons((zero, head N), S), tail N), true)
   |false => ((list{cyc} S, tail N), false)
   } AND list{x => { zero => false | succ(_) => true} p0 x} S.


def prime_pred =
    () => (| (S,F) => head: F
           |          tail: tail_prime_pred(S,F)
           |)(tail_prime_pred(([],nats2),false)).

(*  Finally the sieve is given by: *)


def primes = () => sieve(( ints2, prime_pred), half_nats2).

def list_primes = n => p0 {| zero: () => ([],primes)
                           | succ: (L,P) => (cons(head P, L),tail P)
                           |} int_2_nat n.
