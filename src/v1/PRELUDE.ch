
(****************************************************
**                                                 **
**  The Charity Standard Prelude v0.5: PRELUDE.ch  **
**                                                 **
**          The Charity Development Group          **
**            charity@cpsc.ucalgary.ca             **
**                                                 **
**  CONSTITUENTS: exp.ch                           **
**                coprod.ch                        **
**                SF.ch                            **
**                bool.ch                          **
**                nat.ch                           **
**                list.ch                          **
**                string.ch                        **
**                                                 **
**  TO DO: - support eta-notation                  **
**         - replace "nat" with "int"              **
**         - support infix operators               **
**                                                 **
****************************************************)


(*
 *     THE EXPONENTIAL DATATYPE: THE DATATYPE OF TOTAL FUNCTIONS
 *
 *)

data C -> exp(A, B) = fn: C -> A => B.


def compose: exp(A, B) * exp(B, C) -> exp(A, C)

           = ((fn: f),   (fn: g))  => (fn: a => g f a).


(*
 *     THE COPRODUCT (SUM) DATATYPE
 *
 *)

data coprod(A, B) -> C = b0: A -> C
                       | b1: B -> C.


(*
 *     THE "SUCCESS-OR-FAILURE" DATATYPE: THE DATATYPE OF EXCEPTIONS
 *
 *)

data SF A -> C = ss: A -> C
               | ff: 1 -> C.


def flatten_SF: SF SF A -> SF A

              = ss ss a => ss a
              | _       => ff.


def compose_SF{f: A -> SF B, g: B -> SF C}: A -> SF C

    = a => f a ; ss b => g b
    | _ => ff.


(*
 *     THE BOOLEAN DATATYPE (BUILTIN)
 *
 *     data bool -> C = false | true: 1 -> C.
 *
 *)


def not: bool  -> bool

       = true  => false
       | false => true.


def or: bool *  bool   -> bool

      = (false, false) => false
      | _              => true.


def and: bool * bool  -> bool

       = (true, true) => true
       | _            => false.


def xor: bool *  bool   -> bool

       = (true,  false) => true
       | (false, true)  => true
       | _              => false.


def imp: bool * bool   -> bool

       = (true, false) => false
       | _             => true.


def eqv: bool *  bool   -> bool

       = (false, false) => true
       | (true,  true ) => true
       | _              => false.


(*
 *     THE NATURAL NUMBER DATATYPE
 *
 *)

data nat -> C = zero: 1 -> C
              | succ: C -> C.


def one   = () => succ zero.
def two   = () => succ one.
def three = () => succ two.
def four  = () => succ three.
def five  = () => succ four.
def six   = () => succ five.
def seven = () => succ six.
def eight = () => succ seven.
def nine  = () => succ eight.
def ten   = () => succ nine.


def isZero: nat  -> bool

          = zero => true
          | _    => false.


def pred: nat    -> nat

        = zero   => zero
        | succ n => n.


def add: nat * nat -> nat

       = (m,   n)  => {| zero: () => n
                       | succ: x  => succ x
                       |}
                          m.


def mul: nat * nat -> nat

       = (m,   n)  => {| zero: () => zero
                       | succ: x  => add (n, x)
                       |}
                          m.


def pow: nat * nat -> nat

       = (m,   n)  => {| zero: () => one
                       | succ: x  => mul (m, x)
                       |}
                          n.


def sub: nat * nat -> nat

       = (m,   n)  => {| zero: () => m
                       | succ: x  => pred x
                       |}
                          n.


def lt: nat * nat -> bool

      = (m,   n)  => {| zero: ()      => (fn: zero   => false
                                            | succ _ => true
                                         )
                      | succ: (fn: f) => (fn: zero   => false
                                            | succ n => f n
                                         )
                      |}
                         m
      ; (fn: f) => f n.


def le: nat * nat -> bool

      = (m,   n)  => {| zero: ()      => (fn: _      => true)
                      | succ: (fn: f) => (fn: zero   => false
                                            | succ n => f n
                                         )
                      |}
                         m
      ; (fn: f) => f n.


def gt = pn => not le pn.
def ge = pn => not lt pn.


def eq: nat * nat -> bool

      = (m,   n)  => {| zero: ()      => (fn: zero   => true
                                            | succ _ => false
                                         )
                      | succ: (fn: f) => (fn: zero   => false
                                            | succ n => f n
                                         )
                      |}
                         m
      ; (fn: f) => f n.


def div_mod: nat * nat -> SF(nat * nat)

 = (m, zero) => ff
 | (m, n)    => ss p0 {| zero: ()              => ((zero,   m),          false)
                       | succ: ((q, r), true)  => ((q,      r),          true)
                             | ((q, r), false) =>
                                   { true  =>     ((q,      r),          true)
                                   | false =>     ((succ q, sub (r, n)), false)
                                   }
                                     lt (r, n)
                       |}
                          m.


def div = pn => SF{p0} div_mod pn.
def mod = pn => SF{p1} div_mod pn.


def max: nat * nat -> nat

       = (m,   n)  => { true  => n
                      | false => m
                      }
                        le (m, n).


def min: nat * nat -> nat

       = (m,   n)  => { true  => m
                      | false => n
                      }
                        le (m, n).


(*
 *     THE LIST DATATYPE (BUILTIN)
 *
 *     data list A -> C = nil : 1     -> C
 *                      | cons: A * C -> C.
 *
 *)


def reverse: list A -> list A

           = l      => p0 {| nil : ()                     => ([],          l)
                           | cons: (_, (r, cons (a, l'))) => (cons (a, r), l')
                                 | _                      => ([],          l)
                           |}
                              l.


def isEmpty: list A -> bool

           = nil    => true
           | _      => false.


def length: list A -> nat

          = l      => {| nil : ()     => zero
                       | cons: (_, n) => succ n
                       |}
                          l.


def hd: list A      -> SF A

      = nil         => ff
      | cons (a, _) => ss a.


def tl: list A      -> SF list A

      = nil         => ff
      | cons (_, l) => ss l.


def init: list A -> SF list A

    = l => {| nil : ()         => ff
            | cons: (_, ff)    => ss []
                  | (a, ss l') => ss cons (a, l')
            |}
               l.


def last: list A -> SF A

        = l      =>  hd reverse l.


def append: list A * list A -> list A

          = (l1,      l2)   => {| nil : () => l2
                                | cons: p  => cons p
                                |}
                                   l1.


def flatten: list list A -> list A

           = ll          => {| nil : () => []
                             | cons: p  => append p
                             |}
                                ll.


def remove{p: A -> bool}: list A -> list A

    = l => {| nil : ()           => []
            | cons: (a, r) | p a => p1 #
                           | ..  => cons (a, r)
            |}
               l.


def filter{p: A -> bool}: list A -> list A

    = l => {| nil : ()           => []
            | cons: (a, r) | p a => cons (a, r)
                           | ..  => r
            |}
               l.


def takewhile{p: A -> bool}: list A -> list A

    = l => {| nil : ()           => []
            | cons: (a, r) | p a => cons (a, r)
                           | ..  => []
            |}
               l.


def dropwhile{p: A -> bool}: list A -> list A

    = l => {| nil : ()           => []
            | cons: (a, r) | p a => r
                           | ..  => cons #
            |}
               l.


def OR: list bool -> bool

      = lb        => {| nil : ()          => false
                      | cons: (b, r) | b  => true
                                     | .. => r
                      |}
                         lb.


def AND: list bool -> bool

       = lb        => {| nil : ()          => true
                       | cons: (b, r) | b  => r
                                      | .. => false
                       |}
                          lb.


def ADD: list nat -> nat

       = ln       => {| nil : () => zero
                      | cons: pn => add pn
                      |}
                         ln.


def MUL: list nat -> nat

       = ln       => {| nil : () => one
                      | cons: pn => mul pn
                      |}
                         ln.


def rep: nat * A  -> list A

       = (n,   a) => {| zero: () => []
                      | succ: l  => cons (a, l)
                      |}
                         n.


def test_list{p: A -> bool}: list A -> bool

    = l => {| nil : ()           => false
            | cons: (a, r) | p a => true
                           | ..  => r
            |}
               l.


def eq_list{eq_A: A * A -> bool}: list A * list A -> bool

    = (l1, l2) => {| nil : ()           => (fn: nil                        => true
                                              | cons _                     => false
                                           )
                   | cons: (a, (fn: f)) => (fn: nil                        => false
                                              | cons (b, bs) | eq_A (a, b) => f bs
                                                             | ..          => false
                                           )
                   |}
                      l1

    ; (fn: f) => f l2.


def zip: list A * list B -> list (A * B)

    = (l1, l2) => {| nil : ()           => (fn: _            => [])
                   | cons: (a, (fn: f)) => (fn: nil          => []
                                              | cons (b, bs) => cons ((a, b), f bs)
                                           )
                   |}
                      l1

    ; (fn: f) => f l2.


% cartesian product, sub_list (--),
% relational operators (<, >, =>, <=)
% noDups, take, drop, index, max_list, min_list


data string = list char.


def eq_string: string * string -> bool = ps => eq_list{eq_char} ps.


def explode: string -> list string = s => list{c => [c]} s.


def implode: list string -> string = s => flatten s.

