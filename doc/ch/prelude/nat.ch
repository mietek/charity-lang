
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

