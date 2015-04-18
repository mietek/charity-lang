
(*
 *     THE EXPONENTIAL DATATYPE: THE DATATYPE OF TOTAL FUNCTIONS
 *
 *)

data C -> exp(A, B) = fn: C -> A => B.


def compose: exp(A, B) * exp(B, C) -> exp(A, C)

           = ((fn: f),   (fn: g))  => (fn: a => g f a).

