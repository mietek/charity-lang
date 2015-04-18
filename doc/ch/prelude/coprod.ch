
(*
 *     THE COPRODUCT (SUM) DATATYPE
 *
 *)

data coprod(A, B) -> C = b0: A -> C
                       | b1: B -> C.

