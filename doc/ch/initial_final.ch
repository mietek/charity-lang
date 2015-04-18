
(*
 *  THE INITIAL DATATYPE, IE:
 *
 *       - "THE ZERO"
 *       - "THE NULLARY SUM DATATYPE"
 *       - "THE DATATYPE OF 0 ELEMENTS"
 *
 *  data 0 -> C.
 *
 *)

data O -> C = rec_0: C -> C.


def coterminal: O -> C           % {}

              = o => {| rec_0: c => c |} o.


(*
 *  THE FINAL DATATYPE, IE:
 *
 *       - "THE UNIT"
 *       - "THE NULLARY PRODUCT DATATYPE"
 *       - "THE DATATYPE OF 1 ELEMENT"
 *
 *  data C -> 1.
 *
 *)

data C -> I = rec_1: C -> C.     % EQUIVALENT TO 1


def terminal: C -> I             % EQUIVALENT TO !, IE. ()

            = c => (| c => rec_1: c |) c.

