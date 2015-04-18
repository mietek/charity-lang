
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

