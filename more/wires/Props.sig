signature PROP =
sig

type act = string list * string * string list

datatype prop = VAR of act
              | AND of (string list * prop list * string list)
              | OR  of (string list * prop list * string list)
              | L_POSS of (act * prop)
              | L_BOX  of (act * prop)
              | R_POSS of (prop * act)
              | R_BOX  of (prop * act)
              | ABS of ((string * string) list * prop * (string * string) list)
;


(* "type checking" for circuits *)
val getTypes: prop -> string list * string list

val propErrors: prop -> string

val ppPropText: prop -> string

(* val ppCircuitDot: circuit -> string *)

exception COMPILE_ERROR of string

val COMPILE: string -> prop

val reverse: prop -> prop

val propFold: (prop -> prop) -> prop -> prop
val wireSubst: (string * string) list -> string -> string
val reWireL:  (string * string) list -> prop -> prop
val reWireRight:  (string * string) list -> prop -> prop
val aeOnce: prop -> prop
val abstractionElimination: prop -> prop

end

