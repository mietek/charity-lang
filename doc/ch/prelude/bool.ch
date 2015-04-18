
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

