(*
 *     TWO (DUAL) SPECIAL CASES OF PROCESSES
 *
 *)

data C -> WhiteHole(B) = wh: C -> B * C.       % IE. PRODUCER PROCESS

data C -> BlackHole(A) = bh: C -> A => C.      % IE. CONSUMER PROCESS


(*
 *     ANOTHER (USELESS) SPECIAL CASE OF PROCESSES
 *
 *)

data C -> ParallelUniverse() = pu: C -> C.     % IE. NEITHER


(*
 *     TWO (DUAL) SPECIAL CASES OF FUNCTIONS
 *
 *)

data C -> constant(B)   = confn  : C -> 1 => B.

data C -> coconstant(A) = coconfn: C -> A => 1.


(*
 *     ANOTHER (USELESS) SPECIAL CASE OF FUNCTIONS
 *
 *)

data C -> useless()     = ul     : C -> 1 => 1.
