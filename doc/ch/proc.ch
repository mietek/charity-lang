
(*
 *     THE DATATYPE OF (TOTAL, DETERMINISTIC) PROCESSES
 *
 *)

data C -> proc(A, B) = pr: C -> A => B * C.


(* COMPOSITION (WIRING IN SERIES): *)

def ser: proc(A, B) * proc(B, C) -> proc(A, C)

    = pp => (| (c1, c2) => pr: a => { (b, c1') => { (c, c2') => (c, (c1', c2'))
                                                  }
                                                    pr (b, c2)
                                    }
                                      pr (a, c1)
             |)
                pp.


(* PARALLELIZATION (WIRING IN PARALLEL): *)

def par: proc(A, B) * proc(C, D) -> proc(A * C, B * D)

    = pp => (| (c1, c2) => pr: (a, c) => { ((b, c1'), (d, c2')) =>
                                               ((b, d), (c1', c2'))
                                         }
                                           (pr (a, c1), pr (c, c2))
             |)
                pp.


(*
 *     THE DATATYPE OF PARTIAL PROCESSES
 *
 *)

data C -> Pproc(A, B) = Ppr: C -> A => SF(B * C).


(*
 *     THE DATATYPE OF NONDETERMINISTIC PROCESSES
 *
 *)

data C -> NDproc(A, B) = NDpr: C -> A => list(B * C).

