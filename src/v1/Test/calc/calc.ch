rf "PRELUDE.ch".
rf "syntax-trees.ch".


(*
 *  SCANNER STATES
 *)


data lex_states -> C = s0 | s1: 1 -> C.


(*
 *  THE SCANNER WITH ATTRIBUTES
 *
 *)


data tokens(A) -> C = PLUS : 1 -> C
                    | TIMES: 1 -> C
                    | NUM  : A -> C.

def char2int: char -> int                                           % hopefully this will be built-in 
    = c => { \c0..\c9 => sub_int (code c, code \c0)
           | _        => 0
           }
             c.

def lex: 1 -> rS(char, list(tokens(int)))
    = () => (| (s0, (f, _)) => tok: C => 
                                  { \d32 => MORE (s0, (f, 0))
                                  | \c+ => MORE (s0, ((fn: l => fn(cons (PLUS, l),f)), 0))
                                  | \c* => MORE (s0, ((fn: l => fn(cons (TIMES, l),f)), 0))
                                  | \c0..\c9 => MORE (s1, (f, char2int C))
                                  |  _  => FAIL } C
             |                 end: ss fn ([], f)
             | (s1, (f, s)) => tok: C =>  
                                  { \d32 => MORE (s0, ((fn: l => fn (cons (NUM s, l), f)), 0))
                                  | \c+ => MORE (s0, ((fn: l => fn (cons (NUM s, cons (PLUS, l)), f)), 0))
                                  | \c* => MORE (s0, ((fn: l => fn (cons (NUM s, cons (TIMES, l)), f)), 0))
                                  | \c0..\c9 => MORE (s1, (f, add_int(mul_int(s,10),char2int C) ))
                                  |  _  => FAIL
                                  } C
             |                 end: ss fn ([NUM s], f)
             |) (s0, ((fn: l => l), 0)).


def scan: string -> SF(list(tokens(int)))
    = s => p0 PARSE (lex, s).


(*
 *  THE INTERNAL REPRESENTATION
 *)


data expr -> C = Add: C * C -> C
               | Mul: C * C -> C
               | Val: int   -> C.


(*
 *  THE PARSER
 *)


data syn_states -> C = ps0 | ps1: 1 -> C.


def update_expr: tokens(int) * expr * exp(expr, expr) * tokens(int) -> tokens(int) * expr * exp(expr, expr)
    = (((op, val), f), op') => { (PLUS,  PLUS)  => ((PLUS,  val), (fn: e => Add (fn (val, f), e)))
                               | (PLUS,  TIMES) => ((TIMES, val), (fn: e => fn (Mul (val, e), f)))
                               | (TIMES, PLUS)  => ((PLUS,  val), (fn: e => Add (fn (val, f), e)))
                               | (TIMES, TIMES) => ((TIMES, val), (fn: e => fn (Mul (val, e), f)))
                               | _              => ((op, val), f)
                               }
                                 (op, op').


def syn: 1 -> rS(tokens(int), expr)
    = () => (| (ps0, ((op, val), f)) => tok: NUM i => MORE (ps1, ((op, Val i), f))
                                           | _     => FAIL
             |                          end: ff
             | (ps1, ((op, val), f)) => tok: PLUS  => MORE (ps0, update_expr (((op, val), f), PLUS))
                                           | TIMES => MORE (ps0, update_expr (((op, val), f), TIMES))
                                           | _     => FAIL
             |                          end: ss fn (val, f)
             |) (ps0, ((TIMES, Val 0), (fn: e => e))).


def parse: list tokens int -> SF expr
    = l => p0 PARSE (syn, l).


(*
 *  THE EVALUATOR
 *)


def eval: expr -> int
    = e => {| Add: p => add_int p
            | Mul: p => mul_int p
            | Val: i => i
            |} e.


(*
 * THE ENTIRE SYSTEM
 *)


def calc: string -> SF(int)
    = s => flatten_SF SF{l => SF{eval} parse l} scan s.
