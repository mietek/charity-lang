structure ParseHml :> ParseHml =
  struct

(***********************************************)
(***********************************************)
(****************** UTILITIES ******************)
(***********************************************)
(***********************************************)

fun stringExplode s = map String.str (explode s)


exception PROP_COMPILE_ERROR of string

fun WARN why [] =  ("***" ^ why ^ "***\n")
  | WARN why restOfSrc
  = ( ("***" ^ why ^ "***\n")
           ^ "rest of src = " 
           ^ (foldr (op ^) " " restOfSrc) 
           ^ "\n")

fun WARNING why restOfSrc
  = print (WARN why restOfSrc)

fun ERROR why restOfSrc
  = ( raise PROP_COMPILE_ERROR (WARN why restOfSrc) ) 

(***********************************************)
(***********************************************)
(******************* LEXING  *******************)
(***********************************************)
(***********************************************)

(* Lexemes:

    X                            	Variable
    T F					Constants (True, False)
    action				Act
    & /\ | \/  < > [ ]  		Operator
    ( ) 	                     	Bracket

*)

fun isDigit x     = ("0" <= x andalso x <= "9")  
fun isT x      	  = ("T" = x) 
fun isF x      	  = ("F" = x) 
fun isQuote x	  = ("'" = x) 
fun isletter x    = ("a" <= x andalso x <= "z")  
fun isLetter x    = ("A" <= x andalso x <= "Z") 
fun isVarChar x   = isletter x orelse isLetter x orelse isDigit x 
fun isActChar x   = isletter x orelse isLetter x orelse isDigit x orelse
			isMem x ["'", "-", ","]
fun isWhite x     = isMem x [ " ", "\n", "\t"] (* ignore  chr 12, chr 13 *)
fun isBra x       = isMem x [ "(", ")", "{", "}" ] 
fun isOp x        = isMem x [ "&", "/\\", "|",  "\\/", "<", ">",  "[", "]"]  	

fun getWhile p res []     = (res, [])
  | getWhile p res (a::L) = if p a then getWhile p (res^a) L else (res, a::L) 

val getVar = getWhile isVarChar 
val getAct = getWhile isActChar 

(* Var is an identifier which begins with an uppercase (but not "T" or "F")
   Act is letters, numbers, or -', Example <a>, <A>, <a,b,c>, <-A,'b,'c>
   BRA is left or right bracket
   TRUE/FALSE are T,F
   OP  is an operator, (AND,OR,BOX,POSS) 
	Alias for AND is /\ or &, Alias for OR is | or \/
	Box and Poss are made up of two operators, < and > and [ and ]
*)
datatype lexeme
  = l_VAR    of string
  | l_ACT    of string
  | l_BRA    of string
  | l_MAXMIN of string
  | l_EOL
  | l_TRUE
  | l_FALSE
  | l_OP     of string 
  | l_PROP of hml

fun l_show (l_VAR v) 		= v
  | l_show (l_ACT action)	= action
  | l_show (l_MAXMIN mm) 	= mm
  | l_show (l_BRA b)		= b
  | l_show (l_EOL)		= "EOLEX"
  | l_show (l_TRUE)		= "T"
  | l_show (l_FALSE)		= "F"
  | l_show (l_OP OP)		= OP
  | l_show (l_PROP p)		= ppProp.ppPropText p
 

(*
map l_show [ l_VAR "Fixpoint", 
  	     l_ACT "'act",
  	     l_MAXMIN "max", 
  	     l_BRA "(",
	     l_EOL,
	     l_TRUE,
	     l_FALSE,
	     l_OP "<<"];

*)

local
fun getLex (a::src) 
  = if isWhite a then getLex src else

    if isLetter a then 
      let val (id, src') = getVar a src
      in
	  case id of
	    "T"	=> (l_TRUE, src')
	  | "F"	=> (l_FALSE, src')
	  | _   => (l_VAR id, src')
      end else
    
    if isActChar a then 
      let val (id, src') = getAct a src
      in
	  case id of
	    "max"	=> (l_MAXMIN id, src')
	  | "min"	=> (l_MAXMIN id, src')
	  | _   	=> (l_ACT id, src')
      end else

    if isBra a then (l_BRA a, src) else
	 
    if not (src = []) andalso 
       isOp (a^(hd src)) then (l_OP (a^(hd src)), tl src) else
    
    if isOp a then (l_OP  a, src) else

      ( WARNING ("Illegal source char " ^ a ^ " skipped") (src);
        getLex src
      )

  | getLex L = (l_EOL, [])

and lex soFar src 
  = let val (lexeme, src') = getLex src
    in
        if lexeme = l_EOL then soFar else
           lex (soFar @ [ lexeme ]) src'
    end

in

   fun LEX L = lex [] ((stringExplode L) @ [" "])
   and LEX_FRAG frag = 
       List.concat (map (fn (SMLofNJ.QUOTE a) => LEX a 
			  | (SMLofNJ.ANTIQUOTE a) => [l_PROP a]) frag)
end 


(*
map l_show (LEX (stringExplode "max X. <a>X & [b]Y") );

map l_show (LEX (stringExplode " <a>X & [b]Y") );

map l_show (LEX (stringExplode " <-a,b,c>X /\\ [Acts]Y \\/ Prop ") );
*)


(***********************************************)
(***********************************************)
(******************* SYNTAX ********************)
(***********************************************)
(***********************************************)

(*****************************************************************************)
(* Parser  -  The grammar to be parsed is the following.                     *)
(*      OR   ::=  AND |  AND\/OR                                              *)
(*                                                                           *)
(*      AND  ::=  TERM |  TERM /\ AND                                         *)
(*                                                                           *)
(*      TERM ::=  T  |  F  |  VAR  |  (OR)  |  [MOD]T  |  <MOD>T  |   	     *)
(*                      |  max(X.OR)         |  min(X.OR)                    *)
(*                                                                           *)
(*                                                                           *)
(*                                                                           *)
(*****************************************************************************)

(***********************************************)
(************* PARSING PROPOSITIONS ************)
(***********************************************)

fun checkLex lex []     = false
  | checkLex lex (a::L) = (lex = a) 

fun parseLex lex []     = ERROR ("Expecting " ^ (l_show lex)) []
  | parseLex lex (a::L) = if lex = a then L else
                          ERROR ("Expecting " ^ (l_show lex)) (map l_show L) 


fun parseModality token ((l_VAR v)::(l_OP t)::rest) =
      if token = (l_OP t)
      then (v,rest)
      else ERROR "Expecting modality" (map l_show ((l_VAR v)::(l_OP t)::rest))
  | parseModality token ((l_VAR v)::(l_ACT a)::(l_OP t)::rest) =
      if token = (l_OP t)
      then (v^a,rest)
      else ERROR "Expecting modality" 
	(map l_show ((l_VAR v)::(l_ACT a)::(l_OP t)::rest))
  | parseModality token ((l_ACT a)::(l_OP t)::rest) =
      if token = (l_OP t)
      then (a,rest)
      else ERROR "Expecting modality" (map l_show ((l_ACT a)::(l_OP t)::rest))
  | parseModality token rest =
      ERROR "Expecting modality" (map l_show (token::rest))
;

local
fun parseOr tokens =
  let val (lhs, rest) = parseAnd tokens 
  in
    case rest of
      ((l_OP "|")::rest1) 
          => let val (rhs, rest2) = parseOr rest1
	   in
	     (OR[lhs,rhs],rest2)
	   end
      | ((l_OP "\\/")::rest1) 
          => let val (rhs, rest2) = parseOr rest1
	   in
	     (OR[lhs,rhs],rest2)
	   end
      | _ => (lhs,rest)
  end
and parseAnd tokens = 
  let val (lhs, rest) = parseMonoOp tokens
  in
    case rest of
      ((l_OP "&")::rest1) 
          => let val (rhs, rest2) = parseAnd rest1
	   in
	     (AND[lhs,rhs],rest2)
	   end
      | ((l_OP "/\\")::rest1) 
          => let val (rhs, rest2) = parseAnd rest1
	   in
	     (AND[lhs,rhs],rest2)
	   end
      | _ => (lhs,rest)
  end
and parseMonoOp ((l_TRUE)::rest) = (AND[], rest)
  | parseMonoOp ((l_FALSE)::rest) = (OR[], rest)
  | parseMonoOp  ((l_VAR v)::rest) = (VAR v, rest)
  | parseMonoOp ((l_OP "[")::rest) = 
     let val (modality,rest1) = parseModality (l_OP "]") rest
	 val (prop,rest2) = parseMonoOp rest1
     in
      (BOX(modality, prop), rest2)
     end
  | parseMonoOp ((l_OP "<")::rest) = 
     let val (modality,rest1) = parseModality (l_OP ">") rest
	 val (prop,rest2) = parseMonoOp rest1
     in
      (POSS(modality, prop), rest2)
     end
  | parseMonoOp ((l_BRA "(")::rest) = 
    let val (prop, rest1) = parseOr rest in
      (prop, parseLex (l_BRA ")") rest1)
    end
  | parseMonoOp ((l_PROP p)::rest) = (p,rest)
  | parseMonoOp tokens  = 
	ERROR "Expecting a PROPOSITION, found " (map l_show tokens)

in
  fun PARSE text =
  (let val (prop, lexlist) = parseOr (LEX text)
    in
(*  Should check that lexlist is nil, ...  *)
      prop
    end) handle PROP_COMPILE_ERROR message => 
		(print message; raise PROP_COMPILE_ERROR message)
and P frags = 
  (let val (prop, lexlist) = parseOr (LEX_FRAG frags)
    in
(*  Should check that lexlist is nil, ...  *)
      prop
    end) handle PROP_COMPILE_ERROR message => 
		(print message; raise PROP_COMPILE_ERROR message)
end
 



(*
map showProposition (map PARSE  
	(map stringExplode ["T & F",
	"A | B", 
	(* "T => F => T", *)
	"(A | T&F)",
	" ([-]X & P)",
	" P | <->T&[-]X",
	" [b]F & [-b,c]X & [c]([c]F & [-b,c]Y & [b]X)",
(*	"max (X. [-]X & P)",
	"min (X. P | <->T&[-]X)",
	"max (X. [b]F & [-b,c]X & [c]max (Y. [c]F & [-b,c]Y & [b]X))",
	"(~A | ~B) => C",	*)
	"[A] T&F"
	] ) ) ;
LEX (stringExplode "[A,b,-c]X");
PARSE (stringExplode "[A,b,-c]X");


local 
fun sp env txt = 
    (showProposition (PARSE env (explode txt))) handle COMPILE_ERROR msg => msg
in
map (sp [("A",POSS (POS ["a"],TRUE)),
	("B",POSS (POS ["b"],TRUE)),
	("BOX", 
	  (MACRO ("BOX", TRUE, [("P",(PROPVAR "P")), ("Q", (PROPVAR "Q"))])))
	])
	[
        "T & F",
        "A | B",
        "T => F => T",
        "~(A | T&F)",
        "max (X. [-]X & P)",
        "min (X. P | <->T&[-]X)",
        "max (X. [b]F & [-b,c]X & [c]max (Y. [c]F & [-b,c]Y & [b]X))",
        "~(A | ~B) => BOX A F",
        "~(A | ~B) => BOX A BOX B F",
        "BOX A T&F"
	] ;



val fred = P `<a>T \/ <b><c>T`;
val jim  = P `<a>T \/ <b>T`;

 [fred] |- [jim];

dp it;




*)
end ;

