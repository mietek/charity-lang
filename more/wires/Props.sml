structure PROP :> PROP =
struct

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


(* Utilities 
   Most of these utilities are for "set as a list" 
   I is the identity;
*)

fun fst (x,y) = x
and snd (x,y) = y
and swap (x,y) = (y,x)
and isMem a [] = false
  | isMem a (b::rest) = a = b orelse isMem a rest
and noDups [] = []
  | noDups (a::rest) = if isMem a rest then noDups rest else a::(noDups rest)
and hasNoDups [] = true
  | hasNoDups (a::rest) = not (isMem a rest) andalso hasNoDups rest
and intersection [] _ = []
  | intersection _ [] = []
  | intersection (a::rest) set2 =
    if isMem a set2
    then a::(intersection rest set2)
    else intersection rest set2
and union set1 set2 = noDups (set1 @ set2)
and subset [] set2 = true
  | subset (a::rest) set2 = isMem a set2 andalso subset rest set2
(* setDiff returns all of set1 which is not a member of set2 *)
and setDiff set1 set2 = List.filter (fn x => not (isMem x set2)) set1
and setEQ a b = setDiff a b = [] andalso setDiff b a = []
and rm item set = List.filter (fn i => not (i = item)) set
and rmOnce item [] = []
  | rmOnce item (a::rest) = 
      if item = a
      then rest
      else a::(rmOnce item rest)
and bagDiff [] _ = []
  | bagDiff (a::bag1) bag2 =
    if isMem a bag2
    then     bagDiff bag1 (rmOnce a bag2)
    else a::(bagDiff bag1 (rmOnce a bag2))
and I x = x
;


(* "type checking" for propositions ;
    Return the "type" of a proposition; 
    list of names of input wires * list of names of output wires
    
*)
fun getTypes (VAR(inputs,name,outputs)) = (inputs,outputs)
  | getTypes (L_POSS((ins,_,outs),prop)) =
	let val (inP,outP) = getTypes prop
	in
	  ((setDiff inP outs)@ins, (setDiff outs inP)@outP)
	end
  | getTypes (L_BOX((ins,_,outs),prop)) =
	let val (inP,outP) = getTypes prop
	in
	  ((setDiff inP outs)@ins, (setDiff outs inP)@outP)
	end
  | getTypes (R_POSS(prop,(ins,_,outs))) =
	let val (inP,outP) = getTypes prop
	in
	  ((setDiff ins outP)@inP, (setDiff outP ins)@outs)
	end
  | getTypes (R_BOX(prop,(ins,_,outs))) =
	let val (inP,outP) = getTypes prop
	in
	  ((setDiff ins outP)@inP, (setDiff outP ins)@outs)
	end
  | getTypes (AND (ins,props,outs)) = (ins,outs)
  | getTypes (OR (ins,props,outs)) = (ins,outs)
  | getTypes (ABS(inputPairs,p,outputPairs)) =
	let val (ins,outs) = getTypes p
	    val exposedIns = setDiff ins (map snd inputPairs)
	    val exposedOuts = setDiff outs (map fst outputPairs)
	in
	  ((map fst inputPairs)@exposedIns, exposedOuts@(map snd outputPairs))
	end
;


fun comma [] = ""
  | comma [a] = a
  | comma (a::rest) = a^ "," ^ (comma rest)
and dups [] = []
  | dups (a::rest) = 
    if isMem a rest 
    then a::(dups rest) 
    else dups rest
and dupError errorMsg set =
	if hasNoDups set
	then "" 
	else errorMsg ^ concat (dups set)
and eqError errorMsg set1 set2 =
	if setEQ set1 set2
	then ""
	else errorMsg ^ (comma (setDiff set1 set2)) ^ 
			(comma (setDiff set2 set1))
and sameType (ins,outs) c =
    let val (inC,outC) = getTypes c
    in
      (eqError "Inputs don't match " ins inC) ^
      (eqError "Outputs don't match " outs outC)
    end
;


(* Generate error messages for "badly typed circuits" *)

	
fun compErr msg (in1,out1) (in2,out2) =
      (dupError msg (in1@(setDiff in2 out1))) ^
      (dupError msg (out2@(setDiff out1 in2)) ) 
;

    
fun propErrors (VAR(ins,A,outs)) =
	(dupError ("Duplicate inputs in component " ^ A ^ "  ") ins) ^
	(dupError ("Duplicate outputs in component " ^ A ^ "  ") outs) 
  | propErrors (AND(inputs,props,outputs)) =
	(dupError "Duplicate input wires in AND " inputs) ^
	(dupError "Duplicate output wires in AND " outputs) ^
    	(concat (map propErrors props) ) ^ 
    	(concat (map (sameType (inputs,outputs)) props))
  | propErrors (OR(inputs,props,outputs)) =
	(dupError "Duplicate input wires in OR " inputs) ^
	(dupError "Duplicate output wires in OR " outputs) ^
    	(concat (map propErrors props) ) ^ 
    	(concat (map (sameType (inputs,outputs)) props))
  | propErrors (ABS(inPairs,p,outPairs)) =
      let val (inputs,outputs) = getTypes p
	  val inWires = List.filter (fn (x,y) => not (isMem y inputs)) inPairs
	  val outWires = List.filter 
			(fn (x,y) => not (isMem x outputs)) outPairs
      in
        concat [propErrors p,
	eqError "Abstraction mismatch "  
		(map snd inWires) (map fst outWires),
	dupError "Abstraction input Duplicate " (map fst inPairs) ,
	dupError "Abstraction input Duplicate " (map snd inPairs) ,
	dupError "Abstraction output Duplicate " (map fst outPairs) ,
	dupError "Abstraction output Duplicate " (map snd outPairs)]
      end
  | propErrors (L_POSS((ins,a,outs),prop)) =
      (compErr ("POSS " ^ a ^ " ")  (ins,outs) (getTypes prop)) ^ 
      (propErrors prop)
  | propErrors (L_BOX((ins,a,outs),prop)) =
      (compErr ("BOX " ^ a ^ " ") (ins,outs) (getTypes prop)) ^ 
      (propErrors prop)
  | propErrors (R_POSS(prop,(ins,a,outs))) =
      (compErr ("SSOP " ^ a ^ " ") (getTypes prop) (ins,outs)) ^ 
      (propErrors prop)
  | propErrors (R_BOX(prop,(ins,a,outs))) =
      (compErr ("XOB " ^ a ^ " ") (getTypes prop) (ins,outs)) ^ 
      (propErrors prop)
;

(* TESTS

CIRCUITS.circuitErrors (CIRCUITS.CIR_COMPONENT(["a","a"],"A",["b","c"]));
CIRCUITS.circuitErrors (CIRCUITS.CIR_AND(["a","a"],[],["b","c"]));

*)

(* ***************************************************************
fun ppWires wires = "[" ^ (comma wires) ^ "]"
;

fun ppCircuitText (CIR_COMPONENT(ins,A,outs)) =
	[(ppWires ins) ^ A ^ (ppWires outs)]
  | ppCircuitText (CIR_AND(inputs,circuits,outputs)) =
    	((ppWires inputs) ^ "AND ( " ) ::
	(List.concat (map ppCircuitText circuits)) @ 
	[ " )" ^ (ppWires outputs)]
  | ppCircuitText (CIR_ABS(inPairs,c,outPairs)) =
        ((ppWires (map fst inPairs)) ^ "<" ^ (comma (map snd inPairs)) ^ " | ")
	:: (ppCircuitText c) @ [comma (map fst outPairs) ^ " > ",
			        ppWires (map snd outPairs)]
  | ppCircuitText (CIR_COMPOSITION(components)) =
	(case map ppCircuitText components of
	   [] => []
	 | [a] => a
	 | (a::rest) => List.concat (a:: (map (fn x => " ; " :: x) rest))
	)
;
*************************************************************** *)


(* Try again for pretty print; this time use indenting *)

(* Given a list of strings with an indent, join together the strings
   until greater than the linewidth.  At that point, create a 
   new line and indent by "indent".  
*)
fun joinList indent [] = ""
  | joinList indent [s] = s
  | joinList indent (s1::s2::rest) =
    if size s1 + size s2 + size indent < (!Compiler.Control.Print.linewidth)
    then joinList indent (s1^s2::rest)
    else s1 ^ "\n" ^ indent ^(joinList indent (s2::rest))
;


(* comma has a nil case and a cons case *)
fun comma n c [] = [n]
  | comma n c [a] = [a^n]
  | comma n c (a::rest) = (a^c)::(comma n c rest)
and commaHead n c h [] = [h ^ n]
  | commaHead n c h [a] = [h ^ a ^ n]
  | commaHead n c h (a::rest) = comma n c (h ^ a :: rest)
(* AND's are "AND(" separated by commas terminated with a ") " *)
(* Wires's are "{" separated by commas terminated with a "} " *)
(* Components's are "" separated by semicolons terminated with a "" *)
and ppAnd l = commaHead ")" "," "AND (" l
and ppOR  l = commaHead ")" "," "OR (" l
and ppWires l = commaHead "} " ", " "{" l
and ppLAbs l = commaHead "|" ", " "<" l
and ppRAbs l = commaHead ">" ", " "|" l
and ppAct lBkt rBkt (ins,a,outs) = 
		[lBkt]@(ppWires ins)@[a]@(ppWires outs)@[rBkt]
;

local
fun ppP indent (VAR(ins,A,outs)) =
	joinList indent ((ppWires ins) @ [A] @ (ppWires outs))
  | ppP indent (AND(inputs,circuits,outputs)) =
	joinList indent ((ppWires inputs) @ 
	  [joinList (indent ^ "  ")
		(ppAnd (map (ppP (indent ^ "  ")) circuits))] @
	  (ppWires outputs))
  | ppP indent (OR(inputs,circuits,outputs)) =
	joinList indent ((ppWires inputs) @ 
	  [joinList (indent ^ "  ")
		(ppOR (map (ppP (indent ^ "  ")) circuits))] @
	  (ppWires outputs))
  | ppP indent (ABS(inPairs,c,outPairs)) =
	joinList indent ((ppWires (map fst inPairs)) @
		(ppLAbs (map snd inPairs)) @ 
		[ppP (indent ^ "  ") c] @
		(ppRAbs (map fst outPairs)) @
		(ppWires (map snd outPairs)))

(* FIX THIS *)
  | ppP indent (L_POSS(act,p)) =
	joinList indent ((ppAct "<" ">" act)@["("]@[ppP indent p]@[")"])
  | ppP indent (L_BOX(act,p)) =
	joinList indent ((ppAct "[" "]" act)@["("]@[ppP indent p]@[")"])
  | ppP indent (R_POSS(p,act)) =
	joinList indent (["("]@[ppP indent p]@[")"]@(ppAct "<" ">" act))
  | ppP indent (R_BOX(p,act)) =
	joinList indent (["("]@[ppP indent p]@[")"]@(ppAct "[" "]" act))
in
fun ppPropText prop = ppP "" prop
end
;

local
(* Wires's are "{" separated by commas terminated with a "} " *)
(* Components's are "" separated by semicolons terminated with a "" *)
fun commaH nilCase commaCase headCase l = 
	String.concat (commaHead nilCase commaCase headCase l)
and ppAnd l = commaH ")" "," "\\AND (" l
and ppOR  l = commaH ")" "," "\\OR (" l
and ppWires l = commaH "\\} " ", " "\\{" l
and ppLAbs l = commaH "\\mid " ", " "\\langle " l
and ppRAbs l = commaH "\\rangle " ", " "\\mid " l
and ppAct lBkt rBkt (ins,a,outs) = 
		lBkt ^ (ppWires ins) ^ a ^ (ppWires outs) ^ rBkt
and ppP (VAR(ins,A,outs)) =
	((ppWires ins) ^ A ^ (ppWires outs))
  | ppP (AND(inputs,circuits,outputs)) =
	((ppWires inputs) ^ (ppAnd (map ppP  circuits)) ^ (ppWires outputs))
  | ppP (OR(inputs,circuits,outputs)) =
	((ppWires inputs) ^ (ppOR (map ppP  circuits)) ^ (ppWires outputs))
  | ppP (ABS(inPairs,c,outPairs)) =
	((ppWires (map fst inPairs)) ^
		(ppLAbs (map snd inPairs)) ^ (ppP c) ^
		(ppRAbs (map fst outPairs)) ^ (ppWires (map snd outPairs)))

(* FIX THIS *)
  | ppP (L_POSS(act,p)) =
	((ppAct "\\langle " "\\rangle " act) ^ "(" ^ (ppP p) ^ ")")
  | ppP (L_BOX(act,p)) =
	((ppAct "[" "]" act) ^ "(" ^ (ppP p) ^ ")")
  | ppP (R_POSS(p,act)) =
	("(" ^ (ppP p) ^ ")" ^ (ppAct "<" ">" act))
  | ppP (R_BOX(p,act)) =
	("(" ^ (ppP p) ^ ")" ^ (ppAct "[" "]" act))
in
fun ppPropLatex prop = ppP prop
end
;


(* 
COMPILE "<{x,y}A{w,z}> {w,z}B{ww,zz}";
ppPropLatex it;

COMPILE "{x,y}A{w,z}";
ppPropLatex it;
COMPILE "{x,y}<x1,x2 | {x1}A{y1} | y1,x2>{x,y}" ;
ppPropLatex it;
COMPILE "{x} &(){y} " ; 
ppPropLatex it;

*)


(***********************************************)
(***********************************************)
(******************* COMPILING *****************)
(***********************************************)
(***********************************************)


exception COMPILE_ERROR of string

fun WARN why [] =  ("***" ^ why ^ "***\n")
  | WARN why restOfSrc
  = ( ("***" ^ why ^ "***\n")
           ^ "rest of src = " 
           ^ (String.concat restOfSrc) 
           ^ "\n")

fun WARNING why restOfSrc
  = ( print  (WARN why restOfSrc))

fun ERROR why restOfSrc
  = ( raise COMPILE_ERROR (WARN why restOfSrc) ) 

(***********************************************)
(***********************************************)
(******************* LEXING  *******************)
(***********************************************)
(***********************************************)


(* Lexemes:

    X                            	Variable
    & + 		 		Operator
    ( ) { } <> | ,                	Bracket

*)

fun isDigit x     = ("0" <= x andalso x <= "9")  
fun isletter x    = ("a" <= x andalso x <= "z")  
fun isLetter x    = ("A" <= x andalso x <= "Z") 
fun isSpecial x   = isMem x ["-","'"]
fun isAlpha x     = isletter x orelse isLetter x 
fun isVarChar x   = isletter x orelse isLetter x orelse isDigit x orelse 
			isSpecial x
fun isWhite x     = isMem x [ " ", "\n", "\t", "\^L", "\^M" ] 
fun isBra x       = isMem x [ "(", ")", "{", "}", "<", ">", "|", "[","]", ","] 
fun isOp x        = isMem x ["&", "+" ]  	

fun getWhile p res []     = (res, [])
  | getWhile p res (a::L) = if p a then getWhile p (res^a) L else (res, a::L) 

val getVar = getWhile isVarChar 

datatype lexeme
  = l_VAR    of string
  | l_BRA    of string
  | l_EOL
  | l_OP     of string 
;


fun l_show (l_VAR a) = a
  | l_show (l_BRA p)    = p
  | l_show (l_EOL)      = "EOLEX"
  | l_show (l_OP opr)   = opr 
;


local

fun getLex (a::src) 
  = if isWhite a then getLex src else

    if isAlpha a then 
      let val (id, src') = getVar a src
      in
          (l_VAR id, src')
      end else


    if isBra a then (l_BRA a, src) else
    if isOp  a then (l_OP  a, src) else

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

   fun LEX L = lex [] ((map str (explode L)) @ [" "])

end 
;

(***********************************************)
(***********************************************)
(******************* SYNTAX ********************)
(***********************************************)
(***********************************************)

(*  SYNTAX:

 Prop ::= {wires}COMPONENT{wires}
        |  AND (Prop list)
        |  OR  (Prop list)
        |  {wires}AND(Prop list){wires}
        |  {wires}OR(Prop list){wires}
        |  {wires}<wires | Prop | wires>{wires}
	|  <act>Prop
	|  [act]Prop
	|  Prop<act>
	|  Prop[act]
	|  (Prop)


*)


fun checkLex lex []     = false
  | checkLex lex (a::L) = (lex = a) 

fun parseLex lex []     = ERROR ("Expecting " ^ (l_show lex)) []
  | parseLex lex (a::L) = if lex = a then L else
                          ERROR ("Expecting " ^ (l_show lex)) (map l_show L) 
;


local
fun parseNames L terminal ((l_VAR a)::src)
      = parseNames (L @ [ a ]) terminal  src
  | parseNames L terminal ((l_BRA ",")::src) = parseNames L terminal src
  | parseNames L terminal ((l_BRA a)::src) 
      = if terminal = a
	then (L, src)
	else ERROR "Expecting name list" (map l_show src)
  | parseNames L terminal src
      = ERROR "Expecting name list" (map l_show src)
and parseAct terminal ((l_BRA "{") :: tokens) =
    (case parseNames [] "}" tokens of
      (ins,(l_VAR name)::rest) =>
	  let val (outs,rest1) = parseNames [] "}" 
				(parseLex (l_BRA "{") rest)
	  in
	    ((ins,name,outs),parseLex (l_BRA terminal) rest1)
	  end
    | (_,rest) => ERROR "Expecting action: found " (map l_show rest)
    )
  | parseAct terminal tokens =
	ERROR "Expecting action: found " (map l_show tokens)
in
fun PARSE text = 
let 
fun parseProp ((l_BRA "<")::tokens) = 
    let val (act, rest) = parseAct  ">" tokens 
        val (prop,rest1) = parseProp rest
    in
      parseR (L_POSS(act,prop),rest1)
    end
  | parseProp ((l_BRA "[")::tokens) = 
    let val (act, rest) = parseAct "]" tokens 
        val (prop,rest1) = parseProp rest
    in
      parseR (L_BOX(act,prop),rest1)
    end
  | parseProp ((l_BRA "(")::tokens) = 
    let val (prop, rest) = parseProp tokens 
    in
      parseR (prop,parseLex (l_BRA ")") rest)
    end
  | parseProp ((l_BRA "{")::tokens) =
    (case parseNames [] "}" tokens of
      (inActs,(l_OP "&")::rest) => 
	  let val (props,rest1) = parseAndOrs rest
	      val (outActs,rest2) = parseNames [] "}" 
					(parseLex (l_BRA "{") rest1)
	  in
	    parseR (AND(inActs,props,outActs),rest2)
	  end
    | (inActs,(l_OP "+")::rest) => 
	  let val (props,rest1) = parseAndOrs rest
	      val (outActs,rest2) = parseNames [] "}" 
					(parseLex (l_BRA "{") rest1)
	  in
	    parseR (OR(inActs,props,outActs),rest2)
	  end
    | (inActs,(l_VAR name)::rest) =>
	  let val (outActs,rest1) = parseNames [] "}" 
					(parseLex (l_BRA "{") rest)
	  in
	    parseR (VAR(inActs,name,outActs),rest1)
	  end
    | (inNames,(l_BRA "<")::rest) => 
	  let val (namesL,rest1) = parseNames [] "|"  rest
	      val (prop,rest2)   = parseProp rest1
	      val (namesR,rest3) = parseNames [] ">" 
					(parseLex (l_BRA "|") rest2)
	      val (outNames,rest4) = parseNames [] "}" 
					(parseLex (l_BRA "{") rest3)
	  in
	    if not (length inNames = length namesL) orelse
	       not (length namesR = length outNames)
            then ERROR "Abstraction/Wires mismatch"
                                        (inNames @ namesL @ namesR @ outNames)
            else parseR (ABS(ListPair.zip (inNames,namesL),
                                        prop,
                                        ListPair.zip (namesR,outNames)),rest4)

	  end
    | (inActs,rest) => ERROR "Expecting AND or OR " (map l_show rest)
    )
  | parseProp ((l_OP "&")::tokens) = 
    (case parseAndOrs tokens of
      ([],rest) => ERROR "Empty AND Needs Types " (map l_show tokens)
    | (p::props,rest) => 
	let val (ins,outs) = getTypes p
	in
	  parseR (AND(ins,p::props,outs),rest)
	end
    )
  | parseProp ((l_OP "+")::tokens) = 
    (case parseAndOrs tokens of
      ([],rest) => ERROR "Empty OR Needs Types " (map l_show tokens)
    | (p::props,rest) => 
	let val (ins,outs) = getTypes p
	in
	  parseR (OR(ins,p::props,outs),rest)
	end
    )
  | parseProp tokens = ERROR "Expecting a prop, found " (map l_show tokens)
and parseR (prop,(l_BRA "<")::tokens) =
    let val (act, rest) = parseAct ">" tokens
    in
      parseR (R_POSS(prop,act),rest)
    end
  | parseR (prop,(l_BRA "[")::tokens) =
    let val (act, rest) = parseAct "]" tokens
    in
      parseR (R_BOX(prop,act),rest)
    end
  | parseR (prop,tokens) = (prop,tokens)
and parseAndOrs tokens =
    let fun pAOs soFar ((l_BRA ")")::rest) = (soFar,rest)
	  | pAOs soFar tokens =
	    (case parseProp tokens of
	      (lhs, (l_BRA ",")::rest) => pAOs (soFar@[lhs]) rest
	    | (lhs, (l_BRA ")")::rest) => (soFar@[lhs],rest)
	    | (lhs,rest) => ERROR "Missing Comma or Bracket in And"
		(map l_show rest)
	    )
    in
      pAOs [] (parseLex (l_BRA "(") tokens)
    end
in
  let val (prop, lexlist) = parseProp (LEX text)
    in
      if not (lexlist = []) 
      then ERROR "Unparsed stuff at end of line; Missing Operator?"
	   (map l_show lexlist)
      else prop
    end
end
end
;



fun COMPILE text =
    let val prop = PARSE text
    in
      (case propErrors prop of
	"" => prop
(*      | errors => raise COMPILE_ERROR ("Type Errors: " ^ errors) *)
	| errors => (print errors; prop)
      )
    end
;

(* Tests:
fun comp text =
    (print (ppPropText (COMPILE text))) 
	handle COMPILE_ERROR (msg) => print msg
;




COMPILE "{x,y}A{w,z}";

COMPILE "<{x,y}A{w,z}> {w,z}B{ww,zz}";

comp "&({x,y}A{w,z} , {x,y}B{w,z})";

comp "A{w,z}" ; (* error *)

comp " &() " ; (* error *)


comp "{x} &(){y} " ; 

comp "{x}AAA{w}; &()" ; (* error *)

comp "{x}A{yy};{x}B{zz}" ; (* error *)

comp "&({xx}A{yy}, {x}B{yy})"; (* error *)
comp "&({x}A{yy}, {x}B{yy})";  (* no error *)

comp "{x,y}<x1,x2 | {x1}A{y1} | y1,x2>{x,y}" ;

comp "{x,y}<x1,x2 | {x1,x2}A{y1} | y1,x2>{x,y}" ; (* error *)



*)


fun reverse (VAR (ins,c,outs)) = VAR (outs,c,ins)
  | reverse (L_POSS((ins,a,outs),p)) = R_POSS(p,(outs,a,ins))
  | reverse (L_BOX((ins,a,outs),p)) = R_BOX(p,(outs,a,ins))
  | reverse (R_POSS(p,(ins,a,outs))) = L_POSS((outs,a,ins),p)
  | reverse (R_BOX(p,(ins,a,outs))) = L_BOX((outs,a,ins),p)
  | reverse (AND(ins,props,outs)) = AND(outs,map reverse props,ins)
  | reverse (OR(ins,props,outs)) = OR(outs,map reverse props,ins)
  | reverse (ABS(inPairs,prop,outPairs)) =
        ABS(map swap outPairs,reverse prop, map swap inPairs)
;

fun propFold f (VAR component) = f (VAR component)
  | propFold f (L_POSS(act,prop)) = f (L_POSS(act, propFold f prop))
  | propFold f (R_POSS(prop,act)) = f (R_POSS(propFold f prop,act))
  | propFold f (L_BOX(act,prop)) = f (L_BOX(act, propFold f prop))
  | propFold f (R_BOX(prop,act)) = f (R_BOX(propFold f prop,act))
  | propFold f (AND(ins,props,outs)) =
        f (AND(ins, map (propFold f) props,outs))
  | propFold f (OR(ins,props,outs)) =
        f (OR(ins, map (propFold f) props,outs))
  | propFold f (ABS(inPairs,prop,outPairs)) =
        f (ABS(inPairs,propFold f prop, outPairs))
;




(* Abstraction Elimination:
   ins<inL, c, outR>outs => 
	(ins=inL) ; c ; (outR = outs) =>
	(ins'=inL') ; c' ; (outR' = outs') 
	(where (c';(outR'=outs')) = (relabelWires (ins/inL) (c;(outR=outs)))
	 and then eliminate from ins=inL all wires which are in 
	 (c';(outR'=outs')).  )
*)


(* Think of "wireSubst" as being compostion of wirePairs with wire.
   i.e. (x,y);y = x
*)
fun wireSubst wirePairs wire =
    (case List.find (fn (x,y) => y = wire) wirePairs of
      NONE => wire
    | SOME(wire',_) => wire'
    )
;


(* It can never be the case that composing "wires" as in xs=ys;Component
   will result in a duplicate wire name on the input side
   but it could result in capture:
    x=y;{z}C{y};{x}D{z}
   In it is possible to rename internal wires,
   but above, the "y" wire goes all the way outside 
   and can't be renamed unless the external interface gets renamed too.  
   HOWEVER, if it is a Modal Circuit, the "y" wire doesn't go all the way
   out and it is possible to rename from the outside in.  
*)


fun mergeWirePairs pair1 pair2 =
    let val out1 = map snd pair1
	val in2  = map fst pair2
	val (comp1,nonComp1) = List.partition (fn (x,y) => isMem y in2) pair1
	val (comp2,nonComp2) = List.partition (fn (x,y) => isMem x out1) pair2
	val composed = map (fn (x,y) => (wireSubst pair1 x,y)) comp2
    in
      nonComp1@nonComp2@composed
    end
;

fun reWireL  [] c  =  c
  | reWireL wirePairs (VAR(ins,c,outs)) =
	VAR(map (wireSubst wirePairs) ins,c,outs)
  | reWireL wirePairs (AND(ins,props,outs)) =
	let val ins' = map (wireSubst wirePairs) ins
	in
	  AND(ins', map (reWireL wirePairs) props, outs)
	end
  | reWireL wirePairs (OR(ins,props,outs)) =
	let val ins' = map (wireSubst wirePairs) ins
	in
	  AND(ins', map (reWireL wirePairs) props, outs)
	end
  | reWireL wirePairs (ABS(inPairs,prop,outPairs)) =
      let val wirePairs' = mergeWirePairs wirePairs inPairs
      in
	if outPairs = []
	then reWireL wirePairs' prop
	else ABS([],reWireL wirePairs' prop,outPairs)
	(* uses implicitly the identity of ABS([],p,[]) = p *)
      end
  | reWireL wirePairs (L_POSS((ins,act,outs),prop)) =
      let val ins' = map (wireSubst wirePairs) ins
	  val wirePairs' = List.filter (fn (x,y) => not (isMem y ins)) 
				wirePairs
	  val outs' = map (wireSubst (map swap wirePairs')) outs
	  val capturedWires = 
		map swap (List.filter (fn (x,y) => isMem x outs) wirePairs')
	  val wirePairs'' = capturedWires@wirePairs'
      in
	    L_POSS((ins',act,outs'),reWireL wirePairs' prop)
      end
  | reWireL wirePairs (L_BOX((ins,act,outs),prop)) =
      let val ins' = map (wireSubst wirePairs) ins
	  val wirePairs' = List.filter (fn (x,y) => not (isMem y ins))
				wirePairs
	  val outs' = map (wireSubst (map swap wirePairs')) outs
	  val capturedWires = 
		map swap (List.filter (fn (x,y) => isMem x outs) wirePairs')
	  val wirePairs'' = capturedWires@wirePairs'
      in
	    L_BOX((ins',act,outs'),reWireL wirePairs' prop)
      end
  | reWireL wirePairs (R_POSS(prop,act)) =
	R_POSS(reWireL wirePairs prop,act)
  | reWireL wirePairs (R_BOX(prop,act)) =
	R_BOX(reWireL wirePairs prop,act)
and reWireRight wirePairs prop =
	let val prop' = reWireL (map swap wirePairs) (reverse prop)
	in
	  reverse prop'
	end
;


fun aeOnce (ABS(inPairs,prop,outPairs)) =
    reWireRight outPairs (reWireL inPairs prop) 
  | aeOnce p = p
and abstractionElimination prop = propFold aeOnce prop
;


end

