structure StateHml :> StateHml =
  struct

(* A Labelled Transition System has States, 
   with Transitions to other States
   Notionally, if everything is finite (no recursion) 
   it can be represented as a tree, with labelled transitions to new states.
*)

datatype state = STATE of (string * state) list
;

(* Create a counter Example for a failed proof 
   This relies on the "dp" program 
*)

exception HAS_A_PROOF;

fun counter (ASSUMPTION(lhs,rhs)) = 
    (case dp (ASSUMPTION(lhs,rhs)) of
	 (JUDGEMENT j) => counter (JUDGEMENT j)
    | (ASSUMPTION(lhs,rhs)) => 
      (* Start of search *)
      let val possGoals = List.concat (
		map (fn (POSS(a,p)) => 
			[possRule  a p  (ASSUMPTION(lhs,rhs))] | _ => []) lhs)
          val boxGoals = List.concat (
		map (fn (BOX(a,p)) => 
			[boxRule  a p  (ASSUMPTION(lhs,rhs))] | _ => []) rhs)
	  val states = map counter (possGoals@boxGoals)
	  val lhsVars = List.concat (map (fn (VAR v) => [v] | _ => []) lhs)
	  val rhsVars = List.concat (map (fn (VAR v) => ["-"^v] | _ => []) rhs)
	  val stateVars = map (fn v => (v,STATE[])) (lhsVars@rhsVars)
       in
	 STATE(stateVars@(List.concat (map (fn (STATE s) => s) states)))
       end
    )
  | counter (JUDGEMENT(POSS_R(a,p), sequent, premises)) = 
		STATE(map (fn p => (a,counter p)) premises)
  | counter (JUDGEMENT(BOX_R (a,p), sequent, premises)) = 
		STATE(map (fn p => (a,counter p)) premises)
  (* for both of these cases, (box/poss) there ought to be only one premise *)
  | counter (JUDGEMENT(rule,sequent,premises)) =
    (case List.filter (fn p => not (isAProof p)) premises of
      [] => raise HAS_A_PROOF
    | (p::_) => counter p
    )
    (* for the cases of L-AND/R-OR, there is only one premise;
       for the cases of L-OR/R-AND, (distribution) pick a "failing" premise
       for the cases of Weak-R/Weak-L, there is only one premise
       for the cases of "cut" it ought to have been eliminated
    *)
;

(* A "pretty Printer" for states; print in the CCS style;
   . = sequence, + = Choice
*)

local
fun comma [] = ""
  | comma [a] = a
  | comma (a::rest) = a^" + " ^ (comma rest)
in
fun ppStateText (STATE([])) = "0"
  | ppStateText (STATE([(a,STATE [])])) = 
	(* If first char isLower, then is an action, else is Var *)
	if String.size a = 0 orelse Char.isLower (String.sub (a,0))
	then a ^ ".0"
	else a
  | ppStateText (STATE([(a,state)])) = a ^ "." ^ (ppStateText state)
  | ppStateText (STATE(transitions)) = (
    "(" ^ (comma 
	(map (fn (a,b) => a ^ "." ^ (ppStateText b)) transitions)) ^ ")" )
end
;

(* A pretty printer for states, in Latex Style 
fun stateToTree (STATE([])) = Tree.Node("0",[])
  | stateToTree (STATE([(a,STATE [])])) = 
    if String.size a = 0 orelse Char.isLower (String.sub (a,0))
    then Tree.Node("",[(a,Tree.Node("0",[]))])
    else Tree.Node(a,[])
  | stateToTree (STATE(children)) = 
	Tree.Node("", map (fn (a,t) => (a, stateToTree t)) children)
;

fun ppStateLatex s = Draw.picture (stateToTree s);
*)


(* Duplicate of ppProp.latexProlog; I don't need proof,Box,Poss,AND,OR *)
(* I do need eepic *)
val latexProlog = 
	"\\documentclass[]{book}\n" ^
	"\\usepackage{proof}\n" ^
	"\\usepackage{epic}\n" ^
	"\\usepackage{eepic}\n" ^
	"\\textheight 100cm\n" ^
	"\\textwidth 100cm \n" ^
	"\\newcommand{\\Poss}[1]   {\\mbox{$\\langle${#1}$\\rangle$}} \n" ^
	"\\newcommand{\\Bx}[1]     {\\mbox{$[${#1}$]$}} \n" ^
	"\\newcommand{\\AND}       {\\mbox{$\\bigwedge$}} \n" ^
	"\\newcommand{\\OR}        {\\mbox{$\\bigvee$}} \n" ^
	"\\begin{document} \n" ^
	"\\pagestyle{empty} \n" ;

val latexEpilog = "\\end{document}\n"

fun latexCaption caption = "\n \\section*{" ^ caption ^ "}\n" ;

(* Another way to draw a picture *)

val latexProlog = 
	"\\documentclass[]{book}\n" ^
	"\\usepackage{proof}\n" ^
	"\\usepackage{epic}\n" ^
	"\\usepackage{eepic}\n" ^
	"\\usepackage{ecltree}\n" ^
	"\\textheight 100cm\n" ^
	"\\textwidth 100cm \n" ^
	"\\newcommand{\\Poss}[1]   {\\mbox{$\\langle${#1}$\\rangle$}} \n" ^
	"\\newcommand{\\Bx}[1]     {\\mbox{$[${#1}$]$}} \n" ^
	"\\newcommand{\\AND}       {\\mbox{$\\bigwedge$}} \n" ^
	"\\newcommand{\\OR}        {\\mbox{$\\bigvee$}} \n" ^
	"\\begin{document} \n" ^
	"\\pagestyle{empty} \n" ;


(* A pretty printer for states, in Latex Style *)
fun ppStateLatex  (STATE([])) = "0"
  | ppStateLatex  (STATE([(a,STATE [])])) = 
    if String.size a = 0 orelse Char.isLower (String.sub (a,0))
    then "\\begin{bundle}{" ^a ^ ".0}\\chunk[" ^ a ^ "]{0}\n\\end{bundle}\n"
    else a
  | ppStateLatex  (STATE(children)) = 
	"\\begin{bundle}{" ^ (ppStateText (STATE(children))) ^ 
	"}\n" ^ (foldr (op ^) "\\end{bundle}\n" 
	(map (fn (a,child) => "\\chunk[" ^ a ^ "]{\n" ^ (ppStateLatex child)
	^"}\n") children))
;


end
;
