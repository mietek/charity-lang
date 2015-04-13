structure ppProp :> ppProp =
struct
fun comma [] = ""
  | comma [a] = a
  | comma (a::rest) = a^ "," ^ (comma rest)
and sep separator [] = ""
  | sep separator [a] = a
  | sep separator (a::rest) = a ^ separator ^ (sep separator rest)
;

fun precedence (AND []) = 3
  | precedence (OR [])  = 3
  | precedence (BOX _)  = 2
  | precedence (POSS _) = 2
  | precedence (AND _)  = 1
  | precedence (OR  _)  = 0
  | precedence (VAR _)  = 3
and bkt env self str =
      if (precedence env) > (precedence self)
      then "(" ^ str ^ ")"
      else str
;

local
fun ppProp env (AND []) = "T"
  | ppProp env (AND [p]) = ppProp env p
  | ppProp env (self as (AND (p::props))) =
        bkt env self (
        (sep "\\AND " (map (ppProp self) (p::props))))
  | ppProp env (OR []) = "F"
  | ppProp env (self as (OR [p])) = ppProp env p
  | ppProp env (self as (OR (p::props))) =
        bkt env self (
        (sep "\\OR " (map (ppProp self) (p::props))))
  | ppProp env (self as (BOX(a,p))) = "\\Bx{" ^ a ^ "}" ^ (ppProp self p)
  | ppProp env (self as (POSS(a,p))) = "\\Poss{" ^ a ^ "}" ^ (ppProp self p)
  | ppProp env (VAR v) = v
in
fun ppPropLatex p = ppProp p p  (* no brackets at outer level *)
and ppSequentLatex (lhs,rhs) =
        (comma (map ppPropLatex lhs)) ^ " \\vdash " ^
        (comma (map ppPropLatex rhs))
and ppRule LFALSE = "\\mbox{\\cal F}"
  | ppRule RTRUE = "\\mbox{\\cal T}"
  | ppRule LAND = "\\mbox{L-\\AND}"
  | ppRule ROR  = "\\mbox{R-\\OR}"
  | ppRule LOR  = "\\mbox{L-\\OR}"
  | ppRule RAND = "\\mbox{R-\\AND}"
  | ppRule ID   = "\\mbox{ID}"
  | ppRule (BOX_R(a,_))  = "\\mbox{box}"
  | ppRule (POSS_R(a,_))  = "\\mbox{poss}"
  | ppRule WEAK = "\\mbox{weak}"
  | ppRule (CUT x) = "\\mbox{cut}"
and ppJudgement (ASSUMPTION(a)) = ppSequentLatex a
  | ppJudgement (JUDGEMENT(rule,conclusion,assumptions)) =
      "\\infer[" ^ (ppRule rule) ^ "]{" ^ (ppSequentLatex conclusion) ^ "}{\n" ^
        (sep " & " (map ppJudgement assumptions)) ^ "}\n"
end
;


local
fun ppProp env (AND []) = "T"
  | ppProp env (AND [p]) = ppProp env p
  | ppProp env (self as (AND (p::props))) =
        bkt env self (
        (sep "/\\" (map (ppProp self) (p::props))))
  | ppProp env (OR []) = "F"
  | ppProp env (self as (OR [p])) = ppProp env p
  | ppProp env (self as (OR (p::props))) =
        bkt env self (
        (sep "\\/" (map (ppProp self) (p::props))))
  | ppProp env (self as (BOX(a,p))) = "[" ^ a ^ "]" ^ (ppProp self p)
  | ppProp env (self as (POSS(a,p))) = "<" ^ a ^ ">" ^ (ppProp self p)
  | ppProp env (VAR v) = v
in
fun ppPropText p = ppProp p p   (* no brackets at outer level *)
and ppSequentText (lhs,rhs) =
        (comma (map ppPropText lhs)) ^ " |- " ^ (comma (map ppPropText rhs))
end
;


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

fun judgementToLatex caption judgement =
        latexProlog ^ (ppJudgement judgement) ^ (latexCaption caption) ^
                latexEpilog
;

(*
fun printLine p = print (p ^ "\n");
map ppPropText [ AND[],
           OR [],
           VAR "XFIX",
           AND [[AND[], OR[OR[],OR[]],VAR "P"]],
           OR [[AND[], OR[OR[],OR[]],VAR "P"]],
           BOX ("-A,B",POSS("A", (OR[AND[],OR[]]))),
           POSS ("'output", AND[AND[],OR[],VAR "X"]),
           BOX ("-'a,B",POSS("A", OR[OR[]]))
*)
end;

