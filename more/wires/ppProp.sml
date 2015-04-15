structure ppProp :> ppProp =
struct


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

fun comma [] = ""
  | comma [a] = a
  | comma (a::rest) = a^ "," ^ (comma rest)
and sep separator [] = ""
  | sep separator [a] = a
  | sep separator (a::rest) = a ^ separator ^ (sep separator rest)
and ppAct (ins,a,outs) = String.concat ((ppWires ins)@[a]@(ppWires outs))
;


fun ppSequentLatex (ins,guardsL,lhs,rhs,guardsR,outs) =
        (comma ins) ^ "~\\mid~ " ^
        (comma (map (ppAct o fst) guardsL)) ^ ("~\\mid~") ^
        (comma (map ppPropLatex lhs)) ^"\\vdash " ^
        (comma (map ppPropLatex rhs)) ^"~\\mid~"  ^
        (comma (map (ppAct o snd) guardsR)) ^"~\\mid~"  ^
        (comma outs)
and ppRule LFALSE = "\\mbox{\\cal F}"
  | ppRule RTRUE = "\\mbox{\\cal T}"
  | ppRule LAND = "\\mbox{L-\\AND}"
  | ppRule ROR  = "\\mbox{R-\\OR}"
  | ppRule LOR  = "\\mbox{L-\\OR}"
  | ppRule RAND = "\\mbox{R-\\AND}"
  | ppRule ID   = "\\mbox{ID}"
  | ppRule LPOSS  = "\\mbox{L-POSS}"
  | ppRule RBOX  = "\\mbox{R-BOX}"
  | ppRule LSSOP  = "\\mbox{L-SSOP}"
  | ppRule RXOB  = "\\mbox{R- XOB}"
  | ppRule LBOX  = "\\mbox{L-BOX}"
  | ppRule RPOSS  = "\\mbox{R-POSS}"
  | ppRule LXOB  = "\\mbox{L-XOB}"
  | ppRule RSSOP  = "\\mbox{R- SSOP}"
  | ppRule WEAKP  = "\\mbox{weakp}"
  | ppRule WEAKG  = "\\mbox{weakg}"
  | ppRule (CUT x) = "\\mbox{cut}"
  | ppRule GUARDL  ="\\mbox{GuardL}"
  | ppRule GUARDR  ="\\mbox{GuardR}"
and ppJudgement (ASSUMPTION(a)) = ppSequentLatex a
  | ppJudgement (JUDGEMENT(rule,conclusion,assumptions)) =
      "\\infer[" ^ (ppRule rule) ^ "]{" ^ (ppSequentLatex conclusion) ^ "}{\n" ^
        (sep " & " (map ppJudgement assumptions)) ^ "}\n"
;


val foldCmds =
    "\\newcommand{\\muInfer}[3] { \n" ^
    "\\ensuremath{\n" ^
    "\\begin{array}[b]{c}\n" ^
    "  \\begin{array}[b]{|c}\n" ^
    "  \\hline \n" ^
    "  #2 \\\\ \n" ^
    "  \\hline \\\\ \n" ^
    "  #3 \\\\ \n" ^
    "  \\hline  \n" ^
    "  \\end{array} \\\\ \n" ^
    "#1 \\\\ \n" ^
    "\\end{array}\\\\ \n" ^
    "}\n" ^
    "}\n" ^
    "\\newcommand{\\nuInfer}[3] { \n" ^
    "\\ensuremath{\n" ^
    "\\begin{array}[b]{c}\n" ^
    "  \\begin{array}[b]{c|}\n" ^
    "  \\hline \n" ^
    "  #2 \\\\ \n" ^
    "  \\hline \\\\ \n" ^
    "  #3 \\\\ \n" ^
    "  \\hline  \n" ^
    "  \\end{array} \\\\ \n" ^
    "#1 \\\\ \n" ^
    "\\end{array}\\\\ \n" ^
    "}\n" ^
    "}\n" ^
    "\n"
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
        "\\newcommand{\\NU}        {\\mbox{$\\nu$}} \n" ^
        "\\newcommand{\\MU}        {\\mbox{$\\mu$}} \n" ^
        "\\newcommand{\\VDASH}        {\\mbox{$\\vdash$}} \n" ^
        foldCmds ^
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

