(* Build a Gentzen style sequent for HML  *)
(* This is the version from Aug 10.  It uses the decision procedure
   in the HML chapter.
*)
(* AND/OR take a set of propositions (not a list);
   Box/Poss take a relation * a proposition
   The constant TRUE = AND [];
   The constant FALSE = OR [];

   There are no fixed points at present.
*)


datatype hml = AND of hml list
             | OR of hml list
             | BOX of string * hml
             | POSS of string * hml
             | VAR of string
;


(*      TEST             *)

fun verbose () =
    (Compiler.Control.Print.printDepth := 30;
     Compiler.Control.Print.printLength := 50;
     Compiler.Control.Print.stringDepth := 150
     );

verbose ();


(* Utilities
   Most of these utilities are for "set as a list"
   I is the identity;
   findLeast will find the "least" element in a non empty list
*)

fun isMem a [] = false
  | isMem a (b::rest) = a = b orelse isMem a rest
and noDups [] = []
  | noDups (a::rest) = if isMem a rest then noDups rest else a::(noDups rest)
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


(* A Tableau is normally a Judgement or an Assumption.
   Notionally, a Judgement holds if all of its parents hold.
   An Assumption is a leaf.
   A Proof is a tableau where there are no Assumptions;
   That is, all leaves are Judgements with an empty set of parents.
   That is sufficient for a completed proof, but we may need to explore.
   Compare this to be an AND/OR tree;
   This just looks like an AND tree
*)

datatype 'a RULE =
        LFALSE | RTRUE | LAND | ROR | LOR | RAND | ID
        | BOX_R of string * 'a | POSS_R of string * 'a  | WEAK | CUT of 'a
;

datatype  'a TABLEAU = JUDGEMENT of (hml RULE * 'a * 'a TABLEAU list)
                  | ASSUMPTION of 'a
;

fun antecedent (ASSUMPTION(a,_)) = a
  | antecedent (JUDGEMENT (_,(a,_),_)) = a
and succedent (ASSUMPTION(_,s)) = s
  | succedent (JUDGEMENT (_,(_,s),_)) = s
;

fun proofFold f (JUDGEMENT(rule,sequent,parents)) =
                f (JUDGEMENT(rule,sequent,map (proofFold f) parents))
  | proofFold f (ASSUMPTION s) = f (ASSUMPTION s)
;

(* another proofFold: This one is "more general"
   The preceeding one folds proofs over proofs.

fun proofFold (a,j) (ASSUMPTION s) = a s
  | proofFold (a,j) (JUDGEMENT(rule,s,children)) =
                j (rule,s,map (proofFold (a,j)) children)
;

fun isAProof' tableau =
        proofFold (fn _ => true, fn (_,_,children) => List.all I children)
;

*)

fun isAProof (ASSUMPTION _) = false
  | isAProof (JUDGEMENT(_,_,parents)) = List.all isAProof parents
;


(* Rules are:

       Cut                      Implication

   L-False, R-True              The only rules which are terminals

   L-And, R-Or,                 Essentially just adding to the set

   L-Or,  R-And                 Distribution

   Poss, Box                    Modal Reduction


        Weak

There is one weaking rule:

            G  |- D
        ----------------
        G U G' |- D U D'

*)

fun lAnd (ASSUMPTION(ante,suc)) =
    let val ante' = List.concat
                        (map (fn (AND props) => props | p => [p]) ante)
    in
      if ante = ante'
      then (ASSUMPTION (ante,suc))
      else (JUDGEMENT(LAND,(ante,suc), [ASSUMPTION(ante',suc)]))
    end
  | lAnd (JUDGEMENT j) = JUDGEMENT j
and rOr (ASSUMPTION(lhs,rhs)) =
    let val rhs' = List.concat
                        (map (fn (OR props) => props | p => [p]) rhs)
    in
      if rhs = rhs'
      then (ASSUMPTION (lhs,rhs))
      else (JUDGEMENT(ROR,(lhs,rhs), [ASSUMPTION (lhs,rhs')]))
    end
  | rOr (JUDGEMENT j) = JUDGEMENT j
and lFalse (ASSUMPTION(lhs,rhs)) =
        if isMem (OR []) lhs
        then (JUDGEMENT (LFALSE,(lhs,rhs),[]))
        else (ASSUMPTION (lhs,rhs))
  | lFalse (JUDGEMENT j) = JUDGEMENT j
and rTrue (ASSUMPTION(lhs,rhs)) =
        if isMem (AND []) rhs
        then (JUDGEMENT (RTRUE,(lhs,rhs),[]))
        else (ASSUMPTION (lhs,rhs))
  | rTrue (JUDGEMENT j) = JUDGEMENT j
and lOr (ASSUMPTION(lhs,rhs)) =
    (case List.find (fn (OR _) => true | _ => false) lhs of
      NONE => ASSUMPTION (lhs,rhs)
    | SOME (OR props) =>
        (JUDGEMENT(LOR,(lhs,rhs),
                map (fn p => (ASSUMPTION(p::(rm (OR props) lhs),rhs))) props))
    )
  | lOr (JUDGEMENT j) = JUDGEMENT j
and rAnd (ASSUMPTION(lhs,rhs)) =
    (case List.find (fn (AND _) => true | _ => false) rhs of
      NONE => ASSUMPTION (lhs,rhs)
    | SOME (AND props) =>
        (JUDGEMENT(RAND,(lhs,rhs),
                map (fn p => ASSUMPTION((lhs,p::(rm (AND props) rhs)))) props))
    )
  | rAnd (JUDGEMENT j) = JUDGEMENT j
and boxRule a p (ASSUMPTION(lhs,rhs)) =
    let val lhsFilter = List.filter (fn (BOX(b,q)) => a = b | _ => false) lhs
        val rhsFilter = List.filter (fn (POSS(b,q)) => a = b | _ => false) rhs
        val lhs' = List.concat (
                map (fn (BOX(b,q)) => if a = b then [q] else [] | _ => []) lhs)
        val rhs' = List.concat (
                map (fn (POSS(b,q)) => if a = b then [q] else [] | _ => []) rhs)
    in
        if isMem (BOX(a,p)) rhs
        then if List.length lhs' = List.length lhs andalso
                List.length rhs' + 1 = List.length rhs
             then (JUDGEMENT(BOX_R(a,p),(lhs,rhs),[ASSUMPTION(lhs',p::rhs')]))
             else JUDGEMENT(WEAK,(lhs,rhs),
                    [JUDGEMENT(BOX_R(a,p),(lhsFilter,BOX(a,p)::rhsFilter),
                    [ASSUMPTION(lhs',p::rhs')])])
        else ASSUMPTION(lhs,rhs)
    end
  | boxRule a p (JUDGEMENT j) = JUDGEMENT j
and possRule a p (ASSUMPTION(lhs,rhs)) =
    let val lhsFilter = List.filter (fn (BOX(b,q)) => a = b | _ => false) lhs
        val rhsFilter = List.filter (fn (POSS(b,q)) => a = b | _ => false) rhs
        val lhs' = List.concat (
                map (fn (BOX(b,q)) => if a = b then [q] else [] | _ => []) lhs)
        val rhs' = List.concat (
                map (fn (POSS(b,q)) => if a = b then [q] else [] | _ => []) rhs)
    in
        if isMem (POSS(a,p)) lhs
        then if List.length lhs' + 1 = List.length lhs andalso
                List.length rhs' = List.length rhs
             then (JUDGEMENT(POSS_R(a,p),(lhs,rhs),[ASSUMPTION(p::lhs',rhs')]))
             else JUDGEMENT(WEAK,(lhs,rhs),
                    [JUDGEMENT(POSS_R(a,p),(POSS(a,p)::lhsFilter,rhsFilter),
                    [ASSUMPTION(p::lhs',rhs')])])
        else ASSUMPTION(lhs,rhs)
    end
  | possRule a p (JUDGEMENT j) = JUDGEMENT j
and idRule (ASSUMPTION(lhs,rhs)) =
    (case List.filter (fn VAR _ => true | _ => false) (intersection lhs rhs) of
      [] => ASSUMPTION(lhs,rhs)
    | (v::_) => if lhs = [v] andalso rhs = [v]
                then JUDGEMENT(ID,(lhs,rhs),[])
                else JUDGEMENT(WEAK,(lhs,rhs),
                        [JUDGEMENT(ID,([v],[v]),[])])
    )
  | idRule (JUDGEMENT j) = JUDGEMENT j
;


(* a decision procedure *)

local
fun foldFun [] a = a
  | foldFun (f::rest) assumption =
    (case f assumption of
      (ASSUMPTION _ ) => foldFun rest assumption
    | (JUDGEMENT j) => JUDGEMENT j
    )
in
fun dp (JUDGEMENT(rule,conclusion,premises)) =
        JUDGEMENT(rule,conclusion,map dp premises)
  | dp (ASSUMPTION (lhs,rhs)) =
    (case foldFun [lFalse,rTrue,idRule,lAnd,rOr,lOr,rAnd]
                (ASSUMPTION (lhs,rhs)) of
      (JUDGEMENT j) => dp (JUDGEMENT j)
    | (ASSUMPTION _) =>
      (* Start of search *)
      let val possGoals = List.concat (
                map (fn (POSS(a,p)) =>
                        [possRule  a p  (ASSUMPTION(lhs,rhs))] | _ => [])
                                lhs)
          val boxGoals = List.concat (
                map (fn (BOX(a,p)) =>
                        [boxRule  a p  (ASSUMPTION (lhs,rhs))] | _ => []) rhs)
       in
         (case List.find (fn tab => isAProof (dp tab)) (boxGoals@possGoals) of
           NONE => ASSUMPTION(lhs,rhs)  (* failure *)
         | SOME(tab) => dp tab
         )
       end
    )
end
;


(*
fun negate (AND props) = OR (map negate props)
  | negate (OR props) = AND (map negate props)
  | negate (BOX(a,p)) = POSS(a,negate p)
  | negate (POSS(a,p)) = BOX(a,negate p)
;

*)

(* state uses dp for counter examples.  It uses Tree to draw
app use ["TreeAct.sig", "TreeAct.sml", "Draw.sig", "Draw.sml"];
*)
app use ["state.sig", "state.sml"];

app use ["cutHml.sig", "cutHml.sml"];

(* another way to "cut eliminate" is to use half the rules
   by using the duality
*)

fun dualProp (AND props) = OR (map dualProp props)
  | dualProp (OR props) = AND (map dualProp props)
  | dualProp (BOX(a,p)) = POSS(a,dualProp p)
  | dualProp (POSS(a,p)) = BOX(a,dualProp p)
  | dualProp (VAR v) = (VAR v)
and dualSeq (lhs,rhs) = (map dualProp rhs, map dualProp lhs)
and dualJudgement (ASSUMPTION seq) = ASSUMPTION (dualSeq seq)
  | dualJudgement (JUDGEMENT (rule,seq,premises)) =
    JUDGEMENT(dualRule rule, dualSeq seq, map dualJudgement premises)
and dualRule LFALSE = RTRUE
  | dualRule RTRUE  = LFALSE
  | dualRule LAND   = ROR
  | dualRule ROR    = LAND
  | dualRule LOR    = RAND
  | dualRule RAND   = LOR
  | dualRule ID     = ID
  | dualRule (BOX_R (a,p)) = POSS_R(a, dualProp p)
  | dualRule (POSS_R (a,p)) = BOX_R(a, dualProp p)
  | dualRule WEAK   = WEAK
  | dualRule (CUT p) = CUT (dualProp p)
;



fun thicken (wL,wR) (ASSUMPTION (lhs,rhs)) =
        ASSUMPTION (lhs@wL,rhs@wR)
  | thicken (wL,wR) (JUDGEMENT(LFALSE,(lhs,rhs),prem)) =
      JUDGEMENT(LFALSE,(lhs@wL,rhs@wR),prem)
  | thicken (wL,wR) (JUDGEMENT(RTRUE,(lhs,rhs),prem)) =
      JUDGEMENT(RTRUE,(lhs@wL,rhs@wR),prem)
  | thicken (wL,wR) (JUDGEMENT(LAND,(lhs,rhs),prem)) =
      JUDGEMENT(LAND,(lhs@wL,rhs@wR),map (thicken (wL,wR)) prem)
  | thicken (wL,wR) (JUDGEMENT(ROR,(lhs,rhs),prem)) =
      JUDGEMENT(ROR,(lhs@wL,rhs@wR),map (thicken (wL,wR)) prem)
  | thicken (wL,wR) (JUDGEMENT(LOR,(lhs,rhs),prem)) =
      JUDGEMENT(LOR,(lhs@wL,rhs@wR),map (thicken (wL,wR)) prem)
  | thicken (wL,wR) (JUDGEMENT(RAND,(lhs,rhs),prem)) =
      JUDGEMENT(RAND,(lhs@wL,rhs@wR),map (thicken (wL,wR)) prem)
  | thicken (wL,wR) (JUDGEMENT(ID,(lhs,rhs),prem)) =
      JUDGEMENT(WEAK, (lhs@wL,rhs@wR),[(JUDGEMENT(ID,(lhs,rhs),prem))])
  | thicken (wL,wR) (JUDGEMENT(BOX_R (a,p),(lhs,rhs),prem)) =
    let val (bx,wL') =  List.partition (fn (BOX(a',_)) => a = a' | _ => false)
                                        wL
        val (ps,wR') =  List.partition (fn (POSS(a',_)) => a = a' | _ => false)
                                        wR
        val bx' = map (fn (BOX(a,p)) => p) bx
        val ps' = map (fn (POSS(a,p)) => p) ps
    in
      if wL' = [] andalso wR' = []
      then JUDGEMENT(BOX_R (a,p),(bx@lhs,ps@rhs), map (thicken (bx',ps')) prem)
      else
        JUDGEMENT(WEAK,(wL@lhs,wR@rhs),
          [JUDGEMENT(BOX_R (a,p),(bx@lhs,ps@rhs),
                map (thicken (bx',ps')) prem)])
    end
  | thicken (wL,wR) (JUDGEMENT(POSS_R (a,p),(lhs,rhs),prem)) =
    let val (bx,wL') =  List.partition (fn (BOX(a',_)) => a = a' | _ => false)
                                wL
        val (ps,wR') =  List.partition (fn (POSS(a',_)) => a = a' | _ => false)
                                wR
        val bx' = map (fn (BOX(a,p)) => p) bx
        val ps' = map (fn (POSS(a,p)) => p) ps
    in
      if wL' = [] andalso wR' = []
      then JUDGEMENT(POSS_R (a,p),(bx@lhs,ps@rhs), map (thicken (bx',ps')) prem)
      else
        JUDGEMENT(WEAK,(wL@lhs,wR@rhs),
          [JUDGEMENT(POSS_R (a,p),(bx@lhs,ps@rhs),
                map (thicken (bx',ps')) prem)])
    end
  | thicken (wL,wR) (JUDGEMENT(WEAK,(lhs,rhs),prem)) =
      let val wL' = wL@(setDiff lhs (antecedent (hd prem)))
          val wR' = wR@(setDiff rhs (succedent (hd prem)))
      in
        thicken (wL',wR') (hd prem)
      end
  | thicken (wL,wR) (JUDGEMENT(CUT x,(lhs,rhs),prem)) =
        JUDGEMENT(CUT x,(lhs,rhs),prem)

;



exception AT;

fun iTH f 0 (a::rest) = (f a)::rest
  | iTH f i (a::rest) = a::(iTH f (i-1) rest)
  | iTH f i [] = raise AT
;

fun at f [] p = f p
  | at f (i::rest) (JUDGEMENT(rule,conclusion,premises)) =
    if i < length premises
    then JUDGEMENT(rule,conclusion, iTH (at f rest) i premises)
    else raise AT
;


(*
fun promote tableau =
fun distribute tableau =

*)

app use ["ppProp.sig", "ppProp.sml"];

app use ["parseHml.sig", "parseHml.sml"];
val P = ParseHml.P;


fun proofOrCe assumption =
    let val proof = dp assumption
    in
      if isAProof proof
      then ppProp.latexProlog ^ ppProp.ppJudgement proof ^ ppProp.latexEpilog
      else StateHml.latexProlog ^
           StateHml.ppStateLatex (StateHml.counter proof) ^
           StateHml.latexEpilog
    end
;


val execute= Unix.streamsOf o Unix.execute;




(* What is going on here?
   I want to execute "xdvi", but if I don't wait for a bit
   sml will execute before the shell file has a chance to
   create the latex file.
   I could "sleep" for a few seconds;
   I could create the latex file separately and then exec the process
   I could wait for all of the xdvi programs to exit.
   The latter seemed easiest.
*)

fun xdvi latex =
    let val proc = Unix.execute ("/bin/csh",["testSh"])
        val (ins,outs) = Unix.streamsOf proc
        val _ = TextIO.output (outs, latex)
        val _ = TextIO.flushOut outs
        val _ = TextIO.closeOut outs
        val _ = OS.Process.atExit (fn () => (Unix.reap proc; ()))
    in
       ()
    end
;

(*      This doesn't work.
fun xdvi judgement =
    let val (ins,outs) = execute ("/bin/nohup",["testSh"])
        val _ = TextIO.output (outs, judgementToLatex judgement)
        val _ = TextIO.flushOut outs
        val _ = TextIO.closeOut outs
    in
       ()
    end
;
*)

fun xdviProofOrCe p = xdvi (proofOrCe p);

infix 3 |- ;
val op |- = (fn (a,b) => ASSUMPTION(a,b));

fun writeOut (fileName,text) =
    let val fdesc = TextIO.openOut fileName
        val _ = TextIO.output (fdesc, text)
        val _ = TextIO.flushOut fdesc
        val _ = TextIO.closeOut fdesc
    in
      ()
    end
;

fun appendOut (fileName,text) =
    let val fdesc = TextIO.openAppend fileName
        val _ = TextIO.output (fdesc, text)
        val _ = TextIO.flushOut fdesc
        val _ = TextIO.closeOut fdesc
    in
      ()
    end
;

(* Allow fragements *)
Compiler.Control.quotation := true;

(* Turn off all compiler output *)
(* Compiler.Control.Print.out := {say=fn _=>(), flush=fn()=>()}; *)

fun main (argc,(fileName::rest)) =
    (use fileName; OS.Process.exit; OS.Process.success)
  | main (argc,_) = (print "No file name\n"; OS.Process.failure)
;

(*
SMLofNJ.exportML "image";

exportFn (filename, f)
      Dump the function f into a heap image called filename.arch-opsys
      (where arch is the machine architecture, such as "sparc" or "x86",
      and opsys is the operating system, such as "solaris" or "win32";)
      and then exit.

      When the heap image is loaded into an SML runtime system,
      (with sml @SMLload=filename.arch-opsys arg1 arg2 ..., )
      computation will start with a call to f(arg0,[arg1,arg2,...])
      where arg0 is the name executable file.

      When f returns, the ML process terminates.

Compiler.Control.Print.printDepth := 0;
SMLofNJ.exportFn ("hmlSml", main);

*)


