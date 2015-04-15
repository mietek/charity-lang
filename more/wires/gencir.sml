(* Build a Gentzen style sequent for Circuits  *)
(* This is the version from August.  It uses the decision procedure
   in the Wires chapter.
*)
(* AND/OR take a set of propositions (not a list);
   Box/Poss take a relation * a proposition
   The constant TRUE = AND [];
   The constant FALSE = OR [];
*)


open PROP;

(*

datatype prop = VAR of act
              | AND of (string list * prop list * string list)
              | OR  of (string list * prop list * string list)
              | L_POSS of (act * prop)
              | L_BOX  of (act * prop)
              | R_POSS of (prop * act)
              | R_BOX  of (prop * act)
              | ABS of ((string * string) list * prop * (string * string) list)
;

*)

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

fun fst (x,y) = x
and snd (x,y) = y
and swap (x,y) = (y,x)
and isMem a [] = false
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


fun isNeg name =
    String.size name > 0 andalso String.substring (name,0,1) = "-"
and getAct name =
    if isNeg name
    then String.substring (name,1,String.size name-1)
    else name
;


(*
fun substitute [] wild = wild
  | substitute substDict (VAR v ) =
      (case List.find (fn (v',_) => v'= v) substDict of
        NONE => VAR v
      | SOME(_,body) => body
      )
  | substitute substDict (AND props) =
      AND(map (substitute substDict) props)
  | substitute substDict (OR props) =
      OR(map (substitute substDict) props)
  | substitute substDict (POSS(function, prop))   =
      POSS(function, substitute substDict prop)
  | substitute substDict (BOX(function, prop))   =
      BOX(function, substitute substDict prop)
  | substitute substDict (MU(var, prop))   =
      MU(var, substitute
        (List.filter (fn (v',_) => not (v' = var)) substDict) prop)
  | substitute substDict (NU(var, prop))   =
      NU(var, substitute
        (List.filter (fn (v',_) => not (v' = var)) substDict) prop)
and substituteActs allActs [] wild = wild
  | substituteActs allActs substDict (VAR v ) = (VAR v)
  | substituteActs allActs substDict (AND props) =
      AND(map (substituteActs allActs substDict) props)
  | substituteActs allActs substDict (OR props) =
      OR(map (substituteActs allActs substDict) props)
  | substituteActs allActs substDict (POSS(act, prop))   =
      (case List.find (fn (a',_) => a'= (getAct act)) substDict of
        NONE => POSS(act,substituteActs allActs substDict prop)
      | SOME(_,newActs) =>
          if isNeg act
          then OR (map (fn a => (POSS(a,prop))) (setDiff allActs newActs))
          else OR (map (fn a => (POSS(a,prop))) newActs)
      )
  | substituteActs allActs substDict (BOX(act, prop))   =
      (case List.find (fn (a',_) => a'= (getAct act)) substDict of
        NONE => POSS(act,substituteActs allActs substDict prop)
      | SOME(_,newActs) =>
          if isNeg act
          then AND (map (fn a => (POSS(a,prop))) (setDiff allActs newActs))
          else AND (map (fn a => (POSS(a,prop))) newActs)
      )
  | substituteActs allActs substDict (MU(var, prop))   =
      MU(var, substituteActs allActs substDict prop)
  | substituteActs allActs substDict (NU(var, prop))   =
      NU(var, substituteActs allActs substDict prop)
;
*)


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

        | LPOSS | RBOX | LSSOP | RXOB
        | LBOX  | RPOSS | LXOB | RSSOP
        | WEAKP | WEAKG | CUT of 'a
        | GUARDL | GUARDR


(*
        | BOX_R of act * 'a     | POSS_R of act * 'a    | WEAK | CUT of 'a
        | MUFOLD of (string * 'a)
        | NUFOLD of (string * 'a)
        | MUCONS   of string    | NUCONS   of string
        | MUINDUCT of string    | NUINDUCT of string
        | CYCLE of string
*)
;

exception ILL_FORMED_PROOF;

datatype  'a TABLEAU =
            JUDGEMENT of ( prop RULE * 'a * 'a TABLEAU list)
          | ASSUMPTION of 'a
;

fun conclusion (ASSUMPTION(a)) = a
  | conclusion (JUDGEMENT (_,c,_)) = c
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
fun tabFold (a,j) (ASSUMPTION s) = a s
  | tabFold (a,j) (JUDGEMENT(rule,s,children)) =
                j (rule,s,map (tabFold (a,j)) children)
;

fun isAProof (ASSUMPTION _) = false
  | isAProof (JUDGEMENT(_,_,parents)) = List.all isAProof parents
;


(* Rules are:

   ID

   L-False, R-True              The only rules which are terminals

   L-And,  R-Or,                Essentially just adding to the set

   L-Or,   R-And                Distribution

   L-Poss, R-Box,               Release
   L-Box,  R-Poss,              Bind
   L-ssoP, R-xoB,               Release
   L-xoB,  R-ssoP,              Bind

   Weak-P                       Weaken a Proposition    either side
   Weak-G                       Weaken a Guard          either side
   ForceGuard                   Unwrap a guard          either side




   MUFOLD, NUFOLD

   MUCONS, NUCONS

   MUINDUCT, NUINDUCT

        Weak

There is one weaking rule:

            G  |- D
        ----------------
        G U G' |- D U D'

*)


(* Fresh Variables: To get a fresh variable, find one which hasn't
   been used a proposition, sequent, history
   NewVar "x" will return "x1"; newVar "x15" will return "x16"
   freshVar x vars will keep calling newVar until there is
   a "newVar x" which isn't a member of vars
*)
fun inputVars p = (fst o getTypes) p
and outputVars p = (snd o getTypes) p
and actOuts (_,_,outs) = outs
and actIns (ins,_,_) = ins
(* usedInputVarsSeq returns not only the "input" vars, but also the
   outputs of the guards.  *)
and usedInputVarsSeq (i,gL,ante,suc,gR,ou) =
        (List.concat (map inputVars ante))@(List.concat (map inputVars suc))
        @(List.concat (map (actOuts o fst)  gL))@
        (List.concat (map (actIns o fst)  gL))@i
and usedOutputVarsSeq (i,gL,ante,suc,gR,ou) =
        (List.concat (map outputVars ante))@(List.concat (map outputVars suc))
        @(List.concat (map (actIns o snd) gR))
        @(List.concat (map (actOuts o snd) gR))@ou
and newVar var =
    let val (name,suffix) =  Substring.splitr Char.isDigit (Substring.all var)
        val newSuffix = Int.toString (
                (getOpt (Int.fromString (Substring.string suffix),0)) +1)
    in
      (Substring.string name)^newSuffix
    end
and freshVar x vars =
    if isMem x vars
    then freshVar (newVar x) vars
    else x
and freshVars [] usedVars = []
  | freshVars (v::newVars) usedVars =
    let val v' = freshVar v usedVars
    in
      (v',v)::(freshVars newVars (v'::usedVars))
    end
and uniqueLeftVars ((ins,act,outs),prop) sequent =
    let val reWirings = freshVars outs (usedInputVarsSeq sequent)
    in
      ((ins,act,map (wireSubst reWirings) outs),
       reWireL reWirings prop)
    end
and uniqueRightVars (prop,(ins,act,outs)) sequent =
    let val reWirings = map swap (freshVars ins (usedOutputVarsSeq sequent))
    in
      (reWireRight reWirings prop,
      (map (wireSubst reWirings) ins,act,outs))
    end
;



(* MAKE UNIQUE VARIABLES  *)

(* I need a way to calculate "unique identifiers"
   reset will set everything back to zero.
   This is useful for testing so that on subsequent runs,
   things will look equivalent.
*)

val unique_id = ref 0;

fun reset () = unique_id :=0;


fun inc x = x := !x +1
fun mkNewVar varName =
      let val x =  inc unique_id
          in
            varName ^ (Int.toString (!unique_id))
          end
;

fun lAnd (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    let val ante' = List.concat
                        (map (fn (AND (_,props,_)) => props | p => [p]) ante)
    in
      if ante = ante'
      then (ASSUMPTION (i,gL,ante,suc,gR,ou))
      else (JUDGEMENT(LAND,(i,gL,ante,suc,gR,ou),
        [ASSUMPTION(i,gL,ante',suc,gR,ou)]))
    end
  | lAnd (JUDGEMENT j) = JUDGEMENT j
and rOr (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    let val suc' = List.concat
                        (map (fn (OR (_,props,_)) => props | p => [p]) suc)
    in
      if suc = suc'
      then (ASSUMPTION (i,gL,ante,suc,gR,ou))
      else (JUDGEMENT(ROR,(i,gL,ante,suc,gR,ou),
        [ASSUMPTION (i,gL,ante,suc',gR,ou)]))
    end
  | rOr (JUDGEMENT j) = JUDGEMENT j
and lFalse (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
        if List.exists (fn (OR(_,[],_)) => true | _ => false) ante
        then (JUDGEMENT (LFALSE,(i,gL,ante,suc,gR,ou),[]))
        else (ASSUMPTION (i,gL,ante,suc,gR,ou))
  | lFalse (JUDGEMENT j) = JUDGEMENT j
and rTrue (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
        if List.exists (fn (AND(_,[],_)) => true | _ => false) suc
        then (JUDGEMENT (RTRUE,(i,gL,ante,suc,gR,ou),[]))
        else (ASSUMPTION (i,gL,ante,suc,gR,ou))
  | rTrue (JUDGEMENT j) = JUDGEMENT j
and lOr (ASSUMPTION (i,gL,ante,suc,gR,ou)) =
    (case List.find (fn (OR _) => true | _ => false) suc of
      NONE => ASSUMPTION (i,gL,ante,suc,gR,ou)
    | SOME (OR (l,props,r)) =>
        (JUDGEMENT(LOR,(i,gL,ante,suc,gR,ou),
            map (fn p =>
            (ASSUMPTION(i,gL,p::(rm (OR (l,props,r)) ante),suc,gR,ou))) props))
    )
  | lOr (JUDGEMENT j) = JUDGEMENT j
and rAnd (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.find (fn (AND _) => true | _ => false) suc of
      NONE => ASSUMPTION (i,gL,ante,suc,gR,ou)
    | SOME (AND (l,props,r)) =>
        (JUDGEMENT(RAND,(i,gL,ante,suc,gR,ou),
        map (fn p => ASSUMPTION(i,gL,ante,p::(rm (AND (l,props,r)) suc),gR,ou)
            ) props))
    )
  | rAnd (JUDGEMENT j) = JUDGEMENT j


(* For LeftPoss (release):
   find a left poss;
   find all free variables used in antecedent, succedent
   change the bound variables to fresh variables.
   put the guard (with fresh variables) in the guard section
   and the proposition in antecedent.  Delete the original LeftPoss
The right Box, Left ssoP, Right xoB are dual.
*)


and lPoss (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.find (fn (L_POSS _) => true | _ => false) ante of
      NONE => (ASSUMPTION(i,gL,ante,suc,gR,ou))
    | SOME (L_POSS ((ins,a,outs),p)) =>
        let val (act',p') = uniqueLeftVars ((ins,a,outs),p)
                                                    (i,gL,ante,suc,gR,ou)
        in
          JUDGEMENT(LPOSS, (i,gL,ante,suc,gR,ou),
          [ASSUMPTION(i,(act',[])::gL, p'::(rm (L_POSS((ins,a,outs),p)) ante),
                      suc,gR,ou)])
        end
    )
and rBox (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.find (fn (L_BOX _) => true | _ => false) suc of
      NONE => (ASSUMPTION(i,gL,ante,suc,gR,ou))
    | SOME (L_BOX ((ins,a,outs),p)) =>
        let val (act',p') = uniqueLeftVars ((ins,a,outs),p)
                                                    (i,gL,ante,suc,gR,ou)
        in
          JUDGEMENT(RBOX, (i,gL,ante,suc,gR,ou),
          [ASSUMPTION(i,(act',[])::gL, ante,
                      p'::(rm (L_BOX((ins,a,outs),p)) suc),gR,ou)])
        end
    )
and lSsop (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.find (fn (R_POSS _) => true | _ => false) ante of
      NONE => (ASSUMPTION(i,gL,ante,suc,gR,ou))
    | SOME (R_POSS (p,(ins,a,outs))) =>
        let val (p',act') = uniqueRightVars (p,(ins,a,outs))
                                                    (i,gL,ante,suc,gR,ou)
        in
          JUDGEMENT(LSSOP, (i,gL,ante,suc,gR,ou),
          [ASSUMPTION(i,gL, p'::(rm (R_POSS(p,(ins,a,outs))) ante),
                      suc,([],act')::gR,ou)])
        end
    )
and rXob (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.find (fn (R_BOX _) => true | _ => false) suc of
      NONE => (ASSUMPTION(i,gL,ante,suc,gR,ou))
    | SOME (R_BOX (p,(ins,a,outs))) =>
        let val (p',act') = uniqueRightVars (p,(ins,a,outs))
                                                    (i,gL,ante,suc,gR,ou)
        in
          JUDGEMENT(RXOB, (i,gL,ante,suc,gR,ou),
          [ASSUMPTION(i,gL, ante,
                      p'::(rm (R_BOX(p,(ins,a,outs))) suc),([],act')::gR,ou)])
        end
    )
and lBox (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    let val boxes = List.filter (fn (L_BOX _) => true | _ => false) ante
        val guardBoxes = map (fn ((x,A,y),props) => (((x,A,y),props),
                List.filter (fn (L_BOX((x',A',y'),p)) =>
                        x = x' andalso A = A' andalso not
                            (isMem (L_BOX((x',A',y'),p)) props)) boxes)) gL
        val releasedBoxes = map (fn (((x,A,y),props),boxes) =>
                (((x,A,y),boxes@props),map (fn (L_BOX((x',A',y'),p)) =>
                    reWireL (ListPair.zip (y,y')) p) boxes)) guardBoxes
        val (guards',releasedBoxes') = ListPair.unzip releasedBoxes
        val releasedBoxes'' = List.concat releasedBoxes'
    in
      if releasedBoxes'' = []
      then (ASSUMPTION(i,gL,ante,suc,gR,ou))
      else JUDGEMENT(LBOX, (i,gL,ante,suc,gR,ou),
                [ASSUMPTION(i,guards',releasedBoxes''@ante,suc,gR,ou)])
    end
and rPoss (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    let val posses = List.filter (fn (L_POSS _) => true | _ => false) suc
        val guardPosses = map (fn ((x,A,y),props) => (((x,A,y),props),
                List.filter (fn (L_POSS((x',A',y'),p)) =>
                        x = x' andalso A = A' andalso not
                            (isMem (L_POSS((x',A',y'),p)) props)) posses)) gL
        val releasedPosses = map (fn (((x,A,y),props),posses) =>
                (((x,A,y),posses@props),map (fn (L_POSS((x',A',y'),p)) =>
                    reWireL (ListPair.zip (y,y')) p) posses)) guardPosses
        val (guards',releasedPosses') = ListPair.unzip releasedPosses
        val releasedPosses'' = List.concat releasedPosses'
    in
      if releasedPosses'' = []
      then (ASSUMPTION(i,gL,ante,suc,gR,ou))
      else JUDGEMENT(RPOSS, (i,gL,ante,suc,gR,ou),
                [ASSUMPTION(i,guards',ante,releasedPosses''@suc,gR,ou)])
    end

and lXob (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    let val boxes = List.filter (fn (R_BOX _) => true | _ => false) ante
        val guardBoxes = map (fn (props,(x,A,y)) => ((props,(x,A,y)),
                List.filter (fn (R_BOX(p,(x',A',y'))) =>
                        y = y' andalso A = A' andalso not
                            (isMem (R_BOX(p,(x',A',y'))) props)) boxes)) gR
        val releasedBoxes = map (fn ((props,(x,A,y)),boxes) =>
                ((boxes@props,(x,A,y)),map (fn (R_BOX(p,(x',A',y'))) =>
                    reWireRight (ListPair.zip (x,x')) p) boxes)) guardBoxes
        val (guards',releasedBoxes') = ListPair.unzip releasedBoxes
        val releasedBoxes'' = List.concat releasedBoxes'
    in
      if releasedBoxes'' = []
      then (ASSUMPTION(i,gL,ante,suc,gR,ou))
      else JUDGEMENT(LXOB, (i,gL,ante,suc,gR,ou),
                [ASSUMPTION(i,gL,releasedBoxes''@ante,suc,guards',ou)])
    end
and rSsop (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    let val posses = List.filter (fn (R_POSS _) => true | _ => false) ante
        val guardPosses = map (fn (props,(x,A,y)) => ((props,(x,A,y)),
                List.filter (fn (R_POSS(p,(x',A',y'))) =>
                        y = y' andalso A = A' andalso not
                            (isMem (R_POSS(p,(x',A',y'))) props)) posses)) gR
        val releasedPosses = map (fn ((props,(x,A,y)),posses) =>
                ((posses@props,(x,A,y)),map (fn (R_POSS(p,(x',A',y'))) =>
                    reWireRight (ListPair.zip (x,x')) p) posses)) guardPosses
        val (guards',releasedPosses') = ListPair.unzip releasedPosses
        val releasedPosses'' = List.concat releasedPosses'
    in
      if releasedPosses'' = []
      then (ASSUMPTION(i,gL,ante,suc,gR,ou))
      else JUDGEMENT(RSSOP, (i,gL,ante,suc,gR,ou),
                [ASSUMPTION(i,gL,ante,releasedPosses''@suc,guards',ou)])
    end
and idRule (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.filter (fn VAR _ => true | _ => false) (intersection ante suc) of
      [] => ASSUMPTION(i,gL,ante,suc,gR,ou)
    | (v::_) =>
        let val idProof =
                if ante = [v] andalso suc = [v]
                then ASSUMPTION(i,gL,[v],[v],gR,ou)
                else JUDGEMENT(WEAKP,(i,gL,ante,suc,gR,ou),
                        [ASSUMPTION(i,gL,[v],[v],gR,ou)])
            val idProof' = proofFold guardWeak idProof
            val idProof'' = proofFold leftGuard idProof'
            val idProof''' = proofFold rightGuard idProof''
        in
          proofFold idRule' idProof'''
        end
    )
  | idRule (JUDGEMENT j) = JUDGEMENT j
and idRule' (ASSUMPTION(i,[],[v1],[v2],[],ou)) =
        if v1 = v2
        then JUDGEMENT(ID,(i,[],[v1],[v2],[],ou),[])
        else (ASSUMPTION(i,[],[v1],[v2],[],ou))
  | idRule' j = j
and guardWeak (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    let val usedInTypes = List.concat ((map inputVars ante)@
            (map (actIns o fst) gL)@(map inputVars suc))
        val usedOutTypes = List.concat ((map outputVars ante)@
                (map (actOuts o snd) gR)@(map outputVars suc))
        val usedLeftGuards = List.filter (fn ((_,_,outs),_) =>
                        not (intersection outs usedInTypes = [])) gL
        val usedRightGuards = List.filter (fn (_,(ins,_,_)) =>
                        not (intersection ins usedOutTypes = [])) gR
    in
      if usedLeftGuards = gL andalso usedRightGuards = gR
      then (ASSUMPTION(i,gL,ante,suc,gR,ou))
      else JUDGEMENT(WEAKG,(i,gL,ante,suc,gR,ou), [guardWeak
            (ASSUMPTION(i,usedLeftGuards,ante,suc,usedRightGuards,ou))])
    end
  | guardWeak  (JUDGEMENT j) = JUDGEMENT j
and leftGuard (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    let val usedInPropTypes = List.concat ((map inputVars ante)@
                                                (map inputVars suc))
        val guards = map fst gL
    in
      (case List.find (fn (x,A,y) => subset x i andalso
        intersection x usedInPropTypes = [] andalso
          intersection (List.concat (map actIns (rm (x,A,y) guards))) x = [])
                                guards of
         NONE => (ASSUMPTION(i,gL,ante,suc,gR,ou))
       | SOME(x,A,y) =>
         let val i' = y@(setDiff i x)
             val gL' = List.filter (fn (act,_) => not (act = (x,A,y))) gL
         in
           JUDGEMENT(GUARDL,(i,gL,ante,suc,gR,ou),
                [leftGuard (ASSUMPTION(i',gL',ante,suc,gR,ou))])
         end
      )
    end
  | leftGuard (JUDGEMENT j) = JUDGEMENT j
and rightGuard (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    let val usedOutPropTypes = List.concat ((map outputVars ante)@
                                                (map outputVars suc))
        val guards = map snd gR
    in
      (case List.find (fn (x,A,y) => subset y ou andalso
        intersection y usedOutPropTypes = [] andalso
          intersection (List.concat (map actOuts (rm (x,A,y) guards))) y = [])
                                        guards of
         NONE => (ASSUMPTION(i,gL,ante,suc,gR,ou))
       | SOME(x,A,y) =>
         let val ou' = x@(setDiff i y)
             val gR' = List.filter (fn (_,act) => not (act = (x,A,y))) gR
         in
           JUDGEMENT(GUARDR,(i,gL,ante,suc,gR,ou),
                [rightGuard (ASSUMPTION(i,gL,ante,suc,gR',ou'))])
         end
      )
    end
  | rightGuard (JUDGEMENT j) = JUDGEMENT j

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
        JUDGEMENT(rule,conclusion, map dp  premises)
  | dp (ASSUMPTION seq) =
    (case foldFun [lFalse,rTrue,idRule,lAnd,rOr,lPoss,rBox,
                   lSsop,rXob,lBox,rPoss,lXob,rSsop,lOr,rAnd]
                        (ASSUMPTION seq) of
      (JUDGEMENT j) => dp (JUDGEMENT j)
    | (ASSUMPTION seq) => (ASSUMPTION seq) (* failure *)
    )
end
;

local
fun foldFun [] a = a
  | foldFun (f::rest) assumption =
    (case f assumption of
      (ASSUMPTION _ ) => foldFun rest assumption
    | (JUDGEMENT j) => JUDGEMENT j
    )
in
fun dpOnce (JUDGEMENT(rule,conclusion,premises)) =
        JUDGEMENT(rule,conclusion, map dp  premises)
  | dpOnce (ASSUMPTION seq) =
    (case foldFun [lFalse,rTrue,idRule,lAnd,rOr,lPoss,rBox,
                   lSsop,rXob,lBox,rPoss,lXob,rSsop,lOr,rAnd]
                        (ASSUMPTION seq) of
      (JUDGEMENT j) => (JUDGEMENT j)
    | (ASSUMPTION seq) => (ASSUMPTION seq) (* failure *)
    )
end
;


(* Some Tests:

fun comp text =
    (print (ppPropText (COMPILE text)))
        handle COMPILE_ERROR (msg) => print msg
;

val ante = COMPILE "<{x}A{y}>{y}P{}";
val suc = COMPILE "<{x}A{y}>{y}P{}";

val seq1 = ASSUMPTION(["x"],[],[ante],[suc],[],[]);

val (JUDGEMENT(rule,conc,[p1])) = dp seq1;

val (JUDGEMENT(rule,conc,[p1])) = dpOnce seq1;
val (JUDGEMENT(rule,conc,[p2])) = dpOnce p1;

dp seq1;

Test for unique variables:

val ante = COMPILE "&(<{x}A{y}>{y}P{},(<{x}A{y}>{y}Q{}))";
val suc = COMPILE "<{x}A{y}>{y}P{}";

val seq1 = ASSUMPTION(["x"],[],[ante],[suc],[],[]);
show "Poss AND Poss" (dp seq1);


val ante2 = COMPILE "<{u}B{v}>[{x}A{y}]{y,v}P{}";
val suc2 = COMPILE "[{x}A{y}]<{u}B{v}>{y,v}P{}";

val seq2 = ASSUMPTION(["x","u"],[],[ante2],[suc2],[],[]);
dp seq2;

val ante3 = COMPILE "<{u}B{v}><{x}A{y}>{y,v}P{}";
val suc3 = COMPILE "<{x}A{y}><{u}B{v}>{y,v}P{}";

val seq3 = ASSUMPTION(["x","u"],[],[ante3],[suc3],[],[]);
dp seq3;


variation of seq2; this time use different bound variables.
val ante4 = COMPILE "<{u}B{v}>[{x}A{y}]{y,v}P{}";
val suc4 = COMPILE "[{x}A{z}]<{u}B{w}>{z,w}P{}";

val seq4 = ASSUMPTION(["x","u"],[],[ante4],[suc4],[],[]);
dp seq4;


*)



app use ["ppProp.sig", "ppProp.sml"];



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
  | at f _ (ASSUMPTION _) = raise AT
;

fun proofOrCe assumption =
    let val proof = dp assumption
    in
      if isAProof proof
      then ppProp.latexProlog ^ ppProp.ppJudgement proof ^ ppProp.latexEpilog
      else ppProp.latexProlog ^
           "Is Not a Proof" ^
           ppProp.latexEpilog
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
and xdviCaption caption latex =
    let val proc = Unix.execute ("/bin/csh",["testSh",
            String.translate
                (fn x => if Char.isAlphaNum x
                then Char.toString x else "") caption])
        val (ins,outs) = Unix.streamsOf proc
        val _ = TextIO.output (outs, latex)
        val _ = TextIO.flushOut outs
        val _ = TextIO.closeOut outs
        val _ = OS.Process.atExit (fn () => (Unix.reap proc; ()))
    in
       ()
    end
;


fun show caption proof =
        xdviCaption caption (ppProp.latexProlog ^
              (ppProp.ppJudgement proof) ^
              (ppProp.latexCaption caption)^
              ppProp.latexEpilog)
;

(*

show "Poss " (dp seq1);

show "Poss Poss" (dp seq2);

show "Poss Box" (dp seq4);


*)


(*
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

(* fun xdviProofOrCe p = xdvi (proofOrCe p); *)

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

fun showFile fileName caption proof =
    writeOut (fileName,
              ppProp.latexProlog ^
              (ppProp.ppJudgement proof) ^
              (ppProp.latexCaption caption)^
              ppProp.latexEpilog)
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
SMLofNJ.exportFn ("wires", main);

*)

*)
