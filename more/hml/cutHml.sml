structure CutHml :> CutHml =
  struct

(* Little functions to push cut upLeft/upRight *)
fun upLOpt (JUDGEMENT(CUT x, (lhs,rhs),
		[JUDGEMENT(LFALSE,(lhs',rhs'),prem), premR])) =
	  SOME(JUDGEMENT(LFALSE,(lhs,rhs),[]))
  | upLOpt (JUDGEMENT(CUT x,(lhs,rhs),
		[JUDGEMENT(RTRUE,(lhs',rhs'),prem), premR])) =
	  SOME(JUDGEMENT(RTRUE,(lhs,rhs),[]))
  | upLOpt (JUDGEMENT(CUT x,(lhs,rhs),
		[JUDGEMENT(LAND,(lhs',rhs'),premL),premR])) =
	SOME(JUDGEMENT(LAND,(lhs,rhs),
	  	[JUDGEMENT(CUT(x),(lhs',rhs'),(premL@[premR]))]))
  | upLOpt (JUDGEMENT(CUT x,(lhs,rhs),
		[JUDGEMENT(ID,(lhs',rhs'),premL), premR])) = SOME(premR)
  | upLOpt (JUDGEMENT(CUT x,(lhs,rhs),
		[JUDGEMENT(WEAK,(lhs',rhs'),premL),premR])) = 
	  if isMem x (succedent (hd premL))
	  then 
	  let val (lhsL,rhsL) = (antecedent (hd premL),succedent (hd premL))
	      val (lhsR,rhsR) = (antecedent premR,succedent premR)
	      val lhs' = lhsL@(rmOnce x lhsR)
	      val rhs' = (rmOnce x rhsL)@rhsR
	  in
	    SOME(JUDGEMENT(WEAK,(lhs,rhs),[JUDGEMENT(CUT x,(lhs',rhs'),
		premL@[premR])]))
	  end
	  else SOME(JUDGEMENT(WEAK,(lhs,rhs),premL))
  | upLOpt (JUDGEMENT(CUT x,(lhs,rhs),
		[JUDGEMENT(LOR,(lhs',rhs'),premL), premR])) = 
	let fun cutEmUp prem =
	    let val (lhsR,rhsR) = (antecedent premR,succedent premR)
	        val (lhsL,rhsL) = (antecedent prem, succedent prem)
	        val lhs' = lhsL@(rmOnce x lhsR)
	        val rhs' = (rmOnce x rhsL)@rhsR
	    in
	      JUDGEMENT(CUT(x),(lhs',rhs'), [prem,premR])
	    end

	  in
	    SOME(JUDGEMENT(RAND,(lhs,rhs), map cutEmUp premL))
	  end
  | upLOpt (JUDGEMENT(CUT x,(lhs,rhs),
		[JUDGEMENT(ROR,(lhs',rhs'),premL), premR])) = 
	if isMem x (succedent (hd premL))
	then SOME(JUDGEMENT(ROR,(lhs,rhs),
	      [JUDGEMENT(CUT(x), (lhs@(rmOnce x lhs'),(rmOnce x rhs)@rhs'),
		premL@[premR])]))
	else NONE

  | upLOpt (JUDGEMENT(CUT x,(lhs,rhs),
		[JUDGEMENT(RAND,(lhs',rhs'),premL), premR])) = 
	let fun cutEmUp prem =
	    let val (lhsR,rhsR) = (antecedent premR,succedent premR)
	        val (lhsL,rhsL) = (antecedent prem, succedent prem)
	        val lhs' = lhsL@(rmOnce x lhsR)
	        val rhs' = (rmOnce x rhsL)@rhsR
	    in
	      JUDGEMENT(CUT(x),(lhs',rhs'), [prem,premR])
	    end
	  in
	    if List.all (fn j => isMem x (succedent j)) premL
	    then SOME(JUDGEMENT(RAND,(lhs,rhs), map cutEmUp premL))
	    else NONE
	  end
  | upLOpt (JUDGEMENT(CUT x,(lhs,rhs),
		[JUDGEMENT(BOX_R _,_,_),premR])) = NONE
  | upLOpt (JUDGEMENT(CUT x,(lhs,rhs),
		[JUDGEMENT(POSS_R _,_,_), premR])) = NONE
  | upLOpt (JUDGEMENT(CUT x,(lhs,rhs),
		[JUDGEMENT(CUT _,(lhs',rhs'),premL), premR])) = NONE
  | upLOpt (JUDGEMENT(CUT x,(lhs,rhs),[ASSUMPTION(lhs',rhs'),premR])) = NONE
  | upLOpt _  = NONE
and upL j = 
    (case upLOpt j of
      SOME j' => j'
    | NONE    => j
    )
;

fun upROpt (JUDGEMENT(CUT x,(lhs,rhs),
		[premL,JUDGEMENT(RTRUE,(lhs',rhs'),premR)])) =
	  SOME(JUDGEMENT(RTRUE,(lhs,rhs),[]))
  | upROpt (JUDGEMENT(CUT x,(lhs,rhs),
		[premL,JUDGEMENT(LFALSE,(lhs',rhs'),premR)])) =
	  SOME(JUDGEMENT(LFALSE,(lhs,rhs),[]))
  | upROpt (JUDGEMENT(CUT x,(lhs,rhs),
		[premL,JUDGEMENT(ROR,(lhs',rhs'),premR)])) =
	SOME(JUDGEMENT(ROR,(lhs,rhs),
		[JUDGEMENT(CUT(x),(lhs',rhs'),([premL]@premR))]))
  | upROpt (JUDGEMENT(CUT x,(lhs,rhs),
		[premL,JUDGEMENT(ID,(lhs',rhs'),premR)])) = SOME premL
  | upROpt (JUDGEMENT(CUT x,(lhs,rhs),
		[premL,JUDGEMENT(WEAK,(lhs',rhs'),premR)])) = 
	  if isMem x (antecedent (hd premR))
	  then 
	  let val (lhsL,rhsL) = (antecedent premL,succedent premL)
	      val (lhsR,rhsR) = (antecedent (hd premR), succedent (hd premR))
	      val lhs' = lhsL@(rmOnce x lhsR)
	      val rhs' = (rmOnce x rhsL)@rhsR
	  in
	    SOME(JUDGEMENT(WEAK,(lhs,rhs),[JUDGEMENT(CUT x,(lhs',rhs'),
		[premL]@premR)]))
	  end
	  else SOME(JUDGEMENT(WEAK,(lhs,rhs),premR))

  | upROpt (JUDGEMENT(CUT x,(lhs,rhs), 
		[premL,JUDGEMENT(RAND,(lhs',rhs'),premR)])) = 
	let fun cutEmUp prem =
	    let val (lhsL,rhsL) = (antecedent premL,succedent premL)
	        val (lhsR,rhsR) = (antecedent prem, succedent prem)
	        val lhs' = lhsL@(rmOnce x lhsR)
	        val rhs' = (rmOnce x rhsL)@rhsR
	    in
	      JUDGEMENT(CUT(x),(lhs',rhs'), [premL,prem])
	    end

	  in
	    SOME(JUDGEMENT(RAND,(lhs,rhs), map cutEmUp premR))
	  end

  | upROpt (JUDGEMENT(CUT x,(lhs,rhs),
		[premL,JUDGEMENT(LAND,(lhs',rhs'),premR)])) = 
	if isMem x (antecedent (hd premR))
	then SOME(JUDGEMENT(LAND,(lhs,rhs),
	      [JUDGEMENT(CUT(x), (lhs@(rmOnce x lhs'),(rmOnce x rhs)@rhs'),
		[premL]@premR)]))
	else NONE

  | upROpt (JUDGEMENT(CUT x,(lhs,rhs), 
		[premL,JUDGEMENT(LOR,(lhs',rhs'),premR)])) =
	let fun cutEmUp prem =
	    let val (lhsL,rhsL) = (antecedent premL,succedent premL)
	        val (lhsR,rhsR) = (antecedent prem, succedent prem)
	        val lhs' = lhsL@(rmOnce x lhsR)
	        val rhs' = (rmOnce x rhsL)@rhsR
	    in
	      JUDGEMENT(CUT x,(lhs',rhs'), [premL,prem])
	    end

	  in
	    if List.all (fn j => isMem x (antecedent j)) premR
	    then SOME(JUDGEMENT(RAND,(lhs,rhs), map cutEmUp premR))
	    else NONE
	  end

  | upROpt (JUDGEMENT(CUT x,(lhs,rhs),
		[premL,JUDGEMENT(BOX_R _,_,_)])) = NONE
  | upROpt (JUDGEMENT(CUT x,(lhs,rhs),
		[premL,JUDGEMENT(POSS_R _,_,_)])) = NONE
  | upROpt (JUDGEMENT(CUT x,(lhs,rhs),
		[premL,JUDGEMENT(CUT _,(lhs',rhs'),premR)])) = NONE

  | upROpt (JUDGEMENT(CUT x,(lhs,rhs),
		[premL,ASSUMPTION a])) = NONE
(* catch all; should never occur in a well formed proof.
   An example might be an ANDL with more than one premise.
*)
  | upROpt _  = NONE 
and upR j = 
    (case upROpt j of
      SOME j' => j'
    | NONE    => j
    )
;

fun upBothOpt (JUDGEMENT(CUT (AND xs), (lhs,rhs),
		[JUDGEMENT(ANDR,(lhsL,rhsL),premL),
            	 JUDGEMENT(ANDL,(lhsR,rhsR),premR)])) =
    let fun foldCut (JUDGEMENT(ANDR,(lhs',rhs'),premL),premR) =
	let val cutX = hd (bagDiff rhs'  rhsL) 
	    val conL = (setDiff lhs' (antecedent premR))@(antecedent premR)
	    val conR = (setDiff rhs' (succedent premR))@(succedent premR)
	in
	  JUDGEMENT(CUT cutX, (rmOnce cutX conL,rmOnce cutX conR),
	  [JUDGEMENT(ANDR,(lhs',rhs'),premL),premR])
	end
    in
    if List.all (fn j => isMem (AND xs) (succedent j)) premL andalso
       List.all (fn j => isMem (AND xs) (antecedent j)) premR
    then NONE
    else SOME (foldr foldCut (JUDGEMENT(ANDL,(lhsR,rhsR),premR)) premL)

    end

  | upBothOpt (JUDGEMENT(CUT (OR xs),(lhs,rhs),
		[JUDGEMENT(ORR,(lhsL,rhsL),premL),
            	 JUDGEMENT(ORL,(lhsR,rhsR),premR)])) =
    let fun foldCut (JUDGEMENT(ORL,(lhs',rhs'),premL),premR) =
	let val cutX = hd (bagDiff lhs'  lhsR) 
	    val conL = (setDiff lhs' (antecedent premR))@(antecedent premR)
	    val conR = (setDiff rhs' (succedent premR))@(succedent premR)
	in
	  JUDGEMENT(CUT cutX, (rmOnce cutX conL,rmOnce cutX conR),
	  [JUDGEMENT(ORL,(lhs',rhs'),premL),premR])
	end
    in
    if List.all (fn j => isMem (AND xs) (succedent j)) premL andalso
       List.all (fn j => isMem (AND xs) (antecedent j)) premR
    then NONE
    else SOME (foldr foldCut (JUDGEMENT(ORR,(lhsL,rhsL),premL)) premR)

    end

  | upBothOpt (JUDGEMENT(CUT (BOX(a,p)), (lhs,rhs),
		[JUDGEMENT(BOX_R (aL,pL), (lhsL,rhsL),premL),
		 JUDGEMENT(BOX_R (aR,pR), (lhsR,rhsR),premR)])) =

	if a = aL andalso a = aR andalso p = pL
	then SOME (JUDGEMENT(BOX_R (aR,pR), 
		(lhsL@(rmOnce (BOX(a,p)) lhsR),(rmOnce (BOX(a,p)) rhsL)@rhsR),
		[JUDGEMENT(CUT p, 
		((antecedent (hd premL))@(rmOnce p (antecedent (hd premR))),
		 (rmOnce p (succedent (hd premL)))@(succedent (hd premR))),
		premL@premR)]))
	else NONE
  | upBothOpt (JUDGEMENT(CUT (BOX(a,p)), (lhs,rhs),
		[JUDGEMENT(BOX_R (aL,pL), (lhsL,rhsL),premL),
		 JUDGEMENT(POSS_R (aR,pR), (lhsR,rhsR),premR)])) =

	if a = aL andalso a = aR andalso p = pL
	then SOME (JUDGEMENT(POSS_R (aR,pR), 
		(lhsL@(rmOnce (BOX(a,p)) lhsR),(rmOnce (BOX(a,p)) rhsL)@rhsR),
		[JUDGEMENT(CUT p, 
		((antecedent (hd premL))@(rmOnce p (antecedent (hd premR))),
		 (rmOnce p (succedent (hd premL)))@(succedent (hd premR))),
		premL@premR)]))
	else NONE
  | upBothOpt (JUDGEMENT(CUT (POSS(a,p)), (lhs,rhs),
		[JUDGEMENT(BOX_R (aL,pL), (lhsL,rhsL),premL),
		 JUDGEMENT(POSS_R (aR,pR), (lhsR,rhsR),premR)])) =

	if a = aL andalso a = aR andalso p = pR
	then SOME (JUDGEMENT(BOX_R (aL,pL), 
		(lhsL@(rmOnce (POSS(a,p)) lhsR),(rmOnce (POSS(a,p)) rhsL)@rhsR),
		[JUDGEMENT(CUT p, 
		((antecedent (hd premL))@(rmOnce p (antecedent (hd premR))),
		 (rmOnce p (succedent (hd premL)))@(succedent (hd premR))),
		premL@premR)]))
	else NONE
  | upBothOpt (JUDGEMENT(CUT (POSS(a,p)), (lhs,rhs),
		[JUDGEMENT(POSS_R (aL,pL), (lhsL,rhsL),premL),
		 JUDGEMENT(POSS_R (aR,pR), (lhsR,rhsR),premR)])) =

	if a = aL andalso a = aR andalso p = pR
	then SOME (JUDGEMENT(POSS_R (aL,pL), 
		(lhsL@(rmOnce (POSS(a,p)) lhsR),(rmOnce (POSS(a,p)) rhsL)@rhsR),
		[JUDGEMENT(CUT p, 
		((antecedent (hd premL))@(rmOnce p (antecedent (hd premR))),
		 (rmOnce p (succedent (hd premL)))@(succedent (hd premR))),
		premL@premR)]))
	else NONE
  | upBothOpt _ = NONE
and upBoth j =
      (case upBothOpt j of
	SOME j' => j'
      | NONE => j
      )
;


fun cutElim (ASSUMPTION a) = ASSUMPTION a
  | cutElim (JUDGEMENT(CUT x, (lhs,rhs), [premL,premR])) =
    (case upROpt (JUDGEMENT(CUT x,(lhs,rhs),[premL,premR])) of
      NONE => 
	(case upLOpt (JUDGEMENT(CUT x,(lhs,rhs), [premL,premR])) of
	  NONE => 
	    (case upBothOpt (JUDGEMENT(CUT x,(lhs,rhs),
		[premL,premR])) of
	      NONE => JUDGEMENT(CUT x, (lhs,rhs), [premL, premR])
	    | SOME p => p
	    )
	| SOME p => p
	)
    | SOME p => p
    )
  | cutElim other = other
(* alias for  fold cutElim *)
and cutElimR (ASSUMPTION a) = ASSUMPTION a
  | cutElimR (JUDGEMENT (rule,seq,premises)) =
	cutElim (JUDGEMENT(rule,seq, map cutElimR premises))
;


exception UN_CUTABLE;

fun mkCut (x,p1,p2) = 
    if isMem x (succedent p1) andalso isMem x (antecedent p2)
    then JUDGEMENT(CUT x, ((antecedent p1)@(rm x (antecedent p2)),
			   (rm x (succedent p1))@(succedent p2)), [p1,p2])
    else raise UN_CUTABLE
;

(* another way to "cut eliminate" is to use half the rules
   by using the duality
*)
end

