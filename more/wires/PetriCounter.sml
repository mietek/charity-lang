structure Counter :> Counter =
  struct

(* Second try; this time use Petri Nets.  *)


(* Create a counter Example for a failed proof 
   This relies on the "dp" program 
*)

(* The Counter Rules:


	Gamma,Gamma'   |- Delta
	-----------------------		L-And (R-OR) is backwards sound
	Gamma,/\Gamma' |- Delta

	Gamma,A            |- Delta
	-----------------------		L-OR (R-AND) needs only one counter
	Gamma,\/(A,Gamma') |- Delta

	I | xAy[],Psi | Gamma    |- Delta
	----------------------------------  L-Poss is backwards sound
	I | Psi | Gamma,<xAy>P |- Delta


	I | xAy[],Psi | Gamma  |- Delta
	----------------------------------  L-Poss is backwards sound
	I | Psi | Gamma,<xAy>P |- Delta


	I | xAy,Psi | Gamma,P,[xAy]P |- Delta
	-------------------------------------  L-Box Can't be Contracted
	I | xAy,Psi | Gamma,[xAy]P   |- Delta

	 
	x /\In(Gamma,Psi) = Empty
	y,I | Psi | Gamma |- Delta
	-------------------------------------  L-Guard is backwards sound
	x,I | xAy,Psi | Gamma |- Delta


	xAy is the only action with "x" inputs.
	y,I | Psi | Gamma |- Delta
	-------------------------------------  L-Guard is backwards sound
	x,I | xAy,Psi | Gamma |- Delta


	There is another action with "x" inputs
	but xAy can't be bound to anything in Gamma or Delta
	x,y,I | Psi | Gamma |- Delta
	-------------------------------------  L-Roll
	x,I | xAy,Psi | Gamma |- Delta

Another Set of rules:

This set uses a special "guard" called xA!.  


	I | xAy,Psi | Gamma,P,[xAy]P |- Delta		No Change
	-------------------------------------  L-Box Can't be Contracted
	I | xAy,Psi | Gamma,[xAy]P   |- Delta


	I | xAy[],Psi | Gamma  |- Delta		Eliminate any xA! from Psi
	-----------------------------------  L-Poss is backwards sound
	I | Psi-xA! | Gamma,<xAy>P |- Delta


	I | {xAy[Ps]},xA!,Psi | Gamma  |- Delta
	-------------------------------------------  L-Box-W only if xA!
	I | {xAy[P,Ps]},Psi | [xAy]P,Gamma |- Delta


	y /\In(Gamma,Psi) = Empty
	I | Psi | Gamma |- Delta
	-------------------------------------  L-GuardW is backwards sound
	I | xAy[],xA!,Psi | Gamma |- Delta	(but Weak isn't)


	x /\In(Gamma,Psi) = Empty		xAy is the only action 
	y,I | Psi | Gamma |- Delta		with "x" inputs.
	-------------------------------------  L-Guard is backwards sound
	x,I | xAy[],xA!,Psi | Gamma |- Delta


	There is another action with "x" inputs
	but xAy can't be bound to anything in Gamma or Delta
	x,y,I | Psi | Gamma |- Delta
	-------------------------------------  L-Roll
	x,I | xAy[],Psi | Gamma |- Delta


	{xVz} /\ Delta = 0
	 x,I | Psi | Gamma  |- Delta..|O,z
	-------------------------------------------  Vars
	x,I | Psi | xVz,Gamma |- Delta |O,z


The effect of these rules:
  1. must introduce all of the xAy guards before any of the [xAy]P 
	propositions into antecedent or <xAy>P into succedent.
  2. must introduce all of the [xAy]P propositions into antecedent
	or <xAy>P propositions into succedent before introducing
	a <xAy>P into antecedent.  
	(i.e. introduce all [xAy]P before a release )
  3. Must bind all [xAy]P's before doing a release for some xAy.
  4. May check for "variables" after all of its guards have been eliminated.

What does this mean for Model Building?



  1. At the rule Vars, 
		
		------------------------------------------
		I | Psi | boxes,[{xVz}] |M'- Delta
	M' = [{xVz}]


  2. At L-Poss (R-Box, L-ssoP, R-xoB) 

	I | xAy[],Psi | Gamma    |M- Delta
	----------------------------------  L-Poss is backwards sound
	I | Psi | Gamma,<xAy>P |M'- Delta
	M' = (xAy)::M

  3. At each L-Box
	I | {xAy[Ps]},Psi | Gamma  |M- Delta
	-------------------------------------------  L-Box-W only if xA!
	I | {xAy[P,Ps]},Psi | [xAy]P,Gamma |M'- Delta
	M' = (xAy)::M

*)

(* The Modelling Rules for Petri Nets.
	A Petri Net is a list of "components".
	Components have inputs, a name, outputs

	Gamma |- P
	-----------------		R-OR
	Gamma |- OR(P,Q)

	{Gamma |- Pi}
	-----------------		R-AND
	Gamma |- AND({Pi})

	y,I/I',x | Gamma |- yP
	---------------------------  R-Poss 
	x,I/I'   | Gamma |- <xAy>P

	{y,I/I',x| Psi |- yP}
	-------------------------------  R-Box
	x,I/I'   | {xAy},Psi |- [xAy]P

	I/I',x | Psi         |- P
	-------------------------------------  L-GuardW 
	I/I',x | {x,z}Ay,Psi |- P

	-------------------------------------  ID
	I/I' | {I}A{O},Psi |- {I}A{O}	| O'/O
	
*)
exception HAS_A_PROOF;
(* exception CE_ERROR of string; *)



fun anteOK ante =
	List.all (fn (AND _) => false | (OR _) => false | (L_POSS _) => false
		| (R_POSS _) => false | _ => true) ante
and sucOK ante =
	List.all (fn (AND _) => false | (OR _) => false | (L_BOX _) => false
		| (R_BOX _) => false | _ => true) ante
and boxBoundToAll gL (L_BOX((x,A,y),p)) =
    List.all (fn ((x',A',y'),props) => 
	if x=x' andalso A=A' 
	then isMem (L_BOX((x,A,y),p)) props
	else true) gL
  | boxBoundToAll gL p = true
and possBoundToAll gL (L_POSS((x,A,y),p)) =
    List.all (fn ((x',A',y'),props) => 
	if x=x' andalso A=A' 
	then isMem (L_POSS((x,A,y),p)) props
	else true) gL
  | possBoundToAll gL p = true
and xobBoundToAll gR (R_BOX(p,(x,A,y))) =
    List.all (fn (props,(x',A',y')) => 
	if y=y' andalso A=A' 
	then isMem (R_BOX(p,(x,A,y))) props
	else true) gR
  | xobBoundToAll gR p = true
and ssopBoundToAll gR (R_POSS(p,(x,A,y))) =
    List.all (fn (props,(x',A',y')) => 
	if y=y' andalso A=A' 
	then isMem (R_POSS(p,(x,A,y))) props
	else true) gR
  | ssopBoundToAll gR p = true
;

(*
fun allBound (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    anteOK ante andalso sucOK suc andalso
    List.all (boxBoundToAll gL ante) andalso
    List.all (possBoundToAll gR suc) andalso
    List.all (xobBoundToAll gL ante) andalso
    List.all (ssopBoundToAll gR suc) 
and boxW (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    let val (boxXobs,rest) = List.partition 
		(fn (L_BOX _) => true | (R_BOX _) => true | _ => false)
	val boxTypes = map getTypes boxXobs
	val typedAnds = map (fn (ins,outs) => AND(ins,[],outs)) boxTypes
    in
      if allBound (ASSUMPTION(i,gL,ante,suc,gR,ou)) 
      then ((fn c => CIRCUITS.CIR_COMPOSITION(c::typedAnds)), 
		ASSUMPTION(i,gL,rest,suc,gR,ou))
      else (I,ASSUMPTION(i,gL,ante,suc,gR,ou))
    end
and possW (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    let val (possSsop,rest) = List.partition 
		(fn (L_POSS _) => true | (R_POSS _) => true | _ => false)
	val possTypes = map getTypes possSsop
	val typedAnds = map (fn (ins,outs) => AND(ins,[],outs)) possTypes
    in
      if allBound (ASSUMPTION(i,gL,ante,suc,gR,ou)) 
      then ((fn c => CIRCUITS.CIR_COMPOSITION(c::typedAnds)), 
		ASSUMPTION(i,gL,ante,rest,gR,ou))
      else (I,ASSUMPTION(i,gL,ante,suc,gR,ou))
    end

(* Another simpler way of achieving the same ends *)

fun nonPoss (ASSUMPTION(i,gL,ante,suc,gR,ou)) = 
	(map (fn ((x,A,_),_) => (x,A)) gL) @
	(List.concat (map (fn (L_BOX((x,A,_),_)) => [(x,A)] | _ => []) ante))@
	(List.concat (map (fn (L_POSS((x,A,_),_)) => [(x,A)] | _ => []) suc))
and nonSsop (ASSUMPTION(i,gL,ante,suc,gR,ou)) = 
	(map (fn (_,(_,A,y)) => (A,y)) gR)@
	(List.concat (map (fn (R_BOX(_,(x,A,_))) => [(A,x)] | _ => []) ante))@
	(List.concat (map (fn (R_POSS(_,(x,A,_))) => [(A,x)] | _ => []) suc))
and compAND p x = (CIRCUITS.CIR_AND(inputVars p,[],outputVars p))::x
and boxW lNP (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.find (fn (L_BOX((x,A,y),p)) =>
		isMem (x,A) lNP andalso
		boxBoundToAll gL (L_BOX((x,A,y),p)) | _ => false) ante of
      NONE => NONE
    | SOME(p) => 
      let val ante' = rm p ante
	  val gL' = map (fn (act,props) => (act,rm p props)) gL
      in
      	SOME(compAND p, ASSUMPTION(i,gL',ante',suc,gR,ou))
      end
    )
and possW lNP (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.find (fn (L_POSS((x,A,y),p)) =>
		isMem (x,A) lNP andalso
		boxBoundToAll gL (L_POSS((x,A,y),p)) | _ => false) ante of
      NONE => NONE
    | SOME(p) => 
      let val ante' = rm p ante
	  val gL' = map (fn (act,props) => (act,rm p props)) gL
      in
      	SOME(compAND p, ASSUMPTION(i,gL',ante',suc,gR,ou))
      end
    )
and xobW rNP (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.find (fn (R_BOX(p,(x,A,y))) =>
		isMem (A,y) rNP andalso
		xobBoundToAll gR (R_BOX(p,(x,A,y))) | _ => false) ante of
      NONE => NONE
    | SOME(p) => 
      let val ante' = rm p ante
	  val gR' = map (fn (props,act) => (rm p props,act)) gR
      in
      	SOME(compAND p,ASSUMPTION(i,gL,ante',suc,gR',ou))
      end
    )
and ssopW rNP (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.find (fn (R_POSS(p,(x,A,y))) =>
		isMem (A,y) rNP andalso
		xobBoundToAll gR (R_POSS(p,(x,A,y))) | _ => false) ante of
      NONE => NONE
    | SOME(p) => 
      let val ante' = rm p ante
	  val gL' = map (fn (act,props) => (act,rm p props)) gL
      in
      	SOME(compAND p, ASSUMPTION(i,gL',ante',suc,gR,ou))
      end
    )
and counterGuardWeak (ASSUMPTION a) =
    (case guardWeak (ASSUMPTION a) of
      (JUDGEMENT(WEAKG,_,[ASSUMPTION a'])) => SOME(I,ASSUMPTION a')
    | _ => NONE
    )
(* rightGuard returns all possible reductions, not just one *)
and counterRightGuard (ASSUMPTION a) =
    (case rightGuard (ASSUMPTION a) of
      (JUDGEMENT(GUARDR,_,[assumption])) => 
	SOME(I,
	  proofFold (fn (JUDGEMENT(GUARDR,_,[a])) => a | j => j) assumption)
    | _ => NONE
    )
and counterLeftGuard (ASSUMPTION a) =
    (case leftGuard (ASSUMPTION a) of
      (JUDGEMENT(GUARDL,_,[assumption])) => 
	SOME(I,
	  proofFold (fn (JUDGEMENT(GUARDL,_,[a])) => a | j => j) assumption)
    | _ => NONE
    )
and fixIns i gL ante suc = 
	noDups (List.concat ((map (fn ((x,A,y),_) => x) gL)@
			(map inputVars (ante@suc))))
and fixOuts ou gR ante suc = 
	noDups (List.concat ((map (fn (_,(x,A,y)) => y) gR)@
			(map outputVars (ante@suc))))
and leftRoll lNP (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.find 
    (fn ((x,A,y),[]) => isMem (x,A) lNP andalso subset x i | _ => false) gL of
      NONE => NONE
    | SOME((x,A,y),_) => 
      let val gL' = rm ((x,A,y),[]) gL
	  val i' = fixIns i gL' ante suc
      in
	    SOME(I,ASSUMPTION(i',gL',ante,suc,gR,ou))
      end
    )
and rightRoll rNP (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.find 
    (fn ([],(x,A,y)) => isMem (A,y) rNP andalso subset y ou | _ => false) gR of
      NONE => NONE
    | SOME(_,(x,A,y)) => 
      let val gR' = rm ([],(x,A,y)) gR
	  val ou' = fixOuts ou gR' ante suc
      in
	    SOME(I,ASSUMPTION(i,gL,ante,suc,gR',ou'))
      end
    )
and vars (ASSUMPTION(i,gL,ante,suc,gR,ou)) =
    (case List.filter 
	(fn (VAR(x,A,y)) => subset x i andalso subset y ou 
		| _ => false) ante of 
      [] => NONE
    | (VAR(x,A,y)::rest) =>
       let val (anteVars,ante') = List.partition 
	    (fn (VAR(x',_,y')) => x = x' andalso y = y' | _ => false) ante
           val (sucVars,suc') = List.partition 
	    (fn (VAR(x',_,y')) => x = x' andalso y = y' | _ => false) suc
	   val andVars = varsToCir anteVars
	   val i' = fixIns i gL ante' suc'
	   val ou' = fixOuts ou gR ante' suc'
       in
	 if intersection sucVars anteVars = []
	 then SOME(fn x => andVars::x, ASSUMPTION(i',gL,ante',suc',gR,ou'))
	 else raise HAS_A_PROOF
       end
    )
;
*)


fun newLAct ((i,gL,ante,suc,gR,ou))
	    ((_,gL',  _,  _, _, _)) =
    (case setDiff (map fst gL') (map fst gL) of
     [act] => act
    | _ => raise HAS_A_PROOF (* need some other exception *)
    )
and newRAct ((i,gL,ante,suc,gR,ou))
	    ((_, _,   _,  _,gR', _)) =
    (case setDiff (map snd gR') (map snd gR) of
     [act] => act
    | _ => raise HAS_A_PROOF (* need some other exception *)
    )
;

fun counter (JUDGEMENT(CUT _,conclusion,premises)) = 
	counter (dp (ASSUMPTION conclusion))
  | counter (JUDGEMENT(WEAKP,conclusion,premises)) = 
	counter (dp (ASSUMPTION conclusion))
  | counter (JUDGEMENT(LPOSS, c, [premise])) = 
	let val act = newLAct c (conclusion premise)
	in
	  act::(counter premise)
	end
  | counter (JUDGEMENT(RBOX, c, [premise])) = 
	let val act = newLAct c (conclusion premise)
	in
	  act::(counter premise)
	end
  | counter (JUDGEMENT(LSSOP, c, [premise])) = 
	let val act = newRAct c (conclusion premise)
	in
	  act::(counter premise)
	end
  | counter (JUDGEMENT(RXOB, c, [premise])) = 
	let val act = newRAct c (conclusion premise)
	in
	  act::(counter premise)
	end
  | counter (JUDGEMENT(rule,sequent,premises)) = 
    (case List.filter (fn p => not (isAProof p)) premises of
      [] => raise HAS_A_PROOF
    | (p::_) => counter p
    )
    (* for the cases of L-AND/R-OR, there is only one premise;
       for the cases of L-OR/R-AND, (distribution) pick a "failing" premise
       for the cases of LBOX,RPOSS,LXOB,RSSOP the rules are backward sound
       for the cases of WEAKG, GUARDL, GUARDR the rules are backward sound
    *)
  | counter (ASSUMPTION seq) = 
    (* The decision procedure failed; start working *)
    (case dp (ASSUMPTION seq) of
	 (JUDGEMENT j) => counter (JUDGEMENT j)
    | (ASSUMPTION(i,gL,ante,suc,gR,ou)) =>
	List.concat (List.map (fn (VAR p) => [p] | _ => []) ante)
    )
;

(* Tests:

   Counter.counter 
val ante2 = COMPILE "<{u}B{v}>[{x}A{y}]{y,v}P{}";
val suc2 = COMPILE "[{x}A{y}]<{u}B{v}>{y,v}P{}";

val seq2 = ASSUMPTION(["x","u"],[],[ante2],[suc2],[],[]);
dp seq2;
  This works.  The converse doesn't.

val seq2' = ASSUMPTION(["x","u"],[],[suc2],[ante2],[],[]);

dp seq2';
  fails (as it should)

Counter.counter seq2';
cirCoalesce it;
CIRCUITS.ppCircuitText (hd it);



val ante3 = COMPILE 
"{x,u}&((<{x}A{y}>{y,u}&(){}) , (<{u}B{v}>([{x}A{y}]{y,v}P{}))){}";
val suc3 = COMPILE " {x,u}+(){}";

val seq3 = ASSUMPTION(["x","u"],[],[ante3],[suc3],[],[]);

dp seq3;

Counter.counter seq3;


val ante4 = COMPILE 
("{x,u}&((<{x}A{y}>({y,u}P{})) , (<{x}A{y}>({y,u}Q{})) , " ^
"(<{u}B{v}>([{x}A{y}]{y,v}R{}))){}");
val suc4 = COMPILE " {x,u}+(){}";

val seq4 = ASSUMPTION(["x","u"],[],[ante4],[suc4],[],[]);

dp seq4;

Counter.counter seq4;


*)

end
;
