(* defintion of TypedTerm, the last step before code generation*)

structure TypedTerm =
struct
open TypeExpression

    datatype ITerm
        = iINT of int
        | iFLOAT of string (* Note 'real' is not an equality type! *)
        | iSTR of string
        | iVAR of string
        | iUNIT
        | iCTR of TypeExp * int * int (* type, number, total constructors *)
        | iDTR of TypeExp * int * int (* type, number, total destructors *)
        | iFUN of TypeExp * string    (* f *)
        | iTUPLE of TypeExp list * ITerm list
        | iAPP of ITerm * ITerm
        | iABS of TypeExp * string list * ITerm
        | iCASE of TypeGroup * ITerm list
        | iRECORD of TypeGroup * ITerm list


    (* given a term(without unbound variables) and the type of the term,
       generate a set of type equations. The term must already be tagged
       by calling renewTVarsInTerm
     *)
    (* Rename the type variables in each sub-term so that different term has different set
       of type variables
     *)

    (* Apply a predicate f:term -> bool over every non-recursive sub-term,
       and return a list of sub-terms that satisfy the predicate.           *)
    fun findITerm f term =
        let
            fun doFind (iTUPLE(_,l)) = doFindList(l)
              | doFind (iAPP(x,y)) = doFind(x) @ doFind(y)
              | doFind (iABS(_,varlst,t)) = doFind(t)
              | doFind (iCASE(_,l))     = doFindList(l)
              | doFind (iRECORD(_,l))   = doFindList(l)
              | doFind x = if f x then [x]
                                  else []
            and doFindList [] = []
              | doFindList (x::xs) = (doFind x) @ doFindList(xs)
        in
            doFind term
        end

    fun getITermFreeVars term =
        let
            fun freevars(boundvars, iTUPLE(_,l)) = freevarsinlist(boundvars, l)
              | freevars(boundvars, iAPP(x,y))= freevars(boundvars,x) @ freevars(boundvars,y)
              | freevars(boundvars, iABS(_,varlst,t)) = freevars(varlst@boundvars, t)
              | freevars(boundvars, iCASE(_,l)) = freevarsinlist(boundvars,l)
              | freevars(boundvars, iRECORD(_,l)) = freevarsinlist(boundvars,l)
              | freevars(boundvars, iVAR v) = if Set.isMember(v, boundvars) then []
                                                                            else [v]
              | freevars(boundvars, _) = []
            and freevarsinlist(boundvars, []) = []
              | freevarsinlist(boundvars, x::xs) = freevars(boundvars,x) @ freevarsinlist(boundvars,xs)
        in
            freevars([],term)
        end


    fun renewTVarsInTerm aTerm =
        let
            fun doTag(iTUPLE(_,l)) = iTUPLE(newTVarList(length l), doTagList(l))
              | doTag(iAPP(x,y))= iAPP(doTag(x), doTag(y))
              | doTag(iCASE(tg,l)) = iCASE(renewTVarsInTG tg, doTagList(l))
              | doTag(iRECORD(tg,l)) = iRECORD(renewTVarsInTG tg, doTagList(l))
              | doTag(iABS(_, varlst,t)) = iABS(tARROW(newTVarList (length varlst), newTVar()),
                                                varlst, doTag(t))
              | doTag(iCTR(te,i,n)) = iCTR(renewTVars te, i, n)
              | doTag(iDTR(te,i,n)) = iDTR(renewTVars te, i, n)
              | doTag(iFUN(te,n)) = iFUN(renewTVars te, n)
              | doTag(t) = t
            and doTagList([]) = []
              | doTagList(x::xs) = doTag(x) :: doTagList(xs)
        in
            doTag aTerm
        end

    fun genTypeEquations(aTerm, aTermType) =
        let
            fun decompList(st, [], _) = []
              | decompList(st, x::xl, y::yl) = decomp(st, x,y) @ decompList(st, xl,yl)
              | decompList(st, _, _) = raise Fail("decompList")
            and decomp(st, iINT _, te)   = [ (intTE, te) ]
              | decomp(st, iSTR _, te)  =  [ (stringTE, te) ]
              | decomp(st, iFLOAT _, te) = [ (realTE, te) ]
              | decomp(st, iVAR s, te)   = [ (Map.getValue(s, st), te) ]
              | decomp(st, iUNIT, te)    = [ (tUNIT, te) ]
              | decomp(st, iTUPLE(tl,l), te) = (tPRODS tl, te)::decompList(st, l, tl)
              | decomp(st, iAPP(f,t), te) =
                        let val T= newTVar()
                        in
                            decomp(st, f, tARROW([T],te))@ decomp(st, t, T)
                        end
              | decomp(st, iRECORD((recType,phraseTypes),l), te) =
                              (recType, te):: decompList(st, l, phraseTypes)
              | decomp(st, iCASE((caseType,phraseTypes), l), te) =
                              (caseType,te)::decompList(st, l, phraseTypes)

              | decomp(st, iCTR(ctrType, _,_), te) = [ (ctrType, te) ]
              | decomp(st, iDTR(dtrType, _,_), te) = [ (dtrType, te) ]
              | decomp(st, iFUN(funType, name), te)= [ (funType, te) ]
              | decomp(st, iABS(absT, arglst, exp), te) =
                        let
                            val newst= (ListPair.zip(arglst, srcList absT)) @ st
                        in
                            (absT, te)::decomp(newst, exp, dstType absT)
                        end
        in
            decomp([], aTerm, aTermType)
        end


end (*TypedTerm*)
