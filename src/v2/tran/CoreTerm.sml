(*core charity term and pattern *)
structure CoreTerm =
struct
structure UT = Utility
    (**********************************************************************
     *                                                                    *
     *    Representing and manipulating Charity terms                     *
     *                                                                    *
     **********************************************************************)

    datatype CPattern
        = cpVAR of string
        | cpFUN of string
        | cpCTR of string * int * CPattern list
        | cpTUPLE of CPattern list
        | cpRECORD of string * CPattern list (* dataname, patterns corresponding to each destructor *)
        | cpINTRANGE of int * int

    (* return a list of variables in a CPattern, excluding the don't care pattern "_" *)
    fun getCPatternVars(cpVAR "_") = []
      | getCPatternVars(cpVAR v) = [ v ]
      | getCPatternVars(cpCTR(_,_, pl)) = getCPatternVarsInList pl
      | getCPatternVars(cpTUPLE pl) = getCPatternVarsInList pl
      | getCPatternVars(cpRECORD(_,rl)) = getCPatternVarsInList rl
      | getCPatternVars _ = []
    and getCPatternVarsInList [] = []
      | getCPatternVarsInList (p::pl) = (getCPatternVars p) @ (getCPatternVarsInList pl)

    (* return a list of function variables in a Pattern, excluding the don't care pattern "_" *)
    fun getCPatternFuns(cpFUN "_") = []
      | getCPatternFuns(cpFUN f) = [ f ]
      | getCPatternFuns(cpCTR(_,_, pl)) = getCPatternFunsInList pl
      | getCPatternFuns(cpTUPLE pl) = getCPatternFunsInList pl
      | getCPatternFuns(cpRECORD(_,rl)) = getCPatternFunsInList rl
      | getCPatternFuns _ = []
    and getCPatternFunsInList [] = []
      | getCPatternFunsInList (p::pl) = (getCPatternFuns p) @ (getCPatternFunsInList pl)


    datatype CTerm
        = cINT of int
        | cFLOAT of string (* Note 'real' is not an equality type! *)
        | cSTR of string
        | cVAR of string
        | cCTR of string * int (* datatype name, constructor number *)
        | cDTR of string * int (* datatype name, destructor number *)
        | cFUN of string       (* function name *)
        | cMACRO of int
        | cUNIT                (* unit is the only value of datatype 1 *)
        (*the following are recursively defined *)
        | cTUPLE of CTerm list
        | cAPP of CTerm * CTerm
        | cABS of string list * CTerm
(*a list of recursive function defintion and a term name,
  similar to the let ...in ...end structure.
  the function is defined as (name of function, the recursive argument position, body of the function
 *)
        | cLETREC of (string * int * CTerm) list * CTerm
        | cCASE of string * CTerm list
        | cRECORD of string * CTerm list
        | cPattABS of (CPattern list * CTerm) list
        | cCOMB of string * CTerm list (* f{g} *)
        | cINLINE of string list

    fun isConstant (cINT _) = true
      | isConstant (cSTR _) = true
      | isConstant (cFLOAT _) = true
      | isConstant (cTUPLE []) = true
      | isConstant _ = false

    fun isVariable (cVAR _) = true
      | isVariable _ = false

    fun isConstructor (cCTR _) = true
      | isConstructor _ = false


    (* a rough estimate of the size of a term *)
    fun termSize term =
        let
            fun doCount (cTUPLE l) = 1 + doCountList(l)
              | doCount (cAPP(x,y)) = doCount(x) + doCount(y)
              | doCount (cABS(_,t)) = doCount(t)
              | doCount (cLETREC _) = 100 (* let rec is always considered large *)
              | doCount (cCOMB(n,l))     = 2 + doCountList(l)
              | doCount (cCASE(_,l))     = 2 + doCountList(l)
              | doCount (cRECORD(_,l))   = 1 + doCountList(l)
              | doCount x                = 1
            and doCountList [] = 0
              | doCountList (x::xs) = (doCount x) + doCountList(xs)
        in
            doCount term
        end

    (* Map a function f:term -> term over every sub-term(including recursive ones)
       inner-most subterms are mapped first
     *)
    fun mapTerm f term =
        let
            fun doMap (cTUPLE(l))   = f(cTUPLE(map doMap l))
              | doMap (cAPP(x,y))      = f(cAPP(doMap x, doMap y))
              | doMap (cABS(varlst,t)) = f(cABS(varlst, doMap t))
              | doMap (cLETREC(letlst, t)) = f(cLETREC(map (fn (n,i,t)=>(n,i,doMap t)) letlst, doMap t))
              | doMap (cCOMB(n,l))     = f(cCOMB(n, map doMap l))
              | doMap (cCASE(n,l))     = f(cCASE(n, map doMap l))
              | doMap (cRECORD(n,l))   = f(cRECORD(n, map doMap l))
              | doMap (cPattABS l) = f(cPattABS(map (fn (pl,t)=> (pl, doMap t)) l))
              | doMap x = f x
        in
            doMap term
        end

    (* Apply a predicate f:term -> bool over every non-recursive sub-term,
       and return a list of sub-terms that satisfy the predicate.           *)
    fun findTerm f term =
        let
            fun doFind (cTUPLE l) = doFindList(l)
              | doFind (cAPP(x,y)) = doFind(x) @ doFind(y)
              | doFind (cABS(varlst,t)) = doFind(t)
              | doFind (cLETREC(letlst,t)) = doFind(t) @ doFindList(map #3 letlst)
              | doFind (cCOMB(_,l))     = doFindList(l)
              | doFind (cCASE(_,l))     = doFindList(l)
              | doFind (cRECORD(_,l))   = doFindList(l)
              | doFind (cPattABS l) = doFindList(map #2 l)
              | doFind x = if f x then [x]
                                  else []
            and doFindList [] = []
              | doFindList (x::xs) = (doFind x) @ doFindList(xs)
        in
            doFind term
        end
(*
    fun findErrors term =
        let fun f (cERROR l) = true
              | f _ = false
            fun flat [] = []
              | flat ((cERROR x)::xs) = x @ flat xs
              | flat _ = raise Fail("findErrors")
        in
            flat(findTerm f term)
        end
*)
    (* return a list of variables, each corresponding to an occurrence of a
       free variable. The result may contain duplicates
       assuming the term contains no cPattABS
     *)
    fun getFreeVars term =
        let
            fun freevars(boundvars, cTUPLE l) = freevarsinlist(boundvars, l)
              | freevars(boundvars, cAPP(x,y))= freevars(boundvars,x) @ freevars(boundvars,y)
              | freevars(boundvars, cABS(varlst,t)) = freevars(varlst@boundvars, t)
              | freevars(boundvars, cCOMB(_,l)) = freevarsinlist(boundvars,l)
              | freevars(boundvars, cCASE(_,l)) = freevarsinlist(boundvars,l)
              | freevars(boundvars, cRECORD(_,l)) = freevarsinlist(boundvars,l)
              | freevars(boundvars, cPattABS l) = raise Fail("getFreeVars")
              | freevars(boundvars, cLETREC(letlst,t)) = freevarsinlist(boundvars, map #3 letlst)
                                                         @ freevars(boundvars, t)

              | freevars(boundvars, cVAR v) = if Set.isMember(v, boundvars) then []
                                                                            else [v]
              | freevars(boundvars, _) = []
            and freevarsinlist(boundvars, []) = []
              | freevarsinlist(boundvars, x::xs) = freevars(boundvars,x) @ freevarsinlist(boundvars,xs)
        in
            freevars([],term)
        end

    fun getFreeNames term =
        let
            fun freenames(boundnames, cTUPLE l) = freenamesinlist(boundnames, l)
              | freenames(boundnames, cAPP(x,y))= freenames(boundnames,x) @ freenames(boundnames,y)
              | freenames(boundnames, cABS(varlst,t)) = freenames(varlst@boundnames, t)
              | freenames(boundnames, cCOMB(_,l)) = freenamesinlist(boundnames,l)
              | freenames(boundnames, cCASE(_,l)) = freenamesinlist(boundnames,l)
              | freenames(boundnames, cRECORD(_,l)) = freenamesinlist(boundnames,l)
              | freenames(boundnames, cPattABS l) = raise Fail("getFreeNames")
              | freenames(boundnames, cLETREC(letlst,t)) =
                    let val newnames = (map #1 letlst) @ boundnames
                    in
                        freenamesinlist(newnames, map #3 letlst)
                        @ freenames(newnames, t)
                    end
              | freenames(boundnames, cFUN v) = if Set.isMember(v, boundnames) then []
                                                                            else [v]
              | freenames(boundnames, cVAR v) = if Set.isMember(v, boundnames) then []
                                                                            else [v]
              | freenames(boundnames, _) = []
            and freenamesinlist(boundnames, []) = []
              | freenamesinlist(boundnames, x::xs) = freenames(boundnames,x) @ freenamesinlist(boundnames,xs)
        in
            freenames([],term)
        end

    (* return a list of names, corresponding to an declaration of a
       bound variable/functions. The result may contain duplicates.
       assuming the term contains no cPattABS
     *)
    fun getBoundNames term =
        let
            fun boundvars(cTUPLE l) = boundvarsinlist l
              | boundvars(cAPP(x,y))= (boundvars x) @ (boundvars y)
              | boundvars(cABS(varlst,t)) = varlst @ (boundvars t)
              | boundvars(cLETREC(letlst,t)) = (map #1 letlst) @
                                               (boundvarsinlist (map #3 letlst)) @
                                               boundvars t
              | boundvars(cCOMB(_,l)) = boundvarsinlist l
              | boundvars(cCASE(_,l)) = boundvarsinlist l
              | boundvars(cRECORD(_,l)) = boundvarsinlist l
              | boundvars(cPattABS l) = raise Fail("getBoundNames")
              | boundvars(_) = []
            and boundvarsinlist([]) = []
              | boundvarsinlist(x::xs) = (boundvars x) @ (boundvarsinlist xs)
        in
            boundvars term
        end

    (* Give all bound variables in varlst a unique name
       Assuming the term contains no cPattABS
     *)
    fun renameAllBoundNames term =
        let
            fun doRename(namepairs, cTUPLE l) = cTUPLE(doRenameList(namepairs, l))
              | doRename(namepairs, cAPP(x,y))= cAPP(doRename(namepairs,x), doRename(namepairs,y))
              | doRename(namepairs, cCOMB(n,l)) = cCOMB(n, doRenameList(namepairs,l))
              | doRename(namepairs, cCASE(n,l)) = cCASE(n, doRenameList(namepairs,l))
              | doRename(namepairs, cRECORD(n,l)) = cRECORD(n, doRenameList(namepairs,l))
              | doRename(namepairs, cLETREC(letlst,t)) =
                    let val oldletnames = map #1 letlst
                        val newletnames = List.tabulate(length letlst, fn _=>"$nv" ^ UT.newName())
                        val newnamepairs = ListPair.zip(oldletnames,newletnames) @ namepairs
                        val newletlst = ListPair.map (fn (newname,(oldname,i,tt)) =>
                                                            (newname,i, doRename(newnamepairs, tt)))
                                                     (newletnames, letlst)
                    in
                        cLETREC(newletlst, doRename(newnamepairs, t))
                    end

              | doRename(namepairs, cABS(varlst,t)) =
                    let
                        val newnames = List.tabulate(length varlst,
                                                     fn _ => "$nv" ^ Int.toString(UT.newID()))

                    in
                        cABS(newnames, doRename(ListPair.zip(varlst,newnames)@namepairs, t))
                    end
              | doRename(namepairs, cVAR v) =
                    ( case Map.find(v, namepairs)
                        of SOME newname => cVAR newname
                         | NONE => cVAR v
                    )
              | doRename(namepairs, cFUN v) =
                    ( case Map.find(v, namepairs)
                        of SOME newname => cFUN newname
                         | NONE => cFUN v
                    )
              | doRename(namepairs, cPattABS _) = raise Fail("renameAllBoundNames")
              | doRename(namepairs, t) = t
            and doRenameList(namepairs, []) = []
              | doRenameList(namepairs, x::xs) = doRename(namepairs,x) :: doRenameList(namepairs,xs)
        in
            doRename([], term)
        end

    (* substitute all free occurrences of varname in term, i.e.
       term[newterm/varname], bound variables in term have to be renamed if
       conflict with free variables in newterm otherwise variable capture may occur.
       NOTE cFUN varname will also be replaced! (higher-order function variable)
     *)
    fun substFreeName (varname, newterm) term =
        let val boundvars = getBoundNames term
            val freevars  = getFreeVars newterm
            (* A substitution M[N/x] is safe provided the bound variables of M are
               disjoint from the free variables of N. Otherwise, all
               bound variables have to be renamed
             *)

            fun doSubst(cTUPLE(l)) = cTUPLE(doSubstList(l))
              | doSubst(cAPP(x,y))= cAPP(doSubst x, doSubst y)
              | doSubst(cCOMB(n,l)) = cCOMB(n, doSubstList l)
              | doSubst(cCASE(n,l)) = cCASE(n, doSubstList l)
              | doSubst(cRECORD(n,l)) = cRECORD(n, doSubstList l)
              | doSubst(cLETREC(letlst, t)) =
                    if Set.isMember(varname, map #1 letlst) then
                        (*shadowed by the letrec definition *)
                        cLETREC(letlst, t)
                    else
                        cLETREC(map (fn (n,i,t) => (n,i, doSubst t)) letlst,
                                doSubst t)
              | doSubst(cABS(varlst,t)) =
                  (* if the variable varname is declared in an abstraction
                     no need to check the body because varname is shadowed
                     by the new bound
                   *)
                    if Set.isMember(varname, varlst) then
                        cABS(varlst, t)
                    else
                        cABS(varlst, doSubst t)
              | doSubst(cVAR v) = if v=varname then newterm else cVAR v
              | doSubst(cFUN v) = if v=varname then newterm else cFUN v
              | doSubst(t) = t
            and doSubstList([]) = []
              | doSubstList(x::xs) = (doSubst x) :: (doSubstList xs)
        in
            doSubst (if Set.intersect(boundvars, freevars)=[] then
                            term
                        else
                            renameAllBoundNames term )
        end
    (* subst all cMACRO in a term with the ith item in macsubList *)
    fun substMacros macsubList term =
        let val boundvars = getBoundNames term
            val freevars = getFreeVars (cTUPLE macsubList)
            val term2 = if Set.intersect(boundvars, freevars)=[] then
                            term
                        else
                            renameAllBoundNames term
            fun f (cMACRO i) = List.nth(macsubList, i)
              | f x = x
        in
            mapTerm f term2
        end
(*
    fun isRecursive term =
        let fun isRecur (iRECUR) = true
              | isRecur _ = false
        in
            (findTerm isRecur term)<>[]
        end
*)
    fun hasMacros term =
        let fun isMac (cMACRO _) = true
              | isMac _ = false
        in
            (findTerm isMac term)<>[]
        end
(*    fun macroOccurCount(term,macid) =
        let fun isMacro (cMACRO i) = i=macid
              | isMacro _ = false
        in
            length (findTerm isMacro term)
        end
*)
    (* lambda-lift a term so that it contains no free variables.
       Recursion with the term are handled so that type mismatch can not happen
       return a pair: the lifted term, a function that can map a call on
       the old term into a term with the same type

    fun lambdaLift term =
        let val freevars = removeDup (getFreeVars term)
            val newvarCnt= length freevars
            val oldvars  = (case term
                             of cABS(varlst,_) => varlst
                              | _ => []
                           )
            val wrap = (fn t => cABS(oldvars,
                                     cAPP(t,
                                          cTUPLE([], map (fn s=>cVAR s) (oldvars @ freevars))
                                    )))
            fun changeRecur fname (cFUN x) = if x=fname then (wrap cFUN x) else cFUN x
              | changeRecur fname x = x
        in
            if newvarCnt=0 then
                (term, (fn x=>x))
            else
                case term
                  of cABS(x, _, varlst, t) =>
                        (case x
                          of NONE => ( cABS(varlst @ freevars,t)
                                     , wrap)
                           | SOME(fname,_) =>
                                (cABS(x, nullTE, varlst @ freevars,
                                      mapTerm (changeRecur fname) t),
                                 wrap)
                        )
                   | t =>
                        ( cABS(NONE, nullTE, freevars, t)
                        , wrap)
        end
    *)

    (*make all the functions in cLETREC a complete function*)
    fun lambdaLift aTerm =
        (* lambda lift should be done from outside to inside,
           because inner-functions may call outside functions
         *)
        let
            fun doLift(cTUPLE l)       = cTUPLE(map doLift l)
              | doLift(cAPP(x,y))      = cAPP(doLift x, doLift y)
              | doLift(cABS(varlst,t)) = cABS(varlst, doLift t)
              | doLift(cCOMB(n,l))     = cCOMB(n, map doLift l)
              | doLift(cCASE(n,l))     = cCASE(n, map doLift l)
              | doLift(cRECORD(n,l))   = cRECORD(n, map doLift l)
              | doLift(cPattABS l)     = cPattABS(map (fn (pl,t)=> (pl, doLift t)) l)
              | doLift(cLETREC(aletlst, aTerm)) =
                let (* collect free variables in all the let functions
                       because they may call each other. This may not
                       be the optimal solution, but it's simpler
                     *)
                    val freevars = UT.removeDup(getFreeVars (cTUPLE(map #3 aletlst)))
                    (*val newfreevars = map (fn x=>"$ll"^x) (UT.newNameList(length freevars))*)
                    val newvarCnt = length freevars
                    fun wrap(fname, _, fbody) =
                        if newvarCnt = 0 then
                            cFUN fname
                        else
                            (case fbody
                               of cABS(varlst,t) =>
                                    cABS(varlst, cAPP(cFUN fname,
                                                      cTUPLE(map (fn s=>cVAR s) (varlst @ freevars))))
                                | t => cAPP(cFUN fname, cTUPLE(map (fn s=>cVAR s) freevars))
                            )

                    val wraplst = map wrap aletlst
                    val namelst = map #1 aletlst

                    fun dosub([], _, aterm) = aterm
                      | dosub(fname::ns, wrap::ws, aterm) =
                            dosub(ns,ws, substFreeName (fname, wrap) aterm)
                      | dosub _ = raise Fail("dosub in lambdaLift")
                    fun f(cABS(varlst,t)) = cABS(varlst@freevars, dosub(namelst,wraplst,t))
                      | f(t) = cABS(freevars, dosub(namelst, wraplst,t))
                in
                    cLETREC(map (fn (n,i,t) => (n,i, doLift (f t))) aletlst,
                            doLift(dosub(namelst,wraplst,aTerm))
                           )
                end
              | doLift x = x
        in
            mapTerm doLift aTerm
        end


    (*******************************************************
     *                 Optimize a CTerm                   *
     *******************************************************)
    fun optimizeTerm aTerm =
        let

        (* how many time varname occurrs freely in term *)
        fun varOccurCnt(term, varname) =
            let val freevars = getFreeVars term
            in
                length(List.filter (fn x=> x=varname) freevars)
            end
        fun getVarList n = List.tabulate(n, fn i => "$op" ^ Int.toString(UT.newID()))

        fun isSimple x = isConstant x orelse isVariable x orelse isConstructor x

        fun doOpt (cTUPLE([x]))  = x
          | doOpt (cABS([],t))   = t
          | doOpt (cLETREC(letlst,t))= optLETREC(letlst, t)
          | doOpt (cAPP(x,y))    = optApp(x, y)
          | doOpt x  = x
        and optLETREC([],aterm) = aterm
          | optLETREC(aletlst, aterm) =
            let
                val freenames = getFreeNames(cTUPLE(aterm::(map #3 aletlst)))
                fun occurCnt aname = length(List.filter (fn x=>x=aname) freenames)
                fun paramCount (cABS(l,_)) = length l
                  | paramCount (cCASE _) = 1
                  | paramCount _ = 0

                (* If we make the term a function, we want to know the size of the
                   term that calls such a function. If the size of calling is no less than
                   the size of the whole term, it's actually cheaper that we don't
                   make the term a function. If the term is an ABS
                    x1,...,xn => t
                   and t contains free variables y1,...,ym, then after lambda lifting it
                   into a function f, calling f is in the form
                    (x,...,xn)=> f (x1,...,xn, y1,...,ym)
                   which is of size n+m+1.
                 *)

                fun wrapsize aterm =
                    let val freecnt = length (getFreeVars aterm)
                    in
                        if  freecnt = 0 then
                            1
                        else
                            freecnt + paramCount aterm + 1
                    end
                (* in the list, find a substitable let definition and subst it.
                   The result may have other clauses that can be substituted,
                   so we recursively call optLETREC
                 *)
                fun findandsub [] = cLETREC(aletlst, aterm)
                  | findandsub ((afname, _, afterm)::ls) =
                        if occurCnt afname<=1 orelse termSize afterm<= wrapsize afterm then
                                (* do the substitution *)
                                let val newterm = substFreeName (afname,afterm) aterm
                                    val newlets = map (fn (n,i,t) => (n,i, substFreeName (afname,afterm) t))
                                                      (List.filter (fn (n,_,_) => n<>afname) aletlst)
                                in
                                    optimizeTerm(cLETREC(newlets, newterm))
                                end
                        else
                            findandsub ls
            in
                findandsub aletlst
            end

        and optApp (afun, aterm) =
              case (afun, aterm)
                of ( cABS([varname], absbody),  y) =>
                        if isSimple y orelse varOccurCnt(absbody, varname)<=1  then
                            (* new optimization may be available after the substitution *)
                            optimizeTerm (substFreeName (varname, y) absbody)
                        else
                            cAPP(afun, aterm)
                 | ( cABS( varlst, absbody), cTUPLE(elist)) =>
                        if length elist <> length varlst then
                            cAPP(afun, aterm)
                        else
                            let
                                fun substtuple( [], _, newvarlst, newelist, newt) =
                                            (rev newvarlst, rev newelist, newt)
                                  | substtuple(v::vs, e::es, newvarlst, newelist, newt) =
                                        if isSimple e orelse varOccurCnt(absbody,v)<=1 then
                                            substtuple(vs,es, newvarlst, newelist,
                                                       optimizeTerm(substFreeName (v,e) newt))
                                        else
                                            substtuple(vs,es, v::newvarlst, e::newelist, newt)
                                  | substtuple _ = raise Fail("substtuple")
                                val (newvarlst, newelst, newt) = substtuple(varlst, elist, [],[],absbody)
                            in
                                if newvarlst = [] then
                                    newt
                                else if length newelst = 1 then
                                    cAPP( cABS(newvarlst, newt), hd newelst)
                                else
                                    cAPP( cABS(newvarlst, newt), cTUPLE(newelst))
                            end
                 | ( cABS(varlst1, body1), cAPP(cABS(varlst2, cTUPLE(termlst)), t3)) =>
                        let fun cansubst (var, e) = isSimple e orelse varOccurCnt(body1,var)<=1
                        in
                            if length varlst1 = length termlst andalso
                               ListPair.all cansubst (varlst1,termlst) then
                                let fun substallvar([], term) = term
                                      | substallvar((varname,newterm)::l, term) =
                                            substallvar(l, substFreeName (varname,newterm) term)
                                    val newbody = substallvar(ListPair.zip(varlst1,termlst), body1)
                                in
                                    cAPP(cABS(varlst2, optimizeTerm newbody), t3)
                                end
                            else
                                cAPP(afun, aterm)
                        end
                 | ( cCASE(_, phraseList), cCTR(_, i)) => List.nth(phraseList,i)
                 | ( cCASE(_, phraseList), cAPP(cCTR(_,i),x)) => optApp(List.nth(phraseList,i),x)
                 | ( cDTR(_,i), cRECORD(_, phraseList)) => List.nth(phraseList,i)
                 | ( f, cTUPLE [])  => f
                 | ( cUNIT, _) => cUNIT
                 | _ => cAPP(afun, aterm)
        in
            mapTerm doOpt aTerm
        end (*optimizeTerm*)


end (*CoreTerm*)
