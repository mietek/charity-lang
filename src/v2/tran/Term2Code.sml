structure Term2Code =
struct
open Predefined
open TypedTerm
open VMCCode
open CharityData

structure TE  = TypeExpression
structure IST = ImperativeSymbolTable
structure TT  = CTermTransformation
exception DEBUG of ITerm

datatype StorageClass = sSTACK | sHEAP | sSTACK2

fun storageClassOf(TE.tARROW([],t)) = storageClassOf t
  | storageClassOf(TE.tNAME(aName,_)) =
        if aName = TE.intName then
            sSTACK
        else if aName=TE.realName orelse aName=TE.longName then
            sSTACK2
        else if aName = TE.stringName then
            sHEAP
        else
            (case IST.find aName
               of IST.sDATA(narg, ctrlst) =>
                    if narg=0 andalso (List.all (fn (_,srclst)=> (srclst=[])) ctrlst) then
                        sSTACK
                    else
                        sHEAP
                | IST.sCODATA _ => sHEAP
                | _ => raise Fail("storageClassOf " ^ aName)
            )
  | storageClassOf _ = sHEAP

fun CTerm2ITerm (aTerm:CTerm): ITerm =
    (case aTerm
       of cINT i => iINT i
        | cFLOAT s => iFLOAT s
        | cSTR s => iSTR s
        | cVAR v => iVAR v
        | cCTR(data,i) =>
            let val (narg, ctrlst) = IST.getDATA data
            in
                iCTR(List.nth(map #2 (typeOfCtrs(data,narg,ctrlst)),i),
                     i,
                     length ctrlst)
            end
        | cDTR(codata,i)=>
            let val (narg, dtrlst) = IST.getCODATA codata
            in
                iDTR(List.nth(map #2 (typeOfDtrs(codata,narg,dtrlst)), i),
                     i,
                     length dtrlst)
            end
        | cFUN fname => iFUN(IST.getFUNType fname, fname)
        | cTUPLE tl => iTUPLE(TE.newTVarList (length tl), map CTerm2ITerm tl)
        | cAPP(f,t) => iAPP(CTerm2ITerm f, CTerm2ITerm t)
        | cABS(varlst, t) => iABS(TE.tARROW(TE.newTVarList(length varlst), TE.newTVar()),
                                  varlst,
                                  CTerm2ITerm t)
        | cCASE(data, phlist) =>
            let val (narg, ctrlst) = IST.getDATA data
            in
                iCASE(genCaseTG(data,narg,ctrlst), map CTerm2ITerm phlist)
            end

        | cRECORD(codata,phlist) =>
            let val (narg, dtrlst) = IST.getCODATA codata
            in
                iRECORD(genRecordTG(codata,narg,dtrlst), map CTerm2ITerm phlist)
            end
        | _ => raise Fail("CTerm2ITerm")
    )

fun funLabel fname = "_" ^ fname

val RETB_LABEL = "_SIMPLERETB"
val RETP_LABEL = "_SIMPLERETP"
val RETU_LABEL = "_SIMPLERETU"

fun caseBranchLabel(caseid, branchid) = "@CASE" ^ Int.toString(caseid) ^ "$" ^ Int.toString(branchid)
fun caseEndLabel(caseid) = "@CASE" ^ Int.toString(caseid) ^ "END"
fun recDtrLabel(recid, dtrid) = "@REC" ^ Int.toString(recid) ^ "$" ^ Int.toString(dtrid)

fun SC2RetInstr sSTACK  = vRetB
  | SC2RetInstr sSTACK2 = vRetU
  | SC2RetInstr sHEAP   = vRetP

(* get the name of a polymorphic function *)
fun instanceName(name, []) = name
  | instanceName(name, typeList) =
    let fun gensuffix [] = ""
          | gensuffix (t::ts) =
                case storageClassOf t
                  of sSTACK  => "B"^(gensuffix ts)
                   | sSTACK2 => "U"^(gensuffix ts)
                   | sHEAP   => "P"^(gensuffix ts)
    in
        name ^ "$" ^ (gensuffix typeList)
    end

(* get the type and position of a var by looking up the stack *)
fun checkVar(aStack, aVarname) =
    let fun docheck([],bcnt,pcnt) = raise Fail("checkVar:"^aVarname)
          | docheck((vname,vtype)::ss, bcnt, pcnt) =
                if aVarname=vname then
                    case vtype
                      of sHEAP   => (sHEAP, pcnt)
                       | sSTACK  => (sSTACK, bcnt)
                       | sSTACK2 => (sSTACK2, bcnt)
                else
                    case vtype
                      of sHEAP   => docheck(ss, bcnt, pcnt+1)
                       | sSTACK  => docheck(ss, bcnt+1, pcnt)
                       | sSTACK2 => docheck(ss, bcnt+2, pcnt)
    in
        docheck(aStack, 0, 0)
    end


(* count the size of basic values and pointer values in a list of storageClass *)
fun countSCSize aSCList =
    let fun docnt([], bcnt, pcnt) = (bcnt, pcnt)
          | docnt(t::ts, bcnt, pcnt) =
                case t
                  of sHEAP => docnt(ts, bcnt, pcnt+1)
                   | sSTACK => docnt(ts, bcnt+1, pcnt)
                   | sSTACK2 => docnt(ts, bcnt+2, pcnt)
    in
        docnt(aSCList, 0, 0)
    end

fun genVMCCode(aFunName, aFunType, aFunBody:ITerm, aTypeSolution) =
    let
        (* get the storage class of a type, solve the type first *)
        fun getStorageClass te = storageClassOf(apply aTypeSolution te)

        fun countBP aTypeList = countSCSize (map getStorageClass aTypeList)

        fun dogen(stack, codeList, iINT i) = vConstI(i)::codeList
          | dogen(stack, codeList, iSTR s) = vNewstr(s)::codeList
          | dogen(stack, codeList, iFLOAT s) = vOther("constF " ^ s) ::codeList
          | dogen(stack, codeList, iVAR s) =
                    ( case checkVar(stack,s)
                        of (sSTACK,   pos) => vDupB(pos)::codeList
                         | (sHEAP, pos) => vDupP(pos)::codeList
                         | (sSTACK2,pos)=> vDupU(pos)::codeList
                    )
          | dogen(stack, codeList, iUNIT) = vNewtuple(0,0)::codeList
          | dogen(stack, codeList, iCTR(te,i,_)) =
                    let val (bcnt, pcnt) = countBP (srcList te)
                        val newcodelist = if srcCnt te >1 then vConstI(i)::vDetuple::codeList
                                                        else vConstI(i)::codeList
                    in
                        if getStorageClass(dstType te)=sHEAP then
                            vNewtuple(bcnt+1,pcnt)::newcodelist
                        else
                            newcodelist
                    end
          | dogen(stack, codeList, iDTR(te,dtrNo,dtrCnt)) =
                    if srcCnt te>1 then (* higher-order *)
                        if dtrCnt=1 then
                            vOther("xcall")::vDetuple::vDetuple::codeList
                        else
                            vOther("xcall")::vDetuple::vGetfieldP(dtrNo)::vDetuple::codeList
                    else
                        vOther("xcall")::vDetuple::vGetfieldP(dtrNo)::vDupP(0)::codeList
          | dogen(stack, codeList, iFUN(te, name)) =
                    let val tvars = map tVAR (getAllTVars te)
                        val actualtypes = map (apply aTypeSolution) tvars
                        val instName =instanceName(name, actualtypes)
                        val flabel= funLabel instName
                        val newcodelist = if srcCnt te > 1 then
                                                vDetuple::codeList
                                          else
                                                codeList
                    in
                        case IST.find instName
                          of IST.sFUN(_, cINLINE inlinecodelist) =>
                                List.revAppend(map vOther inlinecodelist, newcodelist)
                           | _ => vCall(flabel)::newcodelist
(*                        case Predefined.getInlineFunctionCode(instName)
                          of SOME inlinecode => List.revAppend(map vOther inlinecode, newcodelist)
                           | NONE => vCall(flabel)::newcodelist
*)
                    end
(*          | dogen(stack, codeList, iRECUR) =
                    vCall(funLabel aFunName)
                    ::
                    (if srcCnt aFunType>1 then
                         vDetuple::codeList
                     else
                         codeList
                    ) *)
          | dogen(stack, codeList, iTUPLE(tlist, tpl)) =
                    let
                        val (bcnt,pcnt) = countBP tlist
                        fun genlist(stk, cl, tl, []) = cl
                          | genlist(stk, cl, t::ts, x::xs) =
                                genlist(("", getStorageClass t)::stk,
                                        dogen(stk, cl, x),
                                        ts, xs)
                          | genlist _ = raise Fail("genlist")
                    in
                        vNewtuple(bcnt,pcnt)::genlist(stack, codeList, tlist, tpl)
                    end

          | dogen(stack, codeList, iAPP(x,y)) =
                    dogen(stack, dogen(stack, codeList, y),x)
          | dogen(stack, codeList, iABS(te, varlist, t)) =
                    let val vartypes = map getStorageClass (srcList te)
                        val newstack = List.revAppend (ListPair.zip(varlist, vartypes), stack)
                        val newcodelist = if length varlist>1 then
                                                dogen(newstack, vDetuple::codeList, t)
                                        else
                                                dogen(newstack, codeList, t)
                        val (bcnt, pcnt) = countBP(srcList te)
                        val rtype = getStorageClass(dstType te)
                        val movinstr = case rtype
                                        of sSTACK => vMoveB(0, bcnt)
                                        | sHEAP => vMoveP(0, pcnt)
                                        | sSTACK2 => vMoveU(0, bcnt)
                    in
                        vPopP(pcnt)::vPopB(bcnt)::movinstr::newcodelist
                    end
        | dogen(stack, codeList, iCASE(tg, phraselist)) =
                    let val caseID =newID()
                        val caseLabels = List.tabulate(length phraselist, fn i=> caseBranchLabel(caseID,i))
                        val newcodeList = if getStorageClass(srcType (mainTypeOfTG tg)) = sHEAP then
                                                vCase(caseLabels)::vDetuple::codeList
                                        else
                                                vCase(caseLabels)::codeList
                        fun gencaseph([], _, _, codeL) = codeL
                          | gencaseph(ph::phs, pht::phts, n, codeL) =
                                let
                                    val thelabel = vLabel(caseBranchLabel(caseID,n))
                                    val gotoinstr = vGoto(caseEndLabel caseID)
                                    val newcodeL = if srcCnt pht>1 then
                                                        let val (bcnt,pcnt)= countBP(srcList pht)
                                                        in
                                                            vNewtuple(bcnt,pcnt)
                                                            ::thelabel::codeL
                                                        end
                                                else
                                                        thelabel::codeL
                                in
                                    gencaseph(phs,phts, n+1, gotoinstr::(dogen(stack, newcodeL, ph)))
                                end
                        | gencaseph _ = raise Fail("gencaseph")
                    in
                        vLabel(caseEndLabel caseID)::gencaseph(phraselist, subTypesOfTG tg, 0, newcodeList)
                    end

        | dogen(stack, codeList, iRECORD(tg, phraselist)) =
                let
                    fun genPushVars(stk, bcnt, pcnt, [], codeL) = (bcnt, pcnt, codeL)
                      | genPushVars(stk, bcnt, pcnt, var::varlist, codeL)=
                            let val (vartype, newb, newp, newinstr) =
                                ( case checkVar(stk,var)
                                    of (sSTACK, pos) =>(sSTACK, bcnt+1, pcnt, vDupB(pos))
                                     | (sHEAP, pos) => (sHEAP,  bcnt, pcnt+1, vDupP(pos))
                                     | (sSTACK2,pos)=> (sSTACK2,bcnt+2, pcnt, vDupU(pos))
                                )
                            in
                                genPushVars((var,vartype)::stk, newb, newp, varlist, newinstr::codeL)
                            end


                    (* the R rule in thesis *)
                    fun genClosureCode(aFreeVarList, aTerm:ITerm, aTermType, aLabel, aDtrID) =
                        let
                            val env = map (fn varname=> (varname, #1 (checkVar(stack, varname)))) aFreeVarList
                            val (newenv, newterm) =
                                case aTerm
                                  of iABS(te, localvars, term) =>
                                        (ListPair.zip(localvars, map getStorageClass (srcList te)),
                                        term)
                                   | x => ([],x)
                            val codelist = dogen(("",sSTACK)::(rev (newenv @ env)),
                                                [vLabel(aLabel)], newterm)
                            val (bcnt,pcnt) = countSCSize(map #2 (newenv @env))
                            val resultClass = getStorageClass (dstType aTermType)
                        in
                            if srcCnt aTermType>0 then (* higher-order, simply return *)
                                (case resultClass
                                   of sSTACK => vRetB(bcnt, pcnt)
                                    | sSTACK2 => vRetU(bcnt, pcnt)
                                    | sHEAP  => vRetP(bcnt, pcnt)
                                )::codelist
                            else (* normal order, need to update the record *)
                                let val cleanupcode =
                                    (case resultClass
                                        of sSTACK  => [ vDupP(pcnt), (*the record *)
                                                        vDupB(0),
                                                        vConstA(RETB_LABEL),
                                                        vNewtuple(2,0),
                                                        vSetfieldP(aDtrID),
                                                        vRetB(bcnt, pcnt+1) ]

                                         | sSTACK2 => [ vDupP(pcnt), (* the record *)
                                                        vDupU(0),
                                                        vConstA(RETU_LABEL),
                                                        vNewtuple(3,0),
                                                        vSetfieldP(aDtrID),
                                                        vRetU(bcnt, pcnt+1) ]
                                         | sHEAP   => [ vDupP(pcnt+1), (* the record *)
                                                        vDupP(1),      (* the result *)
                                                        vConstA(RETP_LABEL),
                                                        vNewtuple(1,1),
                                                        vSetfieldP(aDtrID),
                                                        vRetP(bcnt, pcnt+1) ]
                                    )
                                in
                                    List.revAppend(cleanupcode, codelist)
                                end
                        end
                    val recordID = newID();
                    (* generate closures in the reverse order so that the ith destructor
                    corresponds to the ith field *)
                    fun genRecPhrases(stk, dtrid, [], phts, codeL) = codeL
                      | genRecPhrases(stk, dtrid, ph::phs, pht::phts, codeL) =
                        let val freevars = removeDup (getITermFreeVars ph)
                            val codelabel = recDtrLabel(recordID, dtrid)
                            val closurecode = genClosureCode(freevars, ph, pht, codelabel, dtrid)
                            val (bcnt,pcnt, newcodeL) = genPushVars(stk, 0,0,freevars, codeL)
                        in
                            genRecPhrases(("", sHEAP)::stk, dtrid-1, phs, phts,
                                (vNewtuple(bcnt+1,pcnt)::vConstA(codelabel)::newcodeL) @ closurecode)
                        end
                      | genRecPhrases _ = raise Fail("genRecPhrases")

                    val phraseCode = genRecPhrases(stack, length phraselist-1,
                                                rev phraselist, rev (subTypesOfTG tg), codeList)
                in
                    if length phraselist = 1 andalso (srcCnt (hd (subTypesOfTG tg))>0 ) then
                        (* higher order with only one destructor *)
                        phraseCode
                    else
                        vNewtuple(0, length phraselist)::phraseCode
                end
        (*| dogen (stack, codeList, x) = raise DEBUG(x) *)

        val (theInput, theTerm) =
            case aFunBody
                of iABS(absType, varList, absTerm) =>
                    let val varclasses = map getStorageClass (srcList absType)
                        val stk = ListPair.zip(varList,varclasses)
                    in
                        (rev (ListPair.zip(varList,varclasses)), absTerm)
                    end
                | _ => ( [], aFunBody)

        val retinstr = SC2RetInstr(getStorageClass (dstType aFunType))
        val (bcnt,pcnt) = countSCSize(map #2 theInput)
    in
            retinstr(bcnt,pcnt)
        ::dogen(("",sSTACK)::theInput,[vLabel(funLabel aFunName)], theTerm)
    end

fun getDependences(aFunBody, aTypeSolu) =
    let
        fun isCall (iFUN(ftype,fname)) = true
          | isCall _ =false
        val calllist = findITerm isCall aFunBody
        fun f(iFUN(funType, fname)) =
                let val tvars = map tVAR (getAllTVars funType)
                    val instType = map (apply aTypeSolu) tvars
                in
                    (instanceName(fname, instType),
                     fname,
                     apply aTypeSolu funType)
                end
          | f _ = raise Fail("getDependences")
    in
        map f calllist
    end
    fun maxDisplayLevel(aType) =
        let
            fun getlevel (TE.tNAME(aTypeName, arglst)) =
                let
                    fun getLevelList x =  map getlevel (map (substTVar1toN arglst) x)
                in
                    case IST.find aTypeName
                      of IST.sCODATA(narg, dtrlst) =>
                            let
                                val struDstTypeList = map #2 dtrlst
                            in
                                if TE.existTVar0InList struDstTypeList then
                                    9999
                                else
                                    1 + getMaxItem(0, getLevelList struDstTypeList)
                            end
                       | IST.sDATA(narg,ctrlst) =>
                            let
                                fun f(_,[x]) = x
                                  | f(_, tl) = TE.tPRODS tl
                            in
                                getMaxItem(0, getLevelList (map f ctrlst))
                            end
                       | _ => 0
                end
            | getlevel(TE.tPRODS tl) = getMaxItem(0, map getlevel tl)
            | getlevel(TE.tARROW(tl,t)) = getMaxItem(0, map getlevel (t::tl))
            | getlevel _  = 0
        in
            getlevel aType
        end
    fun simplifyTerm aCTerm =
        let 
            val b1 = optimizeTerm(TT.deComb aCTerm) handle ex=> (print "deComb"; raise ex)
            val b2 = optimizeTerm(TT.lambdaLift b1)
                       handle ex => (print "lambdaLift"; raise ex)
            val b3 = optimizeTerm(TT.deLetrec b2)
                       handle ex => (print "deletrec"; raise ex)
        in
            b3
        end
    fun genall(result, [] ) = result
      | genall(result, (instname, fname, insttype)::xs ) =
            if Map.find(instname,result)<>NONE then (* already generated*)
                genall(result, xs)
            else
                (case IST.getFUNBody(fname)
                  of cINLINE _ => genall(result,xs) (* inline function*)
                   | funbody =>
                    let val fbody = renewTVarsInTerm(CTerm2ITerm (simplifyTerm funbody))
                        val newinsttype=renewTVars insttype
                        val solu = solveEquSet(genTypeEquations(fbody, newinsttype))
                                handle TYPECHECK(msg)=> raise Fail("genall for " ^ instname ^":\n"
                                                                    ^DomainToStr(newinsttype)^msg)
                        val R =  genVMCCode (instname, newinsttype, fbody, solu)
                                    handle x => (print("Error in "^instname); raise x)
                        val R2 = optiCodeSeq (rev R)
                        val dependences = getDependences(fbody, solu)
                    in
                        genall((instname, R2)::result, dependences @ xs)
                    end
                )

local

in
    fun generateAllCode(rootFunName) =
        let val roottype = IST.getFUNType(rootFunName)
            val rootDstType = TE.dstType roottype
            val displayLevel = maxDisplayLevel(rootDstType)
            val tostrName = "$tostr"
            val tostrType = TE.tARROW([rootDstType, intTE], stringTE)
            val printQ = cABS(["_","$n"], cSTR "?")
            val Qlist = List.tabulate(20, fn _ => printQ)
            val tostrBody = cABS(["$dd","$nn"],
                                   cAPP(genTostrEi(Qlist, rootDstType),
                                        cTUPLE([ cVAR "$dd", cVAR "$nn" ])
                                       )
                                )
            val _ = IST.newFUN(tostrName, tostrType, tostrBody)
            val rootinstname = instanceName(rootFunName, map TE.tVAR (getAllTVars roottype))
            val dispLabel = case storageClassOf rootDstType
                              of sHEAP => "_$dispP"
                               | sSTACK => "_$dispB"
                               | sSTACK2 => "_$dispU"
        in
            genall(
                [ ("$startUp",
                    [ vLabel("_$startUp"),
                        vCall(funLabel rootinstname),
                        vConstI(displayLevel),
                        vConstA(funLabel tostrName),
                        vCall(dispLabel),
                        vPopB(1),
                        vOther("halt")])
                ],
                [(rootinstname, rootFunName, roottype),
                    (tostrName, tostrName, tostrType)]
                )
        end
end
end
