    (**********************************************************************
     *                                                                    *
     *    Transform Abstract Syntax Tree into CoreTerm                    *
     *                                                                    *
     **********************************************************************)

structure AST2CT =
struct

    structure TE=TypeExpression
    structure IST = ImperativeSymbolTable
    structure UT= Utility
    structure ES= ErrorSet

    open AST
    open CoreTerm
    open Predefined
    open CharityData

fun newVarNames n = List.tabulate(n, fn _=> "$dp"^UT.newName())
(*translate a patterned abstraction into non-patterned *)
fun transPABS([]: (CPattern list * CTerm) list, []:CTerm list):CTerm = cINLINE["incomplete"]
  | transPABS(aPABS, []) =
    (* end of translation *)
    let
        (* replace cINLINE in a CTerm, however, only those
           cINLINE that appears as the result of the whole
           term is replaced
         *)
        fun repR(newterm, aTerm) =
            (case aTerm
               of cINLINE ["incomplete"] => newterm
                | cAPP(f,t) => cAPP(repR(newterm, f), t)
                | cTUPLE [x] => repR(newterm, x)
                | cABS(vs, t) => cABS(vs, repR(newterm,t))
                | cCASE(d, pl) => cCASE(d, map (fn t=>repR(newterm,t)) pl)
                | x => x
            )
        (* the patterns are redundant there are multiple phrases in aPABS
           and some of them(except the last one) is complete
           this may not be right, leave for future
        val isRedundant = List.exists (fn (_,term) => findErrors term = [])
                                      (tl aPABS)
         *)
        fun fatten [] = cINLINE []
          | fatten ((_,t)::ts) = repR(fatten ts, t)


    in
        fatten(aPABS)
(*        if isRedundant then
            (ES.newErr("some patterns are redundant"); result)
        else
            result *)
    end
  | transPABS(aPABS, aDiscriminant::aDiscrimList) =
    let
        val hdpattList = map hd (map #1 aPABS)

        fun findCtr([]) = ""
          | findCtr(cpCTR(dname,_,_)::_) = dname
          | findCtr(_::ps) = findCtr ps

        val dataname = findCtr hdpattList

        fun findRec([]) = ""
          | findRec(cpRECORD(dname,_)::_) = dname
          | findRec(_::ps) = findRec ps

        val codataname = findRec hdpattList

        fun findTuple [] = ~1
          | findTuple (cpTUPLE tl::_) = length tl
          | findTuple (_::ps) = findTuple ps

        val tuplelen = findTuple hdpattList

        fun isInt(cpINTRANGE _) = true
        | isInt _ = false

    in
        if dataname<>"" then
            (*if there is at least one constructor pattern*)
            let
                (* (ctrtypes, id) list *)
                val ctrList = UT.enum0toN (map #2 (IST.getDATACtrList dataname))
                (*convert the patterned abstraction, filter out those abs that don't
                  corresponding the corresponding constructor
                  The result is also a PABS
                 *)
                fun transCTR(_, _, []) = []
                  | transCTR(ctrid, dontcares, pabs::ps) =
                    let val rest = transCTR(ctrid, dontcares, ps)
                    in
                        case pabs
                          of (cpCTR(_,id,ctrpats)::patlst, term) =>
                                if id=ctrid then
                                    (ctrpats @ patlst, term)::rest
                                else
                                    rest
                           | ((cpVAR v)::patlst, term) =>
                                (dontcares @ patlst, substFreeName (v, aDiscriminant) term)
                                ::rest
                           | _ => raise Fail("transCTR")
                    end
                fun f(ctrtype,ctrid) =
                    let val cnt = length ctrtype
                        val newvars = newVarNames cnt
                        val newterms = map (fn v=> cVAR v) newvars
                        val dontcares = List.tabulate(cnt, fn _ => cpVAR "_")
                    in
                        cABS(newvars,
                             transPABS(transCTR(ctrid, dontcares, aPABS), newterms @ aDiscrimList)
                            )
                    end
            in
                cAPP(cCASE(dataname, map f ctrList), aDiscriminant)
            end
        else if codataname<>"" then
            (* if there is at least one record pattern *)
            let
                (* (dtrtype, id) list *)
                val dtrTypes = map #2 (IST.getCODATADtrList codataname)
                val dtrList = UT.enum0toN dtrTypes
                (*val HOdtrs = List.filter (fn t => TE.srcCnt t > 0) dtrTypes*)
                val LOdtrs = List.filter (fn (t,_) => TE.srcCnt t = 0) dtrList
                val newdiscrim = map (fn (t, i) => cAPP(cDTR(codataname,i), aDiscriminant)) LOdtrs

                val dontcares = List.tabulate(length LOdtrs, fn _ => cpVAR "_")
                fun getLOPatts([]) = []
                  | getLOPatts((cpFUN _)::ps) = getLOPatts ps
                  | getLOPatts( p::ps ) = p::getLOPatts ps
                fun substHOPatts([],_,_, term) = term
                  | substHOPatts(t::ts,i, pat::pats, term) =
                        if TE.srcCnt t >0 then (*higher-order destructor*)
                            let val newvars = newVarNames(TE.srcCnt t)
                                val newf = cABS(newvars,
                                                cAPP(cDTR(codataname,i),
                                                     cTUPLE( (map (fn v=>cVAR v) newvars) @ [aDiscriminant])
                                                    )
                                               )
                            in
                                case pat
                                  of cpFUN f => substHOPatts(ts,i+1,pats, substFreeName (f,newf) term)
                                   | cpVAR"_" => substHOPatts(ts,i+1,pats, term)
                                   | cpVAR f => substHOPatts(ts,i+1,pats, substFreeName (f,newf) term)
                                   | _ => raise Fail("substHOPatts")
                            end
                        else
                            substHOPatts(ts,i+1,pats,term)
                  | substHOPatts _ = raise Fail("substHOPatts2")

                fun transREC([]) = []
                  | transREC(pabs::ps) =
                    let val rest = transREC ps
                    in
                        case pabs
                          of (cpRECORD(_, recpatts)::patts, term) =>
                                ((getLOPatts recpatts) @ patts, substHOPatts(dtrTypes, 0, recpatts, term))
                                ::rest
                           | (cpVAR v ::patts, term) =>
                                (dontcares @ patts, substFreeName (v, aDiscriminant) term) ::rest
                           | _ => raise Fail("transREC")
                    end
            in
                transPABS(transREC aPABS, newdiscrim @ aDiscrimList)
            end

        else if tuplelen>= 0 then
            (* if there is at least a tuple pattern *)
            let val newvars = newVarNames tuplelen
                val newvarterms = map (fn v=>cVAR v) newvars
                val dontcares = List.tabulate(tuplelen, fn _ => cpVAR "_")

                fun transTUP [] = []
                  | transTUP(pabs::ps) =
                        (case pabs
                          of (cpTUPLE tpl::patts, term) =>
                                (tpl @ patts, term)
                           | (cpVAR v::patts, term) =>
                                (dontcares @ patts, substFreeName (v,aDiscriminant) term)
                           | _ => raise Fail("tuplelen")
                        )::transTUP ps
            in
                cAPP(cABS(newvars, transPABS(transTUP aPABS, newvarterms @ aDiscrimList)), aDiscriminant)
            end
        else if List.exists isInt hdpattList then
            raise Fail("int pattern not implementated")
        else
            (* all the leading patterns are variable patterns *)
            let
                fun dosubst((cpVAR v)::patlst, term) =
                        (patlst, substFreeName (v, aDiscriminant) term)
                  | dosubst _ = raise Fail("dosubst in match")
            in
                transPABS(map dosubst aPABS, aDiscrimList)
            end

    end

(*recursively remove all patterned-abs in a term *)
fun dePattern(aTerm:CTerm):CTerm =
    let fun f (cPattABS(patabslst)) =
            let val newvars = newVarNames (length (#1 (hd patabslst)))
            in
                cABS(newvars,
                    transPABS(patabslst, map (fn v=>cVAR v) newvars)
                    )
            end
          | f x = x
    in
        mapTerm f aTerm
    end

    (* convert a YTypeExp to TypeExp, assuming the YTypeExp is well-formed *)
    fun YTE2TE(aTVarMap: (string * int) list,
               aYTE: YTypeExp
              ): TE.TypeExp =
        let fun cvt x= YTE2TE(aTVarMap, x)
        in
            case aYTE
              of yUNIT => TE.tUNIT
               | yPRODS ytelist => TE.tPRODS(map cvt ytelist)
               | yNAME(name,[]) =>
                       (case Map.find(name, aTVarMap)
                          of SOME i => TE.tVAR i
                           | NONE => TE.tNAME(name,[])
                       )
               | yNAME(name,ytelist) =>
                       TE.tNAME(name, map cvt ytelist)
               | yARROW(ytelist,yte) =>
                       TE.tARROW(map cvt ytelist, cvt yte)
        end


    (* do a primitive check of a pattern to make sure that:
       1. any record pattern contains destructors that belong to
          the same co-inductive datatype
       2. any constructor in a constructor pattern is really a constructor
       3. a constructor is supplied with the same number of arguments as its type specified
       4. There is no duplicate variables
       IST is an implicit argument. No typechecking is done.
       return [] if the check is successful and the pattern is considered
       well-formed, otherwise return a list of error messages

    fun checkPattern(aPattern: YPattern): string list =
        let
            fun checkDtrList dtrlst =
                let
                    val sym = IST.find (hd dtrlst)
                in
                    if not (IST.isDTR sym) then
                        [ "'" ^ hd dtrlst ^ "' is not a destructor" ]
                    else
                        let
                            val dataname = IST.dataOf sym
                            val struList = map #1 (IST.dtrsOf(IST.find dataname))
                        in
                            map (fn aName => "'" ^ aName ^ "' is not a destructor for datatype " ^ dataname)
                                (Set.minus(dtrlst, struList))
                            @
                            (map (fn x=> "duplicate destructor '" ^ x ^ "' in record pattern")
                                 (UT.findDup dtrlst))
                        end
                end
            fun docheck(ypCTR(ctrname, p)) =
                    let val sym = IST.find ctrname
                    in
                        if IST.isCTR sym then
                            let val srccnt = TE.srcCnt(IST.typeOf sym)
                                val argsize = (case p
                                                of ypTUPLE tl => length tl
                                                 | _ => 1
                                              )
                            in
                                if srccnt<>1 andalso srccnt<> argsize then
                                    ("wrong number of argument for pattern '" ^ ctrname ^ "'")::(docheck p)
                                else
                                    docheck p
                            end
                        else
                            ("'" ^ ctrname ^ "' is not a constructor")::(docheck p)
                    end
              | docheck(ypVAR(varname)) =
                    let val sym = IST.find varname
                    in
                        if IST.isCTR sym andalso TE.srcCnt(IST.typeOf sym)<>0 then
                            ["missing arguments for constructor pattern '" ^ varname ^ "'" ]
                        else
                            []
                    end
              | docheck(ypTUPLE pl) = docheckList pl
              | docheck(ypRECORD dtrPairList) =
                    checkDtrList (map #1 dtrPairList)
                    @
                    docheckList(map #2 dtrPairList)
              | docheck _ = []
            and docheckList [] = []
              | docheckList(x::xs) = (docheck x) @ (docheckList xs)

        in
            (docheck aPattern) @
            map (fn x=> "duplicate variable '" ^ x ^ "' in pattern")
                (UT.findDup (getYPatternVars aPattern))
        end
    fun checkPatternList([]) = []
      | checkPatternList(p::ps) = (checkPattern p) @ (checkPatternList ps)
    *)

    (*check a list of constructors, make sure they all belong to
      the same datatype and there is no dupliate or missing constructors.
     *)
    fun isInvalidCtrList(aCtrList:string list):bool =
        (case IST.find (hd aCtrList)
           of IST.sCTR(_,dataname,_) =>
                let
                    val struList = map #1 (IST.getDATACtrList dataname)
                    val errs =
                        map (fn n => "'" ^ n ^ "' is not a constructor of " ^ dataname)
                            (Set.minus(aCtrList, struList))
                        @
                        map (fn n => "missing constructor '" ^ n ^ "'")
                            (Set.minus(struList, aCtrList))
                        @
                        (map (fn n=> "duplicate constructor '" ^ n ^ "'")
                            (UT.findDup aCtrList))

                in
                    ES.newErrs(errs);
                    errs<>[]
                end
            | _ =>  (ES.newErr( "'" ^ hd aCtrList ^ "' is not a constructor" ); true)
        )
    (*check a list of destructors, make sure they all belong to
      the same datatype and there is no dupliate or missing constructors.
      generate a list of error messages if errors found
     *)
    fun isInvalidDtrList aDtrList =
        (case IST.find (hd aDtrList)
           of IST.sDTR(_,dataname,_) =>
                let
                    val struList = map #1 (IST.getCODATADtrList dataname)
                    val errs =
                        map (fn aName => "'" ^ aName ^ "' is not a destructor for datatype " ^ dataname)
                            (Set.minus(aDtrList, struList))
                        @
                        map (fn aName => "missing destructor '" ^ aName ^ "'")
                            (Set.minus(struList, aDtrList))
                        @
                        (map (fn x=> "duplicate destructor '" ^ x ^ "'")
                            (UT.findDup aDtrList))
                in
                    ES.newErrs(errs);
                    errs<>[]
                end
            | _ =>  (ES.newErr("'" ^ hd aDtrList ^ "' is not a destructor"); true)
        )

    (* Collect all type variables in a YTypeExp
       return a list of type variable names
    *)
    fun getYTEVars(aYTE: YTypeExp):string list =
        (case aYTE
           of yUNIT           => []
            | yPRODS telist   => getYTEVarsInList telist
            | yNAME(name, []) =>
                (case IST.find name
                   of IST.sDATA _   => []
                    | IST.sCODATA _ => []
                    | IST.sALIAS _  => []
                    | _ => [ name ]
                )
            | yNAME(name, ytlist)=> getYTEVarsInList ytlist
            | yARROW(ytlist, yt) => getYTEVarsInList (yt::ytlist)
        )
    and getYTEVarsInList [] = []
      | getYTEVarsInList(x::xs) = getYTEVars(x) @ getYTEVarsInList(xs)
    (* check a YTypeExp to make sure that
       1. All type variables are defined in aTypeVarList
       2. Reference to another datatype has correct number of type arguments
       return [] if the check is successful, otherwise return a list of error messages
     *)
    fun checkYTE(aTypeVarList: string list, aYTE: YTypeExp): string list =
        (case aYTE
           of yUNIT => []
            | yPRODS ytelst => checkYTEList(aTypeVarList, ytelst)
            | yNAME(name, ytelist) =>
                if Set.isMember(name, aTypeVarList) then
                    if ytelist=[] then
                        []
                    else
                        [ "Type variable " ^ name ^ " can not have argument" ]
                else
                    let
                        fun checkarg n =
                            if  n = length ytelist then
                                checkYTEList(aTypeVarList, ytelist)
                            else
                                [ "Incorrect number of type variables for type "^ name ]
                    in
                        case IST.find name
                          of IST.sDATA(argcnt, _) => checkarg argcnt
                           | IST.sCODATA(argcnt,_) => checkarg argcnt
                           | _ => [ "Undefined type variable " ^ name ]
                    end
            | yARROW(ytelist, yte) => checkYTEList(aTypeVarList, yte::ytelist)
        )
    and checkYTEList(aTypeVarList: string list, aYTEList: YTypeExp list): string list =
            (case aYTEList
               of [] => []
                | yte::ytelst =>
                        checkYTE(aTypeVarList, yte) @ checkYTEList(aTypeVarList, ytelst)
            )

    local
        fun checknameclash( [] ) = []
          | checknameclash( n::ns) =
                if IST.find n <> IST.sNONE then
                    ("name clash: '" ^ n ^ "'")::(checknameclash ns)
                else
                    (checknameclash ns)

        fun checknames( aDataName, aArgList, aAliasName, aStruList:string list ) =
            if Set.isMember(aAliasName, aArgList) then
                [ " alias name clash with arguments of datatype " ^ aDataName ]
            else if (UT.findDup aArgList)<>[] then
                [ " duplicate argument names of datatype " ^ aDataName ]
            else if Set.isMember(aDataName, aStruList) then
                [ " datatype name clash with structor name: "^ aDataName ]
            else if UT.findDup aStruList <> [] then
                [ " duplicate structor names in datatype " ^ aDataName ]
            else
                checknameclash(aDataName::aStruList)
    in
        (* check an alias definition to make sure that:
            1. the alias name is not defined elsewhere
            2. the arguments of the alias are distinct
            3. the type expression is valid
            return [] if the check is successfuly, otherwise
            return a list of error messages
        *)
        fun checkAliasDef((aAliasName: string, aArgList: string list),
                           aYTE: YTypeExp
                         ):string list =
            checknameclash([aAliasName])
            @
            (if(UT.findDup aArgList)<>[] then
                [ "duplicate argument names of alias " ^ aAliasName ]
             else
                checkYTE(aArgList, aYTE)
            )

        (* check an datatype definition(inductive and co-inductive) to make sure that:
            1. the datatype name is not defined elsewhere
            2. the arguments of the datatype are distinct
            3. the names of the constructors/destructors are distinct and undefined
            4. the type expressions associated with each structor are valid
        *)
        fun checkDataDef((aDataName:string, aArgList:string list),
                         aAliasName:string,
                         aStruList: (string * YTypeExp list) list
                        ): string list =
            checknames(aDataName, aArgList, aAliasName, map #1 aStruList)
            @
            checkYTEList(aAliasName::aArgList,
                         UT.flattenList(map #2 aStruList))

        fun checkCodataDef((aDataName:string, aArgList:string list),
                            aAliasName:string,
                            aStruList: (string * YTypeExp) list
                          ): string list =
            checknames(aDataName, aArgList, aAliasName, map #1 aStruList)
            @
            checkYTEList(aAliasName::aArgList,
                         map #2 aStruList)
    end

    (*decompose a pattern, return a list of context maps ,
      also generate type equations imperatively
     *)
    fun decomposePattern( aType: TE.TypeExp, aPattern: CPattern) =
            (case aPattern
               of cpVAR v => [ (v, aType) ]
                | cpFUN f => [ (f, aType) ]
                | cpCTR(dataname, i, pattlist) =>
                        let val (narg,ctrlst) = IST.getDATA dataname
                            val ctrtype = TE.renewTVars(typeOfNthCtr((dataname,narg,ctrlst),i))
                        in
                            TE.newEqu (aType, TE.dstType ctrtype);
                            decomposePatternList(TE.srcList ctrtype, pattlist)
                        end
                | cpTUPLE pattlist =>
                        let val tvarlst = TE.newTVarList(length pattlist)
                        in
                            TE.newEqu (aType, TE.tPRODS tvarlst);
                            decomposePatternList(tvarlst, pattlist)
                        end
                | cpRECORD(dataname, recphraseList) =>
                        let
                            val (argcnt, dtrlst) = IST.getCODATA(dataname)
                            val (T, phtypes) = TE.renewTVarsInTG (genRecordTG(dataname, argcnt, dtrlst))
                        in
                            TE.newEqu(aType, T);
                            decomposePatternList(phtypes,
                                                 recphraseList)
                        end
                | _ => []
            )(*end case aPattern*)
    and decomposePatternList([], _) = []
      | decomposePatternList(aType::ts, aPatt::ps) =
                decomposePattern(aType, aPatt) @ decomposePatternList(ts, ps)
      | decomposePatternList _ = raise Fail("decomposePatternList in AST2CT.sml")

    (*decompose a CTerm, generate type equations imperatively
      contextMap is a (name,type) pairs list, macros are
      named as "$1", "$2",... respectively
     *)
    fun decompTerm(contextMap, aType, aTerm) =
        (case aTerm
           of cINT _   => TE.newEqu (aType, TE.intTE)
            | cFLOAT _ => TE.newEqu (aType, TE.realTE)
            | cSTR _   => TE.newEqu (aType, TE.stringTE)
            | cUNIT    => TE.newEqu (aType, TE.tUNIT)
            | cCTR(dataname,i) =>
                    let val (tvarcnt,ctrlst) = IST.getDATA dataname
                        val ctrtype = typeOfNthCtr((dataname,tvarcnt,ctrlst),i)
                    in
                        TE.newEqu (aType, TE.renewTVars ctrtype)
                    end
            | cDTR(dataname,i) =>
                    let val (tvarcnt,dtrlst) = IST.getCODATA dataname
                        val dtrtype = typeOfNthDtr((dataname,tvarcnt,dtrlst),i)
                    in
                        TE.newEqu (aType, TE.renewTVars dtrtype)
                    end
            | cVAR v   => (TE.newEqu (aType, Map.getValue(v,contextMap))
                            handle ex => (print ("decomp cVAR" ^v); raise ex))
            | cAPP(f,t)=>
                    let val T= TE.newTVar()
                    in
                        decompTerm(contextMap, T, t);
                        decompTerm(contextMap, TE.tARROW([T],aType), f)
                    end
            | cTUPLE tl =>
                    let
                        val tvarlst = TE.newTVarList(length tl)
                    in
                        TE.newEqu (aType, TE.tPRODS tvarlst);
                        decompTermList(contextMap, tvarlst, tl)
                    end
            | cPattABS caselist =>
                    let
                        (* each case phrase is of type S1,S2,..,Sn -> T *)
                        val S = TE.newTVarList(length (#1 (hd caselist)))
                        val T = TE.newTVar()
                        fun docaselist [] = ()
                          | docaselist((pattl,term)::ps)=
                            let
                                val newcontext = decomposePatternList (S, pattl)
                            in
                                decompTerm(newcontext@contextMap, T, term);
                                docaselist ps
                            end
                    in
                        TE.newEqu (aType, TE.tARROW(S,T));
                        docaselist caselist
                    end
            | cRECORD(dataname, reclist) =>
                    let
                        val (argcnt, dtrlst) = IST.getCODATA(dataname)
                        val (T, phtypes) = TE.renewTVarsInTG (genRecordTG(dataname,argcnt,dtrlst))
                    in
                        TE.newEqu (aType, T);
                        decompTermList(contextMap, phtypes, reclist)
                    end
            | cCOMB(combname, maclist) =>
                    let
                        val (combtype,mactypes) = TE.renewTVarsInTG(#1 (IST.getCOMB combname))
                    in
                        TE.newEqu (aType, combtype);
                        decompTermList(contextMap, mactypes, maclist)
                    end
            | cFUN(funname) =>
                    (case Map.find(funname, contextMap)
                       of SOME t => TE.newEqu (aType, t)
                        | NONE => TE.newEqu (aType, TE.renewTVars (IST.getFUNType funname))
                    )
            | cMACRO(i) => TE.newEqu (aType, Map.getValue("$"^ Int.toString(i), contextMap))
            | cLETREC(letlst, term) =>
                let val tvarlst = TE.newTVarList (length letlst)
                    val newcontext = ListPair.zip(map #1 letlst, tvarlst) @ contextMap
                in
                    decompTerm(newcontext, aType, term);
                    decompTermList(newcontext, tvarlst, map #3 letlst)
                end
            | cABS(varlst, term) =>
                    let val tvarlst = TE.newTVarList(length varlst)
                        val T = TE.newTVar()
                    in
                        TE.newEqu (aType, TE.tARROW(tvarlst, T));
                        decompTerm(ListPair.zip(varlst,tvarlst) @ contextMap, T, term)
                    end
            | cCASE(dataname, phlist) =>
                    let val (argcnt, ctrlst) = IST.getDATA(dataname)
                        val (casetype, phtypes) = TE.renewTVarsInTG (genCaseTG(dataname,argcnt,ctrlst))
                    in
                        TE.newEqu (aType, casetype);
                        decompTermList(contextMap, phtypes, phlist)
                    end
            | cINLINE _ => raise Fail("decompTerm(_,_,cINLINE _)")
        ) (*end case aTerm*)
    and decompTermList(contextMap, [], _) = ()
      | decompTermList(contextMap, aType::types, aTerm::terms) =
                (decompTerm(contextMap, aType, aTerm);
                 decompTermList(contextMap, types, terms)
                )
      | decompTermList _ = raise Fail("decompTermList")



    fun printlist [] = ()
      | printlist (msg::msgs) = (print msg; print "\n"; printlist msgs)

    (* translate an alias definition and put it into IST
       if there are errors, print them and return false
       otherwise return true
     *)
    fun parseAliasDef(aAliasDef: YAliasDef):bool =
        let
            val ((name:string , arglist: string list), dom: YTypeExp) = aAliasDef
            val errmsg = checkAliasDef aAliasDef
            val tvarmap = UT.from0toN (fn (x,i)=>(x,i)) arglist
        in
            if errmsg<>[] then
                (print ("parsing alias '" ^ name ^ "' definition error:\n");
                 printlist errmsg;
                 false
                )
            else
                (IST.newALIAS (name, length arglist, YTE2TE(tvarmap, dom));
                 true)
        end

    (* translate an inductive data definition and put it into IST
       if there are errors, print them and return false
       otherwise return true
     *)
    fun parseDataDef(aDataDef: YDataDef):bool =
        let
            val ((aDataName:string, aArgList:string list),
                 aAliasName:string,
                 aStruList: (string * YTypeExp list) list
                ) = aDataDef
            val errmsg = checkDataDef aDataDef
            val tvarmap = UT.from0toN (fn (x,i)=>(x,i)) (aAliasName::aArgList)
            fun tote yte = YTE2TE(tvarmap, yte)
        in
            if errmsg<>[] then
                (print ("parsing datatype '" ^ aDataName ^ "' definition error:\n");
                 printlist errmsg;
                 false
                )
            else let val dt = (aDataName,
                               length aArgList,
                               map (fn (ctrname,ctrtypes) => (ctrname, map tote ctrtypes))
                                   aStruList
                              )
                 in
                    (IST.newDATA dt;
                     IST.newCOMB (genFold dt);
                     IST.newCOMB (genMap dt);
                     IST.newCOMB (genInductiveToStr dt);
                     true
                    )
                 end
        end

    (* translate a co-inductive data definition and put it into IST
       if there are errors, print them and return false
       otherwise return true
     *)
    fun parseCodataDef(aCodataDef: YCodataDef):bool =
        let
            val ((aDataName:string, aArgList:string list),
                  aAliasName:string,
                  aStruList: (string * YTypeExp) list
                ) = aCodataDef
            val tvarmap = UT.from0toN (fn (x,i)=>(x,i)) (aAliasName::aArgList)
            val errmsg = checkCodataDef aCodataDef
        in
            if errmsg<>[] then
                (print ("parsing datatype '" ^ aDataName ^ "' definition error:\n");
                 printlist errmsg;
                 false
                )
            else let val dt = ( aDataName,
                                length aArgList,
                                map (fn (dtrname,dtrtype) => (dtrname, YTE2TE(tvarmap, dtrtype)))
                                aStruList
                              )
                 in
                    (IST.newCODATA dt;
                     IST.newCOMB (genUnfold dt);
                     IST.newCOMB (genComap dt);
                     IST.newCOMB (genCoinductiveToStr dt);
                     true
                    )
                 end
        end


    fun YPattern2CPattern (yPattern: YPattern):CPattern =
        let fun ERR(msg) = (ES.newErr msg; cpTUPLE [])
            fun ERRS(msgs) = (ES.newErrs msgs; cpTUPLE [])
            fun yp2cp(ypat) =
            (case ypat
               of ypVAR v =>
                    (case IST.find v
                       of IST.sCTR(ctrtype, dataname, i) =>
                            if(TE.srcCnt ctrtype>0) then
                                ERR("missing arguments for constructor pattern '" ^ v ^ "'")
                            else
                                cpCTR(dataname,i,[])
                        | _ => cpVAR v
                    )
                | ypFUN v => cpFUN v
                | ypCTR(ctrname,pat) =>
                    (case IST.find ctrname
                       of IST.sCTR(ctrtype, dataname, i) =>
                            let
                                val paramCnt = TE.srcCnt ctrtype
                                val (argCnt, arglst)
                                    = case pat
                                        of ypTUPLE [] => (1, [pat])
                                         | ypTUPLE l => (length l, l)
                                         | ypVAR "_" =>
                                            (* allows patterns like "cons _" *)
                                                (paramCnt, List.tabulate(paramCnt, fn _ => ypVAR "_"))
                                         | _ => (1, [pat])
                            in
                                if paramCnt=1 then
                                    cpCTR(dataname, i, [yp2cp pat])
                                else if argCnt=paramCnt then
                                    cpCTR(dataname, i, map yp2cp arglst)
                                else
                                    ERR("wrong number of argument for pattern '" ^ ctrname ^ "'")
                            end
                        | _ => ERR("'" ^ ctrname ^ "' is not a constructor")
                    )
                | ypTUPLE [x] => yp2cp x (* optimize (x) *)
                | ypTUPLE tl => cpTUPLE(map yp2cp tl)
                | ypRECORD recphraselist =>
                    let val dtrname = #1 (hd recphraselist)
                        val phnames = map #1 recphraselist
                    in
                        case IST.find dtrname
                          of IST.sDTR(_,dataname,_) =>
                            let val struList = map #1 (IST.getCODATADtrList dataname)
                                val notdtrs = Set.minus(phnames,struList)
                                val dupdtrs = UT.findDup struList
                                fun f dtrname = case Map.find(dtrname, recphraselist)
                                                  of SOME pat => yp2cp pat
                                                   | NONE => cpVAR "_"
                            in
                                if notdtrs <> [] then
                                    ERRS(map (fn n => "'" ^ n ^ "' is not a destructor of " ^ dataname)
                                             notdtrs)
                                else if dupdtrs<>[] then
                                    ERRS(map (fn x=> "duplicate destructor '" ^ x ^ "' in pattern")
                                             dupdtrs)
                                else
                                    cpRECORD(dataname, map f struList)
                            end
                           | _ => ERR("'" ^ dtrname ^ "' is not a destructor")
                    end
                | ypINTRANGE(i,j) => cpINTRANGE(i,j)
            )(*end case*)
        in
            yp2cp(yPattern)
        end
    fun insertVarsIntoIST [] = ()
      | insertVarsIntoIST(x::xs) = (IST.newVAR(x,0); insertVarsIntoIST xs)
    fun insertFunsIntoIST [] = ()
      | insertFunsIntoIST(x::xs) = (IST.newFUN(x,TE.tVAR 1, cUNIT); insertFunsIntoIST xs)
    fun YT2CT(aYT:YTerm):CTerm =
        let fun ERR(msg) = (ES.newErr msg; cUNIT)
        in
             case aYT
               of yINT n => cINT n
                | ySTR s => cSTR s
                | yFLOAT s => cFLOAT s
                | yNULL    => cUNIT
                | yVAR v   => (case IST.find v
                                 of IST.sFUN _ => cFUN(v)
                                  | IST.sCOMB _ => cCOMB(v,[])
                                  | IST.sCTR(_,dataname,i) => cCTR(dataname,i)
                                  | IST.sMACRO n => cMACRO(n)
                                  | IST.sVAR _ => cVAR(v)
                                  | _ => ERR("undefined variable: '" ^ v ^ "'")
                              )
                | yTUPLE lst  => cTUPLE(map YT2CT lst)
                | yRECORD lst =>
                    if isInvalidDtrList(map #1 lst) then
                        cUNIT
                    else
                        let
                            val dataName = IST.getDTRCodata(#1 (hd lst))
                            val dtrlst = IST.getCODATADtrList dataName
                        in
                            cRECORD(dataName,
                                    map (fn (dname,_) => YT2CT(Map.getValue(dname, lst))) dtrlst
                                )
                        end
                | yAPP(f,t)  => cAPP(YT2CT f, YT2CT t)
                | yCOMB(name, macList) =>
                    let val newml = map YT2CT macList
                        fun notcomb(term) =
                                if macList=[] then term else ERR(name ^ " is not a combinator")
                        fun macmatch(n, term) =
                                if length macList = n then
                                    term
                                else
                                    ERR("wrong number of macro parameter for '" ^ name ^"'")
                    in
                        case IST.find name
                          of IST.sMACRO n  => notcomb(cMACRO(n))
                           | IST.sDATA(n, _)   => macmatch(n, cCOMB(nameOfMap name, newml))
                           | IST.sCODATA(n, _) => macmatch(n, cCOMB(nameOfComap name, newml))
                           | IST.sCTR(_,dname,i) => notcomb(cCTR(dname, i))
                           | IST.sDTR(_,dname,i) => notcomb(cDTR(dname, i))
                           | IST.sFUN _ => notcomb(cFUN(name))
                           | IST.sCOMB((_,mactypes),_) => macmatch(length mactypes, cCOMB(name, newml))
                           | IST.sNONE => ERR("undefined symbol "^ name)
                           | _ => ERR("'" ^ name ^ "' is not a function/combinator")
                           (* ^ "raise Fail("yCOMB(" ^ name ^ ") in cvt in YT2CT") *)
                    end
                | yFOLD lst =>
                    if isInvalidCtrList(map #1 lst) then
                        cUNIT
                    else
                        let val dataName=IST.getCTRData(#1 (hd lst))
                            val ctrlst = IST.getDATACtrList dataName
                            val combname = nameOfFold dataName
                            val sym = IST.find combname
                        in
                            cCOMB(combname,
                                map (fn (cname,_) => YT2CT(Map.getValue(cname, lst))) ctrlst
                                )
                        end
                | yUNFOLD(phlst) =>
                    if isInvalidDtrList(map #1 phlst) then
                        cUNIT
                    else
                        let val dataName=IST.getDTRCodata(#1 (hd phlst))
                            val dtrlst = IST.getCODATADtrList dataName
                            val combname = nameOfUnfold dataName
                            val sym = IST.find combname

                        in
                            cCOMB(combname,
                                map (fn (cname,_) => YT2CT(Map.getValue(cname, phlst)))
                                dtrlst
                                )
                        end
                | yPattABS(patabslst) =>
                    let
                        val patlen = length(#1 (hd patabslst))

                        fun docvt(pattlst, term) =
                            let val cpatlst = map YPattern2CPattern pattlst
                            in
                                if ES.hasErr() then
                                    ([], cUNIT)
                                else
                                    let val vars = getCPatternVarsInList cpatlst
                                        val funs = getCPatternFunsInList cpatlst
                                        val dups = UT.findDup(vars @funs)
                                        val errs = map (fn v=> "duplicate variable '" ^ v ^ "' in pattern") dups
                                    in
                                        if  dups <> [] then
                                            (ES.newErrs(errs);
                                             ([], cUNIT))
                                        else
                                            (IST.save();
                                             insertVarsIntoIST (vars);
                                             insertFunsIntoIST (funs);
                                             (cpatlst, YT2CT term)
                                                 before IST.restore()
                                            )
                                    end
                            end
                    in
                        (* the number of patterns in each phrase must be the same *)
                        if List.exists (fn (ptl,_)=> length ptl <> patlen) patabslst then
                            ERR( "number of arguments in the patterned abstraction do not match" )
                        else
                            cPattABS(map docvt patabslst)
                    end
                | yLOCALFUN(fundeflist) =>
                    let
                        val funnames = map #1 fundeflist
                        fun f(fname, recurPos, term) = (fname,recurPos, YT2CT term)
(*                        fun f(fname, recurvar, varlst, term) =
                            if UT.findDup(fname::varlst)<>[] then
                                (ES.newErrs(map (fn v=>"duplicate variable '"^v^"' in function '"^fname^"'")
                                               (UT.findDup(fname::varlst))
                                           );
                                 (fname, ~1, cUNIT)
                                )
                            else(
                                IST.save();
                                insertVarsIntoIST(varlst);
                                (fname,UT.getPosition(recurvar, varlst), cABS(varlst, YT2CT term))
                                    before IST.restore()
                                )
 *)
                    in
                        (IST.save();
                         insertFunsIntoIST(funnames);
                         cLETREC(map f fundeflist, cFUN(hd funnames))
                        )
                    end
                (*| _ => raise Fail("case _ in cvt in YTerm2CTerm") *)
        end
    fun typeTemplate (cABS(varlst,_)) = TE.tARROW(TE.newTVarList(length varlst), TE.newTVar())
      | typeTemplate (cPattABS []) = TE.newTVar()
      | typeTemplate (cPattABS pattlst) = TE.tARROW(TE.newTVarList(length (#1 (hd pattlst))), TE.newTVar())
      | typeTemplate (cFUN name) = TE.renewTVars (IST.getFUNType name)
      | typeTemplate _ = TE.newTVar()

    fun parseFunDef(aFunDef: YFunDef) =
        let val (aFunName:string,
                 aFunTypeOpt: YTypeExp option,
                 aMacList: (string * YTypeExp option) list,
                 aFunBody: YTerm) = aFunDef

            fun insertmacs([],_) = ()
              | insertmacs(m::ms, i) = (IST.newMACRO(m,i); insertmacs(ms,i+1))

            val _ = ES.resetErrs();
            val _ =
                if IST.find aFunName <> IST.sNONE then
                    ES.newErr( "name clash: " ^ aFunName )
                else if UT.findDup(map #1 aMacList) <> [] then
                    ES.newErr( "duplicate macro names in function " ^ aFunName )
                else
                    ES.newErrs( UT.flattenList (map (fn NONE => []
                                                      | SOME yte => checkYTE(getYTEVars yte, yte))
                                                (aFunTypeOpt::(map #2 aMacList)))
                              )

            val combbody = if ES.hasErr() then
                                cUNIT
                           else
                           (
                            IST.save();
                            IST.newFUN(aFunName, TE.tVAR 1,cINLINE[]);
                            insertmacs(map #1 aMacList, 0);
                            YT2CT aFunBody
                                before IST.restore()
                           )
        in
            if ES.hasErr() then
                (print ("Parsing function '" ^ aFunName ^ "' error:\n");
                 printlist(ES.getErrs());
                 false
                )
            else
                let val ytevars = getYTEVarsInList(map (fn NONE => yPRODS []
                                                         | SOME yte => yte)
                                                       (aFunTypeOpt::(map #2 aMacList)))
                    val varmap = UT.from0toN (fn (x,_)=>(x, TE.newTVarID())) (UT.removeDup ytevars)
                    fun spec2type NONE = TE.newTVar()
                      | spec2type (SOME x) = YTE2TE(varmap, x)

                    val funtype= if aFunTypeOpt=NONE then
                                    (*make sure a multiple argument function S1,...Sn->T will not be
                                      typed as S1*...*Sn ->T
                                     *)
                                    typeTemplate combbody
                                 else
                                    spec2type aFunTypeOpt
                    val mactypes = map spec2type (map #2 aMacList)
                    val maclist = List.tabulate(length mactypes, fn i=> ("$" ^ Int.toString(i)))
                    val context = (aFunName, funtype)::(ListPair.zip(maclist, mactypes))

                    val equs = (TE.resetEquSet();
                                decompTerm((aFunName, funtype)::context, funtype, combbody);
                                TE.getEquSet()
                               )

                    val (sol, typeerr) = (TE.solveEquSet equs, [])
                                handle TE.TYPECHECK(msg) => ([], [msg])

                    val finalFunType = TE.apply sol funtype
                    val finalMacTypes = map (TE.apply sol) mactypes

                    (* check whether user specified types too general *)
                    fun checkUserTypes([], _, _) = true
                      | checkUserTypes(NONE::ys, ft::fs, ut::us) = checkUserTypes(ys,fs,us)
                      | checkUserTypes((SOME _)::ys, ft::fs, ut::us) =
                            if(TE.equivalent(ft, ut)) then
                                checkUserTypes(ys,fs,us)
                            else
                                false
                      | checkUserTypes _ = raise Fail("checkUserTypes")
                    fun transformTerm term =
                        let val depat = optimizeTerm (dePattern term)
                            fun isErr (cINLINE _) = true
                              | isErr _ = false
                        in
                            if findTerm isErr depat <>[] then
                                (print ("Parsing function '" ^ aFunName ^ "' error:\n");
                                 print ("pattern incomplete");
                                 cINLINE[]
                                )
                            else
                                depat
                        end

                in
                    if typeerr<>[] then
                        (print ("Typechecking function '" ^ aFunName ^ "' error:\n");
                         printlist typeerr;
                         false
                        )
                    else if checkUserTypes(aFunTypeOpt::(map #2 aMacList),
                                           finalFunType::finalMacTypes,
                                           funtype::mactypes) then
                        let
                            val t = transformTerm combbody
                            val (newcombbody, success)= if t=cINLINE[] then (combbody, false)
                                                                       else (t, true)
                        in
                            if mactypes=[] then
                                IST.newFUN(aFunName, finalFunType, newcombbody)
                            else
                                IST.newCOMB(aFunName,
                                           (finalFunType, finalMacTypes),
                                           newcombbody
                                           );
                            success
                        end
                    else
                        (print ("Parsing function '" ^ aFunName ^ "' error:\n");
                         print ("user specified type too general");
                         false
                        )
                end
        end


    fun parseAST aAST =
        let
            fun parseList(aFun, []) = true
              | parseList(aFun, x::xs) = if (aFun x) then parseList(aFun, xs) else false

            fun parseDef(aDef) =
                    (case aDef
                       of yDATADEF x   => parseDataDef x
                        | yCODATADEF x => parseCodataDef x
                        | yALIASDEF x  => parseAliasDef x
                        | yFUNDEF x    => parseFunDef x
                                          handle ex => (print ("unhandled exception in " ^(#1 x)^ "\n");raise ex)
                    )
        in
            parseList(parseDef, aAST)
        end

end

