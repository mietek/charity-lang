local
    structure TE = TypeExpression
    open CoreTerm
    val identityABS = cABS(["$x"], cVAR "$x")
    (* Each system generated fold/unfold/map combinator is in
    the form of "x => t", where x is the root var
    *)
    val rootVarName = "$X"
    val rootVar = cVAR rootVarName
    val paramVarPrefix = "$v"

    fun getVarList n = List.tabulate(n, fn i => paramVarPrefix ^ Int.toString(i+1))

    val dataVAR  = cVAR "$DD"
    val levelVAR = cVAR "$NN"
    fun genABS t = cABS( ["$DD", "$NN"], t)
    val concatFun = cFUN("op^")

    fun genConcat [] = raise Fail("genConcat")
      | genConcat [x] = x
      | genConcat ((cSTR x)::(cSTR y)::ss) = genConcat( cSTR(x^y):: ss)
      | genConcat (x::y::ss) = genConcat( cAPP(concatFun, cTUPLE( [x,y]))::ss)

    fun addComma [] = []
      | addComma [x] = [x]
      | addComma (x::xs) = x::(cSTR ",")::(addComma xs)

in
structure CharityData =
struct

type CDataDef   = string * int * (string * TE.TypeExp list) list
type CCodataDef = string * int * (string * TE.TypeExp) list

fun nameOfFold   tname = "$FOLD_"^tname
fun nameOfMap    tname = "$MAP_" ^tname
fun nameOfUnfold tname = "$UNFOLD_" ^ tname
fun nameOfComap  tname = "$MAP_" ^ tname (* map and comap has to have the same name *)
fun nameOfToStr  tname = tname ^"2string"

fun typeOfCtrs(aDataDef:CDataDef):(string * TE.TypeExp) list =
    let
        val (typename,tvarcnt, ctrList) = aDataDef
        val theType = TE.genName(typename, tvarcnt)
        fun cvt ctrtypes = TE.tARROW(map (TE.substTVar0 theType) ctrtypes, theType)
    in
        map (fn (n,t) => (n, cvt t)) ctrList
    end
fun typeOfNthCtr(aDataDef, n) = (#2 (List.nth(typeOfCtrs aDataDef, n)))

fun typeOfDtrs(aCodataDef:CCodataDef):(string * TE.TypeExp) list =
    let
        val (typename,tvarcnt, dtrList) = aCodataDef
        val theType = TE.genName(typename, tvarcnt)
        fun addt(TE.tARROW(srclst,dst)) = TE.tARROW(srclst @ [TE.tVAR 0], dst)
          | addt(t) = TE.tARROW([TE.tVAR 0], t)
    in
        map (fn (n,t) => (n, TE.substTVar0 theType (addt t)))
            dtrList
    end
fun typeOfNthDtr(aCodataDef, n) = (#2 (List.nth(typeOfDtrs aCodataDef, n)))

fun genCaseTG(aDataDef: CDataDef): TE.TypeGroup =
    let
        val (typeName,tvarcnt, ctrList) = aDataDef
        val srcType = TE.tNAME(typeName, TE.genTVar1toN tvarcnt)
        val dstType = TE.tVAR(tvarcnt+1)
        val macTypes = map (fn (_, ctrtypes) =>
                                TE.tARROW(map (TE.substTVar0 srcType) ctrtypes, dstType)
                        ) ctrList
    in
        (TE.tARROW([srcType], dstType), macTypes)
    end

fun genFoldTG(aDataDef: CDataDef): TE.TypeGroup =
    let
        val (typeName, typeVarCnt, ctrList) = aDataDef
        val dstType = TE.tVAR(typeVarCnt+1)
    in
        ( TE.tARROW([TE.genName(typeName, typeVarCnt)], dstType),
          map (fn (_,t) => TE.substTVar0 dstType
                                        (TE.tARROW(t,TE.tVAR 0))
            ) ctrList
        )
    end

fun genMapTG(aDataDef: CDataDef):TE.TypeGroup =
    let
        val (typeName, typeVarCnt, ctrList) = aDataDef
        val srcType =  TE.tNAME(typeName, TE.genTVar1toN typeVarCnt)
        val dstType =  TE.tNAME(typeName, TE.genTVarMtoN(typeVarCnt+1, typeVarCnt*2))
        val macTypes = List.tabulate(typeVarCnt,
                            fn i=> TE.tARROW([TE.tVAR(i+1)], TE.tVAR(i+typeVarCnt+1)))
    in
        (TE.tARROW([srcType], dstType), macTypes)
    end


fun genTostrTG(aDataDef: CDataDef): TE.TypeGroup =
    let
        val (typename,tvarcnt, ctrList) = aDataDef
        val srcType = TE.tNAME(typename, TE.genTVar1toN tvarcnt)
    in
        (TE.tARROW([srcType, TE.intTE], TE.stringTE),
        List.tabulate(tvarcnt, fn i=> TE.tARROW([TE.tVAR(i+1), TE.intTE], TE.stringTE)))
    end


fun genRecordTG(aCodataDef: CCodataDef): TE.TypeGroup =
    let
        val (typeName,tvarcnt, dtrList) = aCodataDef
        val dstType = TE.tNAME(typeName, TE.genTVar1toN tvarcnt)
        val macTypes = map (TE.substTVar0 dstType) (map #2 dtrList)
    in
        (dstType, macTypes)
    end


fun genUnfoldTG(aCodataDef: CCodataDef): TE.TypeGroup =
    let
        val (typeName, typeVarCnt, dtrList) = aCodataDef
        val srcType = TE.tVAR (typeVarCnt+1)
    in
        ( TE.tARROW([srcType], TE.genName(typeName, typeVarCnt)),
          map (fn (_,TE.tARROW(srclst,dst)) =>
                        TE.substTVar0 srcType (TE.tARROW(srclst @ [TE.tVAR 0], dst))
                | (_,t) => TE.substTVar0 srcType (TE.tARROW([TE.tVAR 0],t))
              ) dtrList
        )
    end

fun genComapTG(aCodataDef: CCodataDef): TE.TypeGroup =
    let
        val (typename,tvarcnt, dtrList) = aCodataDef
    in
        genMapTG(typename, tvarcnt, [])
    end


fun genCotostrTG(aCodataDef: CCodataDef): TE.TypeGroup =
    let
        val (typename,tvarcnt, dtrList) = aCodataDef
        val srcType = TE.genName(typename, tvarcnt)
    in
        (TE.tARROW([srcType, TE.intTE], TE.stringTE),
         List.tabulate(tvarcnt, fn i=> TE.tARROW([TE.tVAR(i+1), TE.intTE], TE.stringTE)))
    end

fun genTostrEi(aTermList, aTypeExp) =
    let
        (*generate an abstraction A from a type t, which takes an input of type t, and an integer n,
        and output a string
        *)
        fun dogen(TE.tVAR x) = List.nth(aTermList, x)
        | dogen(TE.tUNIT)  = genABS(cSTR "()")
        | dogen(TE.tPRODS []) = genABS(cSTR "")
        | dogen(TE.tPRODS [x]) = dogen(x)
        | dogen(TE.tPRODS tlist) =
                let
                    val varlst= getVarList (length tlist)
                    val slist = ListPair.map
                        (fn (t,v) => cAPP(dogen(t), cTUPLE([cVAR v, levelVAR])))
                        (tlist, varlst)
                    val slist2 = ((cSTR "(")::(addComma slist)) @ [ cSTR ")" ]
                in
                    genABS(cAPP( cABS( varlst, genConcat slist2),
                                dataVAR
                            )
                        )
                end
        | dogen(TE.tNAME(tname, arglst)) = cCOMB(nameOfToStr tname,
                                                map dogen arglst)
        | dogen _ = raise Fail("dogen")
    in
        dogen aTypeExp
    end
(*generate the tostring combinator for a type *)
fun genInductiveToStr( typeName:string
                    , typeVarCnt: int
                    , ctrList: (string * TE.TypeExp list) list
                    ) =
        let
            val combname =nameOfToStr typeName
            val macList = cFUN(combname)::(List.tabulate(typeVarCnt, fn i=> cMACRO i))
            fun gencasephrase (struName, []) = cSTR struName
            | gencasephrase (struName, struTypes) =
                    cABS(["$c"],
                        genConcat(
                            [ if length struTypes>1 then
                                    cSTR struName
                                else
                                    cSTR (struName^" ")
                                ,
                                cAPP(genTostrEi(macList, TE.tPRODS struTypes),
                                    cTUPLE([cVAR "$c", levelVAR]))
                            ]
                            )
                        )
        in
            (combname,
            genTostrTG (typeName, typeVarCnt, ctrList),
            genABS(cAPP(cCASE(typeName, map gencasephrase ctrList),
                        dataVAR)
                )
            )
        end

fun genCoinductiveToStr( typeName:string
                    , typeVarCnt:int
                    , dtrList: (string * TE.TypeExp) list) =
        let
            val combname = nameOfToStr typeName
            val macList = cFUN(combname)::(List.tabulate(typeVarCnt, fn i=> cMACRO i))
            val equalzero = cAPP(cFUN("op>"),
                                cTUPLE( [ levelVAR, cINT 0 ]))
            (*val recType = TE.tNAME(typeName, TE.genTVar1toN typeVarCnt) *)

            fun genrecphrase (dtrName, TE.tARROW _, dtrNo) =
                    cSTR (dtrName^":\\ ...") (*higher order *)
            | genrecphrase (dtrName, dtrType, dtrNo) =
                    cAPP(cCASE("bool",
                            [ (* level<=0, show ... *)
                                cSTR (dtrName ^":... "),
                        (* level> 0, destruct and show*)
                        genConcat([ cSTR (dtrName ^":"),
                            cAPP(genTostrEi(macList, dtrType),
                                    cTUPLE(
                                    [
                                        cAPP(cDTR(typeName, dtrNo), dataVAR),
                                        cAPP(cFUN("op-"),
                                            cTUPLE( [ levelVAR, cINT 1 ]))
                                    ])
                                    ) ])
                        ]),
                    equalzero)
            fun genallphrase(n, []) = []
            | genallphrase(n, (dtrname,dtrtype)::xs) =
                    genrecphrase(dtrname, dtrtype,n)::genallphrase(n+1, xs)
            val phraselist = (cSTR"(")::(addComma(genallphrase(0, dtrList)) @ [ cSTR ")" ])

        in
            (combname,
            genCotostrTG (typeName, typeVarCnt, dtrList),
            genABS(genConcat phraselist)
            )
        end

    (* The \theta^E combinator in my thesis
    termlist is the list of macros, with the first macro being mapped
    to tVAR 0                                                      *)
    fun genMapEi(termlist,TE.tVAR x) = List.nth(termlist, x)
      | genMapEi(termlist,TE.tPRODS l) =
            let val varlst= getVarList (length l)
                fun f(x,y) = cAPP(genMapEi(termlist,x), cVAR y)
            in
                cABS( varlst,cTUPLE( ListPair.map f (l, varlst)))
            end
      | genMapEi(termlist, TE.tNAME(tname,[])) = identityABS
      | genMapEi(termlist, TE.tNAME(tname, l)) =
            let val newtermlist = map (fn x=> genMapEi(termlist,x)) l
            in
                cCOMB(nameOfMap tname, newtermlist)
            end
      | genMapEi(termlist, TE.tUNIT) = cUNIT
      | genMapEi(termlist, _) = raise Fail("genMapEi")

    (* generate the body of the fold combinator *)
    fun genFold(typeName, typeVarCnt, ctrList) =
        let
            val foldname = nameOfFold typeName
            fun genFoldPhrase((_, ctrtype), phraseNo) =
                    let
                        val vars = getVarList (length ctrtype)
                        val Ilist = List.tabulate(typeVarCnt,fn _=> identityABS)
                    in
                        cABS( vars,
                            cAPP(cMACRO phraseNo,
                                cAPP( genMapEi(cFUN(foldname)::Ilist, TE.tPRODS ctrtype),
                                        cTUPLE (map (fn s=>cVAR s) vars)
                                    )
                                )
                            )
                    end
        in
            (foldname,
            genFoldTG(typeName, typeVarCnt, ctrList),
            cABS( [rootVarName],
                cAPP(cCASE(typeName,
                            UT.from0toN genFoldPhrase ctrList),
                        rootVar))
            )
        end

    fun genMap(typeName, typeVarCnt, ctrList) =
        let
            val mapname = nameOfMap typeName
            fun genMapPhrase((ctrname, ctrtype), phraseNo) =
                let
                    val vars = getVarList(length ctrtype)
                in
                    cABS( vars,
                        cAPP(cCTR(typeName,phraseNo),
                            cAPP( genMapEi( cFUN(mapname)::List.tabulate(typeVarCnt, fn i=> cMACRO i),
                                            TE.tPRODS ctrtype),
                                  cTUPLE( map (fn s=>cVAR s) vars)
                                )
                            )
                        )
                end
        in
            (mapname,
            genMapTG(typeName, typeVarCnt, ctrList),
            cABS( [rootVarName],
                cAPP(cCASE(typeName,
                            UT.from0toN genMapPhrase ctrList),
                        rootVar))
            )
        end

    fun genUnfold( typeName:string
                , typeVarCnt: int
                , dtrList: (string * TE.TypeExp) list
                ) =
        let
            val unfoldname = nameOfUnfold typeName
            val Ilist = List.tabulate(typeVarCnt,fn _=> identityABS)
            fun genUnfoldPhrase((_, dtrtype), phraseNo) =
                let val vars = getVarList(TE.srcCnt dtrtype) (* the C in source should not be counted *)
                    val vartuple = if vars=[] then
                                        rootVar
                                else
                                        cTUPLE( (map (fn s=>cVAR s) vars) @ [ rootVar ])
                in
                    cABS( vars,
                        cAPP( genMapEi(cFUN(unfoldname)::Ilist, TE.dstType dtrtype),
                            cAPP(cMACRO phraseNo, vartuple)
                            )
                        )
                end
        in
            (unfoldname,
            genUnfoldTG(typeName, typeVarCnt, dtrList),
            cABS( [rootVarName],
                cRECORD(typeName, UT.from0toN genUnfoldPhrase dtrList)
                )
            )
        end

    fun genComap(typeName, typeVarCnt, dtrList) =
        let
            val comapname = nameOfComap typeName
            fun genComapPhrase ((_, dtrtype), phraseNo) =
                let val vars = getVarList(TE.srcCnt dtrtype)
                    val vartuple = if vars=[] then
                                    rootVar
                            else
                                    cTUPLE( (map (fn s=>cVAR s) vars) @ [ rootVar ])
                in
                    cABS(vars,
                        cAPP( genMapEi(cFUN(comapname)::List.tabulate(typeVarCnt, fn i=> cMACRO i),
                                       TE.dstType dtrtype),
                              cAPP(cDTR(typeName,phraseNo), vartuple)
                            )
                        )
                end
        in
            (comapname,
            genComapTG(typeName, typeVarCnt,[]),
            cABS( [rootVarName],
                cRECORD(typeName, UT.from0toN genComapPhrase dtrList)
                )
            )
        end

end (*struct CharityData *)
end
