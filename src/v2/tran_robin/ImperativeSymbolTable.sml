(* implement symbol table *)
structure ImperativeSymbolTable =
struct
    structure TE = TypeExpression
    type TypeExp = TE.TypeExp
    type TypeGroup = TE.TypeGroup
        type CTerm = CoreTerm.CTerm

    datatype SymValue   = sALIAS    of int * TypeExp        (* # of arguments, type *)
                        | sDATA     of int * (string * TypeExp list) list
                        | sCODATA   of int * (string * TypeExp) list
                        | sCTR      of TypeExp * string * int
                        | sDTR      of TypeExp * string * int
                        | sCOMB     of TypeGroup * CTerm
                        | sFUN      of TypeExp * CTerm
                        | sVAR      of int
                        | sMACRO    of int
                        | sNONE  (* not found in the symbol table *)

    type SymTable = (string * SymValue) list

    val curTblPtr: SymTable ref = ref []
    val tblStkPtr: (SymTable list) ref = ref []

    open Utility
    (* reset/save/restore current symbol table *)
    fun reset() = (curTblPtr:= []; tblStkPtr :=[])
    fun save() = tblStkPtr := (!curTblPtr) :: (!tblStkPtr)
    fun restore() = (curTblPtr:= hd (!tblStkPtr) ; tblStkPtr := tl (!tblStkPtr))

    fun getCurTable() = !curTblPtr
    fun setCurTable(aTable) = curTblPtr := aTable

    fun find aName =
        let fun f [] = sNONE
              | f ((name,attr)::xs) = if name=aName then
                                        attr
                                  else
                                        f xs
        in
            f (!curTblPtr)
        end

    fun insert(aName, aAttr) = curTblPtr:= ((aName,aAttr)::(!curTblPtr))
    fun insertList aPairList = curTblPtr:= List.revAppend(aPairList,!curTblPtr)

    fun newALIAS(aName, aArgCnt, aType) = insert(aName, sALIAS(aArgCnt, aType))
    fun newDATA(aName: string,
                aArgCnt: int,
                aCtrLst: (string * TypeExp list) list
               ) =
        let
            val theType = TE.genName(aName, aArgCnt)
            fun cvt ctrtypes = TE.tARROW(map (TE.substTVar0 theType) ctrtypes, theType)
            val ctrtypes = map (fn (n,t) => (n, cvt t)) aCtrLst
                        fun toNormCtrDef((cname, ctype), i) =
                    (cname, sCTR(ctype,  aName, i))
        in
            insertList( (aName, sDATA(aArgCnt, aCtrLst))::(from0toN toNormCtrDef ctrtypes))
        end

    fun newCODATA(aName: string,
                  aArgCnt: int,
                  aDtrLst: (string * TypeExp) list
                 )=
        let
            val theType = TE.genName(aName, aArgCnt)
            fun addt(TE.tARROW(srclst,dst)) = TE.tARROW(srclst @ [TE.tVAR 0], dst)
              | addt(t) = TE.tARROW([TE.tVAR 0], t)

            val dtrtypes = map (fn (n,t) => (n, TE.substTVar0 theType (addt t)))
                               aDtrLst
            fun toDtrDef((dname,dtype), i) = (dname, sDTR(dtype, aName, i))
        in
            insertList((aName,sCODATA(aArgCnt, aDtrLst))::(from0toN toDtrDef dtrtypes) )
        end

    fun newVAR(aName, i)   = insert(aName, sVAR(i))
    fun newMACRO(aName, i) = insert(aName, sMACRO(i))
    fun newCOMB(aName, tg, term) = insert(aName, sCOMB(tg,term))
    fun newFUN(aName, te, term) = insert(aName,  sFUN(te,term))

    (* the following are convenience functions *)
    fun getDATA(aName) =
        (case find aName
           of sDATA d => d
            | _ => raise Fail("getDATA("^aName^")")
        )
    fun getDATAArgCnt  aName = #1 (getDATA aName)
    fun getDATACtrList aName = #2 (getDATA aName)

    fun getCODATA(aName) =
        (case find aName
           of sCODATA d => d
            | _ => raise Fail("getCODATA("^aName^")")
        )
    fun getCODATAArgCnt  aName = #1 (getCODATA aName)
    fun getCODATADtrList aName = #2 (getCODATA aName)

    fun getALIAS(aName) =
        (case find aName
           of sALIAS d => d
            | _ => raise Fail("getALIAS("^aName^")")
        )
    (*a function is considered a special combinator *)
    fun getFUN(aName) =
        (case find aName
           of sFUN d => d
            | sCOMB((t,[]),body) => (t,body)
            | _ => raise Fail("getFUN("^aName^")")
        )
    fun getFUNBody(aName) = #2 (getFUN aName)
    fun getFUNType(aName) = #1 (getFUN aName)

    fun getCOMB(aName) =
        (case find aName
           of sCOMB d => d
            | sFUN(t,b) => ((t,[]),b)
            | _ => raise Fail("getCOMB("^aName^")")
        )
    fun getCOMBBody(aName) = #2 (getCOMB aName)
    fun getCOMBTG(aName)   = #1 (getCOMB aName)

    fun getCTR(aName) =
        (case find aName
           of sCTR d => d
            | _ => raise Fail("getCTR("^aName^")")
        )
    fun getCTRType aName = #1 (getCTR aName)
    fun getCTRData aName = #2 (getCTR aName)
    fun getCTRID   aName = #3 (getCTR aName)

    fun getDTR(aName) =
        (case find aName
           of sDTR d => d
            | _ => raise Fail("getDTR("^aName^")")
        )
    fun getDTRType   aName = #1 (getDTR aName)
    fun getDTRCodata aName = #2 (getDTR aName)
    fun getDTRID     aName = #3 (getDTR aName)

    fun getVAR(aName) =
        (case find aName
           of sVAR d => d
            | _ => raise Fail("getVAR("^aName^")")
        )
(*
    fun getNthCtrName(aDataName, i) =
        (case find aDataName
           of sDATA(_, ctrlst) => #1 (List.nth(ctrlst, i))
            | _ => raise Fail("getNthCtrName(" ^ aDataName ^ "," ^ Int.toString(i) ^ ")")
        )
    fun getNthDtrName(aDataName, i) =
        (case find aDataName
           of sCODATA(_, dtrlst) => #1 (List.nth(dtrlst, i))
            | _ => raise Fail("getNthDtrName(" ^ aDataName ^ "," ^ Int.toString(i) ^ ")")
        )

    fun getNthCtrType(aDataName, i) =
        (case find aDataName
           of sDATA(tvarcnt, ctrlst) => typeOfNthCtr((aDataName,tvarcnt,ctrlst),i)
            | _ => raise Fail("getNthCtrType(" ^ aDataName ^ "," ^ Int.toString(i) ^ ")")
        )
    fun getNthDtrType(aDataName, i) =
        (case find aDataName
           of sCODATA(tvarcnt, dtrlst) => typeOfNthDtr((aDataName,tvarcnt,dtrlst),i)
            | _ => raise Fail("getNthDtrType(" ^ aDataName ^ "," ^ Int.toString(i) ^ ")")
        )
*)
(*    fun getCtrAttr(aCtrName) =
        (case find aCtrName
           of sCTR(_,dataname, i) => (dataname,i)
            | _ => raise Fail("getCtrAttr(" ^ aCtrName ^ ")")
        )
    fun getDtrAttr(aDtrName) =
        (case find aDtrName
           of sDTR(_,dataname, i) => (dataname,i)
            | _ => raise Fail("getDtrAttr(" ^ aDtrName ^ ")")
        )
*)

(*    fun getCombTG(aCombName) =
        (case find aCombName
           of sCOMB(tg,_) => tg
            | _ => raise Fail("getCombTG(" ^ aCombName ^ ")")
        )
 *)

(*    fun getCaseTG(aDataName) =
        (case find aDataName
           of sDATA(tvarcnt, ctrlist) => typeOfCase(aDataName,tvarcnt, ctrlist)
            | _ => raise Fail("getCaseTG(" ^ aDataName ^ ")")
        )
 *)
 (*
    fun getRecordTG(aDataName) =
        (case find aDataName
           of sCODATA(tvarcnt, dtrlist) => typeOfRecord(aDataName,tvarcnt, dtrlist)
            | _ => raise Fail("getRecordTG(" ^ aDataName ^ ")")
        )
    fun getCombBody(aCombName) =
        (case find aCombName
           of sCOMB(_,body) => body
            | _ => raise Fail("getCombBody(" ^ aCombName ^ ")")
        )
*)

    fun isALIAS (sALIAS _) = true
      | isALIAS _ = false

    fun isDATA (sDATA _) = true
      | isDATA _ = false

    fun isCODATA (sCODATA _) = true
      | isCODATA _ = false

    fun isCTR (sCTR _) = true
      | isCTR _ = false

    fun isDTR (sDTR _) = true
      | isDTR _ = false

    fun isVAR (sVAR _) = true
      | isVAR _ = false

    fun isMACRO (sMACRO _) = true
      | isMACRO _ = false

    fun isFUN (sFUN _) = true
      | isFUN _ = false

    fun isCOMB (sCOMB _) = true
      | isCOMB _ = false

(*    fun isTypeName (sDATA _)   = true
      | isTypeName (sCODATA _) = true
      | isTypeName _ = false
*)
(*
    fun typeOf  (sCTR(t,_,_)) = t
      | typeOf  (sDTR(t,_,_)) = t
      | typeOf  (sCOMB(tg,_)) = TE.mainTypeOfTG tg
      | typeOf  (sFUN(te,_))  = te
      | typeOf  (sALIAS(_,t)) = t
      | typeOf  (sNONE) = raise Fail("typeOf NONE")
      | typeOf  (sMACRO _) = raise Fail("typeOf MACRO")
      | typeOf  (sVAR _)  = raise Fail("typeOf VAR")
      | typeOf  (sDATA _) = raise Fail("typeOf DATA")
      | typeOf  (sCODATA _) = raise Fail("typeOf CODATA") *)
(*      | typeOf _ =  raise Fail("typeOf in ImperativeSymbolTable") *)

(*    fun dataOf  (sCTR(_, d,_)) = d
      | dataOf  (sDTR(_, d,_)) = d
      | dataOf _ = raise Fail("dataOf in ImperativeSymbolTable")

    fun idOf (sCTR(_,_,id)) = id
      | idOf (sDTR(_,_,id)) = id
      | idOf (sVAR id) = id
      | idOf (sMACRO id) = id
      | idOf _ = raise Fail("idOf in ImperativeSymbolTable")

    fun ctrsOf (sDATA(_,sl)) = sl
      | ctrsOf _ = raise Fail("ctrsOf in ImperativeSymbolTable")

    fun dtrsOf (sCODATA(_,sl)) = sl
      | dtrsOf _ = raise Fail("dtrsOf in ImperativeSymbolTable")

        fun bodyOf (sCOMB(_, body)) = body
      | bodyOf (sFUN(_, body)) = body
      | bodyOf _ = raise Fail("bodyOf in ImperativeSymbolTable")

    fun macCnt (sALIAS(cnt,_)) = cnt
      | macCnt (sDATA(cnt,_)) = cnt
      | macCnt (sCODATA(cnt,_)) = cnt
      | macCnt (sCOMB(tg,_)) = length(TE.subTypesOfTG tg)
      | macCnt (sFUN(te,_)) = 0
      | macCnt (sCTR _) = 0
      | macCnt (sDTR _) = 0
      | macCnt (sMACRO _) = 0
      | macCnt _ = raise Fail("macCnt in ImperativeSymbolTable")

    fun macTypes (sCOMB(tg,_)) = TE.subTypesOfTG tg
      | macTypes _ = []
*)
end

