    (**********************************************************************
     *                                                                    *
     *    Representing and manipulating types in Charity                  *
     *                                                                    *
     **********************************************************************)
structure TypeExpression =
struct
    exception TYPECHECK of string
    open Utility
    (* Represents of a term type *)
    datatype TypeExp = tNAME  of string * (TypeExp list)
                     | tPRODS of TypeExp list
                     | tARROW of TypeExp list * TypeExp
                     | tVAR   of int
                     | tUNIT (* tUNIT is different from tPRODS[], it's a built-in type *)

    val intName    = "int"
    val boolName   = "bool"
    val realName   = "real"
    val longName   = "long"
    val stringName = "string"
    val listName   = "list"

    val intTE   = tNAME(intName,[])
    val boolTE  = tNAME(boolName,[])
    val longTE  = tNAME(longName,[])
    val realTE  = tNAME(realName,[])
    val stringTE= tNAME(stringName,[])
    val listTE  = tNAME(listName,[ tVAR 1])

    val nullTE = tVAR(~1)

    (* a group of type expressions, such as the type of a combinator *)
    type TypeGroup= TypeExp * TypeExp list

    fun mainTypeOfTG (maint, _) = maint
    fun subTypesOfTG (_, tlist) = tlist

        local
                val uniqueID= ref 100
        in
        fun newTVarID() = (uniqueID:=(!uniqueID)+1; !uniqueID)
                fun newTVar () = tVAR(newTVarID())
        fun newTVarIDList n = List.tabulate(n, fn _ =>newTVarID())
                fun newTVarList n = List.tabulate(n, fn _=> newTVar())

                fun resetNewTVar x = uniqueID:=x
        end

        local
                val equSetPtr: ((TypeExp*TypeExp) list) ref = ref []
    in
        fun resetEquSet() = (equSetPtr:=[])
        fun getEquSet() = (!equSetPtr)
        fun newEqu(t1,t2) = equSetPtr:= (t1,t2)::(!equSetPtr)
        end

    fun genTVar1toN n = List.tabulate(n, fn i => tVAR (i+1))
    fun genTVarMtoN(m,n) = List.tabulate(n-m+1, fn i=> tVAR (m+i))
    fun genArrow n = tARROW (genTVar1toN n, tVAR(n+1))
    fun genName(name,tvarcnt)  = tNAME(name, genTVar1toN tvarcnt)

    fun srcType (tARROW([],_))  = tPRODS []
      | srcType (tARROW([x],_)) = x
      | srcType (tARROW(l,_))   = tPRODS l
      | srcType _ = tPRODS []

    fun dstType (tARROW(_,t)) = t
      | dstType t = t

    fun typeName(tNAME(name,_)) = name
      | typeName _ = raise Fail("typeName in TypeExpression")

    fun srcList (tARROW(l,_)) = l
      | srcList _ = []

    fun srcCnt (tARROW(src,_)) = length src
      | srcCnt _ = 0

    fun existTVar0 (tVAR 0) = true
      | existTVar0 (tVAR _) = false
      | existTVar0 (tPRODS tl) = existTVar0InList tl
      | existTVar0 (tNAME(s,tl)) = existTVar0InList tl
      | existTVar0 (tARROW(tl,t)) = (existTVar0InList tl) orelse (existTVar0 t)
      | existTVar0 _ = false
    and existTVar0InList [] = false
      | existTVar0InList(t::ts) = (existTVar0 t) orelse (existTVar0InList ts)

    fun mapTE f (tVAR n)    = f n
      | mapTE f (tPRODS l)  = tPRODS(map (mapTE f) l)
      | mapTE f (tNAME(s,l))= tNAME(s, map (mapTE f) l)
      | mapTE f (tARROW(l,t)) = tARROW(map (mapTE f) l, mapTE f t)
      | mapTE f x = x

    fun mapTG f (maint, tlist) = (f maint, map f tlist)

    fun substTVar (id, newt) te  =
        mapTE (fn varid=> if id=varid then newt else (tVAR varid)) te

    (* subst var 0 is so frequently used, so we make a special function for it *)
    fun substTVar0 newt te = substTVar (0, newt) te

    fun substMultiTVars sublist te =
        mapTE (fn varid=> case List.find (fn (i,_)=> i=varid) sublist
                          of NONE => tVAR varid
                           | SOME(_, t) => t
            )
            te

    (* substitute a type var within a function type *)
    (* fun substArrow  sub (inType, outType) = (map (substTVar sub) inType, substTVar sub outType)*)

    (* substitute type var i with the ith type expression in tlist, with i start from 0
       if there are less than i+1 items in the list, then type var i is not substituted *)
    fun substTVar0toN tlist te =
        mapTE (fn varid => if varid>=0 andalso varid<(length tlist) then
                        List.nth(tlist,varid)
                     else
                        tVAR varid
              )
              te
    fun substTVar1toN tlist te =
        mapTE (fn varid => if varid>=1 andalso varid<=(length tlist) then
                                List.nth(tlist,varid-1)
                           else
                                tVAR varid
              )
              te

    (* return a list of distinct IDs, each corresponding to a type var.
       Sorted by their order of first appearance in the domain
     *)
    fun getAllTVars te =
        let fun doGet (tNAME(_, l), result) = doGetList(l, result)
              | doGet (tPRODS l, result )   = doGetList(l, result)
              | doGet (tVAR i, result)      = if i>0 then i::result (*tVAR 0 is special*)
                                                     else result
              | doGet (tARROW(l,t), result) =  doGet(t, doGetList(l,result))
              | doGet (_, result) = result
            and doGetList([], result)    = result
              | doGetList(x::xs, result) = doGetList(xs, doGet(x,result))
        in
            rev (removeDup (doGet(te, [])))
        end

    fun getAllTVarsInTG (maint,tlist) = getAllTVars(tPRODS(maint::tlist))

    (* give all type variables a distinct new id *)
    fun renewTVars (te:TypeExp) =
        let val oldtvars = getAllTVars te
            val newtvars = newTVarList(length oldtvars)
        in
            substMultiTVars (ListPair.zip(oldtvars,newtvars)) te
        end
    fun renewTVarsInTG (tg:TypeGroup) =
        let val oldtvars = getAllTVarsInTG tg
            val newtvars = newTVarList(length oldtvars)
        in
            mapTG (substMultiTVars (ListPair.zip(oldtvars,newtvars))) tg
        end
    local
        fun list2str([], _) = ""
          | list2str(x::[], _) = x
          | list2str(x::y::l, separator) = x ^ separator ^ list2str(y::l, separator)
    in
        (* convert a type expression into a string (for debug purpose) *)
        fun DomainToStr (tVAR x) =  "T"^Int.toString(x)
          | DomainToStr (tPRODS l)    = "(" ^ list2str(map DomainToStr l, "*") ^")"
          | DomainToStr (tNAME(x,[])) = x
          | DomainToStr (tNAME(x,l) ) = x ^ "(" ^ list2str(map DomainToStr l, ",") ^ ")"
          | DomainToStr (tUNIT)       = "1"
          | DomainToStr (tARROW(l,t)) = "[" ^ list2str(map DomainToStr l, ",") ^"] -> "^ (DomainToStr t)
    end

    local
        fun doNorm (tPRODS [x]) = doNorm x
          | doNorm (tARROW([],t)) = doNorm t
          | doNorm (tPRODS l) = tPRODS(map doNorm l)
          | doNorm (tNAME(n,l)) = tNAME(n, map doNorm l)
          | doNorm (tARROW(l,t)) = tARROW(map doNorm l, doNorm t)
          | doNorm x = x
    in
        fun normalizeTE (te:TypeExp) =
            let
                val tvars = getAllTVars te
                val newtvars = genTVar1toN(length tvars)
            in
                substMultiTVars (ListPair.zip(tvars, newtvars)) (doNorm te)
            end

        fun normalizeTG (tg:TypeGroup) =
            let
                val tvars = getAllTVarsInTG tg
                val newtvars = genTVar1toN(length tvars)
            in
                mapTG (substMultiTVars (ListPair.zip(tvars, newtvars))) (mapTG doNorm tg)
            end
    end

    fun solveEqu(S,T) =
        let
            fun occured x (tVAR y)      = x=y
              | occured x (tPRODS l)    = List.exists (occured x) l
              | occured x (tNAME(_,l))  = List.exists (occured x) l
              | occured x (tARROW(l,t)) = (List.exists (occured x) l) orelse (occured x t)
              | occured x _ = false

            fun check(x, tVAR y) = if x=y then []
                                          else [(x, tVAR y)]
              | check(x, y) = if occured x y then
                                  raise TYPECHECK("Cyclic definition in "^DomainToStr(tVAR x)^"="^DomainToStr(y))
                              else
                                  [(x,y)]
            fun match(x, tPRODS [y])= match(x, y) (* (A) = A *)
              | match(tPRODS [x],y )= match(x, y)
              | match(tPRODS(xl), tPRODS(yl)) = matchList(xl,yl)
              | match(tNAME(xn,xl), tNAME(yn,yl)) = if xn<>yn then
                                                        raise TYPECHECK("can not match "^ xn ^" with "^yn)
                                                    else
                                                        matchList(xl,yl)
              | match(tUNIT, tUNIT) = []
              | match(tARROW([],x), y) = match(x,y)
              | match(x, tARROW([],y)) = match(x,y)
              | match(tARROW([x],y), tARROW([x2],y2)) = match(x,x2) @ match(y,y2)
              | match(tARROW([x],y), tARROW(l, y2)) = match(x, tPRODS l) @ match(y,y2)
              | match(tARROW(l,y), tARROW([x],y2)) = match(x, tPRODS l) @ match(y,y2)
              | match(tARROW(xl,y), tARROW(xl2, y2)) = matchList(xl,xl2) @ match(y,y2)
              | match(tVAR x, y)  = check(x, y)
              | match( y, tVAR x) = check(x,y)
              | match(x,y) = raise TYPECHECK("Can not match "^(DomainToStr x) ^ " with " ^ (DomainToStr y))

            and matchList([], []) = []
              | matchList(x::xs, y::ys) = match(x,y)@matchList(xs,ys)
              | matchList _ = raise TYPECHECK("Can not match tuple of different size")

            fun coalesce(_,[]) = []
              | coalesce((x,s), (y,t)::subs) =
                    if x=y then match(s,t)@ coalesce((x,s),subs)
                           else check(y, substTVar (x,s) t)@coalesce((x,s),subs)

            fun linearize [] = []
              | linearize ((x,s)::rest) = (x,s)::linearize(coalesce((x,s),rest))

        in
            linearize(match(S,T))
        end

    fun apply [] T = T
      | apply (sub::rest) T = apply rest (substTVar sub T)

    fun solveEquSet eqnlist =
        let
            fun addEqu(sol, (S,T)) = sol@ solveEqu(apply sol S, apply sol T)
            fun unifyList([], sol) = sol
              | unifyList((s,t)::rest,sol) =
                                        unifyList(rest, addEqu(sol,(s,t)))
(*                              handle TYPECHECK(msg) =>
                                                (print ("error while unify "
                                                       ^ DomainToStr(s) ^ "=" ^ DomainToStr(t)
                                                           ^":" ^ msg);
                                                 raise TYPECHECK(msg)
                                                )
*)
        in
            unifyList(eqnlist, [])
        end

    fun equivalent(x,y) = (normalizeTE x) = (normalizeTE y)
(*        let val varsInX = getAllTVars x
            val tvarsInY = map tVAR (getAllTVars y)
        in
            if length varsInX<>length tvarsInY then
                false
            else
                (substMultiTVars (ListPair.zip(varsInX,tvarsInY)) x) = y
        end *)
    (* test whether S is an instance of T( T is more general than T , S and T are normalized
       this is done by solve the equation S=T and then apply the solution to S; if S is equivalent
       to T after applying the solution, then true *)
    fun isInstance(S, T) =
        let
            val newS = renewTVars S
            val newT = renewTVars T
        in
            equivalent(apply (solveEqu(newS,newT)) newS, S)
                handle TYPECHECK _ => false
        end

end
