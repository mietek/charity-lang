(* convert patterned cterm into non-patterned ones *)
structure CTermTransformation =
struct
structure IST = ImperativeSymbolTable
structure UT = Utility
structure ES = ErrorSet
open CoreTerm
open Predefined
open AST2CT

datatype StagedTypeExp = gNAME of string * (StagedTypeExp list) * int
                       | gPRODS of StagedTypeExp list
                       | gARROW of StagedTypeExp list * StagedTypeExp
                       | gVAR of int * int
                       | gUNIT
 
   fun deLetrec(aTerm:CTerm): CTerm =
        let
            (* remove letrec should be from outside to inside, because inner function
               may call outside function and find it's undefined
             *)
            fun deLet (context,cTUPLE(l))   = cTUPLE(deLetList(context,l))
              | deLet (context,cAPP(x,y))      = cAPP(deLet(context,x), deLet(context,y))
              | deLet (context,cABS(varlst,t)) = cABS(varlst, deLet(context,t))
              | deLet (context,cCOMB(n,l))     = cCOMB(n, deLetList(context,l))
              | deLet (context,cCASE(n,l))     = cCASE(n, deLetList(context,l))
              | deLet (context,cRECORD(n,l))   = cRECORD(n, deLetList(context,l))
              | deLet (context,cPattABS l) = cPattABS(map (fn (pl,t)=> (pl, deLet(context,t))) l)
              | deLet(context, cLETREC(letlst,aterm)) =
                let
                    (*infer types of all the functions, note this functions
                    can be mutually recursive, so we must do the decomp
                    at the same time
                    *)
                    val typepairs = map (fn (s,_,t) => (s, typeTemplate t)) letlst
                    val equset = (TE.resetEquSet();
                                decompTermList(typepairs@context, map #2 typepairs, map #3 letlst);
                                TE.getEquSet())
                                handle ex => (print ("###" ^(#1 (hd letlst))); raise ex)

                    val sol = TE.solveEquSet equset

                    (*to avoid name conflict, these local functions are renamed *)
                    val namepairs = map (fn (s,_,_) => (s, s ^ "$" ^ UT.newName())) letlst

                    fun substnames([],t) = t
                      | substnames((oldn,newn)::ns, t) =
                            substnames(ns, substFreeName (oldn, cFUN newn) t)

                    val newfbodys = map (fn (_,_,b) => substnames(namepairs, b)) letlst
                    val newcontext = ListPair.zip(map #2 namepairs, map (TE.apply sol) (map #2 typepairs))
                    fun insertFuns([],_, _) = ()
                      | insertFuns(fname::ns, fbody::bs, t::ts) =
                        let val ftype = TE.apply sol t
                        in
                            IST.newFUN(fname, ftype, deLet(newcontext @ context, fbody));
                            insertFuns(ns, bs, ts)
                        end
                      | insertFuns _ = raise Fail("insertFuns")
                in
                    insertFuns(map #2 namepairs, newfbodys, map #2 typepairs);
                    substnames(namepairs, aterm)
                end
              | deLet(context, x) = x
            and deLetList(context,[]) = []
              | deLetList(context,x::xs) = deLet(context,x)::deLetList(context,xs)
        in
            deLet([],aTerm)
        end





(* make a term a function, insert the new function into IST,
   return a term which calls the new function and is equivalent to the old term

fun liftTerm(aName, aTerm) =
    let
        val newfname = aName ^ "$" ^ UT.newName()

        val freevars = removeDup (getFreeVariables aTerm)
        val oldvars  = (case aTerm
                          of cABS(varlst,_) => varlst
                           | _ => []
                       )

        fun wrap t = cABS(oldvars,
                          cAPP(t, cTUPLE(map (fn s=>cVAR s) (oldvars @ freevars))
                         ))

        fun changeRecur (cFUN x) = if x=aName then wrap (cFUN x) else cFUN x
          | changeRecur x = x

        val newfname = aName ^ "$" ^ UT.newName()
        val newfbody = (case aTerm
                          of cABS(varlst, t) => cABS(oldvars @ freevars, mapTerm changeRecur t)
                           | cNAMED(name, i, t) => liftTerm(name, t)
                           | t => cABS(freevars, mapTerm changeRecur t)
                       )
        val newftype = inferTermType(newfbody)
    in
        IST.newFUN(newfname, newftype, newfbody);
        wrap (cFUN(newfname))
    end
*)

(* convert all combinator calls in the form of f{g} to a function call f', where f'=[body of f][g/M]
   unless {g} contains cMACRO. Some combinators are defined with calling to another combinator, it
   helps to reduce code size even if we can not fully decomb the combinator.
 *)
fun deComb(aTerm:CTerm):CTerm =
    let
        fun demac(cCOMB(combname,[])) = cFUN(combname) (*cFUN(combname)*)
          | demac(cCOMB(combname,maclist)) =
                if List.exists hasMacros maclist then
                    (* exists macro as parameter, do nothing *)
                    cCOMB(combname, maclist)
                else
                    let
                        val combbody = IST.getCOMBBody combname

                        (* transform the macro parameters into a local function definition
                           the code for demac has become so much simplified now that we
                           have letrec!
                        *)
                        val letlst = map (fn macTerm=>("$mac"^UT.newName(), ~1, macTerm)) maclist
                        val newmaclist = map (fn (n,_,_)=> cFUN n) letlst
                        val newfname = combname ^ "$" ^ UT.newName()
                        val fbody = substFreeName (combname,cFUN newfname)
                                                  (substMacros newmaclist combbody)
                        (*the new fbody may contain comb too*)
                        val fbody2= deComb fbody
                    in
                        cLETREC([(newfname, ~1, cLETREC(letlst,fbody2))], cFUN(newfname))
                    end
          | demac x = x
    in
        mapTerm demac aTerm
    end
(*remove all the letrec structures in a term(assuming all the functions
  in letrec has been lambda-lifted) and insert them into IST
 *)

(*
fun checkLETS(aLetsList: (string *int * CTerm) list) =
    let
        fun docheck(varStageList:(string*int) list, aTerm) =
            (case aTerm
               of

fun checkTermination(aTerm:CTerm):CTerm =
    let


        fun docheck(cLETREC(letlst, t)) = checkLETS(letlst)
          | docheck x = ()
    in
        mapTerm docheck aTerm
    end
*)



















end (*struct*)
