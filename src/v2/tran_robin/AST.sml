(*
    definition of the abstract syntax tree
*)
structure AST =
struct
    exception YACCERR of string

    datatype YPattern = ypVAR of string
                      | ypFUN of string
                      | ypCTR of string * YPattern
                      | ypTUPLE of YPattern list
                      | ypRECORD of (string * YPattern) list (* might be incomplete *)
                      | ypINTRANGE of int * int

    (* return a list of variables in a YPattern, including function variables but excluding
       the don't care pattern "_" *)
    fun getYPatternVars(ypVAR "_")  = []
      | getYPatternVars(ypVAR v)    = [ v ]
      | getYPatternVars(ypFUN "_")  = []
      | getYPatternVars(ypFUN v)    = [ v ]
      | getYPatternVars(ypCTR(_, p))= getYPatternVars p
      | getYPatternVars(ypTUPLE pl) = getYPatternVarsInList pl
      | getYPatternVars(ypRECORD rl)= getYPatternVarsInList(map #2 rl)
      | getYPatternVars _ = []
    and getYPatternVarsInList [] = []
      | getYPatternVarsInList (p::pl) = (getYPatternVars p) @ (getYPatternVarsInList pl)


    (* patterned charity terms *)
    datatype YTerm = yINT    of int
                   | ySTR of string
                   | yFLOAT  of string (* 'real' is not an equality type *)
                   | yVAR    of string
                   | yNULL            (* the empty pair: () *)
                   | yAPP    of YTerm * YTerm
                   | yTUPLE  of YTerm list
                   | yPattABS   of (YPattern list * YTerm) list
                   | yRECORD of (string * YTerm) list
                   | yFOLD   of (string * YTerm) list
                   | yUNFOLD of (string * YTerm) list
                   | yCOMB   of string * (YTerm list)
                   (* a set of local named function, possibly recursive
                      name of function, position of recursition, function body *)
                   | yLOCALFUN of (string * int * YTerm) list

    (* charity type expression *)
    datatype YTypeExp= yUNIT  (* the 1 *)
                     | yPRODS of YTypeExp list
                     | yNAME of string * YTypeExp list
                     | yARROW of YTypeExp list * YTypeExp


    type YDataName = string * string list
    type YMacroDef = string * YTypeExp option
    type YStructorDef = string * YTypeExp option

    type YAliasDef      = YDataName * YTypeExp
    type YDataDef       = YDataName * string * (string * YTypeExp list) list
    type YCodataDef     = YDataName * string * (string * YTypeExp) list
    type YFunDef   = string * YTypeExp option * (string * YTypeExp option) list * YTerm

    datatype YDefinition = yDATADEF of YDataDef
                         | yCODATADEF of YCodataDef
                         | yALIASDEF of YAliasDef
                         | yFUNDEF of YFunDef


end (* AST *)
