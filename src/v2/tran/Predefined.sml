structure Predefined =
struct

structure IST = ImperativeSymbolTable
structure TE = TypeExpression
open CoreTerm
open CharityData


val boolDataDef = (TE.boolName, 0, [ ("false", []), ("true", []) ])
val listDataDef = (TE.listName, 1, [ ("nil", []),   ("cons", [ TE.tVAR 1, TE.tVAR 0 ]) ])

fun loadPredefinedDatatypes () =
    (
        (*primitive types *)
        IST.newDATA(TE.intName   , 0, []);
        IST.newDATA(TE.stringName, 0, []);
        IST.newDATA(TE.realName  , 0, []);
        IST.newDATA(TE.longName  , 0, []);

        (* inductive types *)
        IST.newDATA boolDataDef;
        IST.newCOMB(genInductiveToStr boolDataDef);

        IST.newDATA listDataDef;
        IST.newCOMB(genFold listDataDef);
        IST.newCOMB(genMap  listDataDef);
        IST.newCOMB(genInductiveToStr listDataDef);

        ()
    )

val predefinedFunctions =
[
  ("op+",     TE.tARROW([TE.intTE, TE.intTE], TE.intTE),    [ "addI" ]),
  ("op-",     TE.tARROW([TE.intTE, TE.intTE], TE.intTE),    [ "subI" ]),
  ("op*",     TE.tARROW([TE.intTE, TE.intTE], TE.intTE),    [ "mulI" ]),
  ("op/",     TE.tARROW([TE.intTE, TE.intTE], TE.intTE),    [ "divI" ]),
  ("opmod",   TE.tARROW([TE.intTE, TE.intTE], TE.intTE),    [ "modI" ]),

  ("op>",     TE.tARROW([TE.intTE, TE.intTE], TE.boolTE),   [ "gtI"  ]),
  ("op<",     TE.tARROW([TE.intTE, TE.intTE], TE.boolTE),   [ "gtI", "not"  ]),
  ("op>=",    TE.tARROW([TE.intTE, TE.intTE], TE.boolTE),   [ "geI"  ]),
  ("op<=",    TE.tARROW([TE.intTE, TE.intTE], TE.boolTE),   [ "geI", "not"  ]),

  ("or",      TE.tARROW([TE.boolTE, TE.boolTE], TE.boolTE), [ "or"  ]),
  ("and",     TE.tARROW([TE.boolTE, TE.boolTE], TE.boolTE), [ "and" ]),
  ("not",     TE.tARROW([TE.boolTE, TE.boolTE], TE.boolTE), [ "not" ]),

  ("op==",    TE.tARROW([TE.tVAR 1, TE.tVAR 1], TE.boolTE), []),
  ("op<>",    TE.tARROW([TE.tVAR 1, TE.tVAR 1], TE.boolTE), []),
  ("op==$B",  TE.tARROW([TE.tVAR 1, TE.tVAR 1], TE.boolTE), [ "eqB" ]),
  ("op==$U",  TE.tARROW([TE.tVAR 1, TE.tVAR 1], TE.boolTE), [ "eqU" ]),
  ("op==$P",  TE.tARROW([TE.tVAR 1, TE.tVAR 1], TE.boolTE), [ "eqP" ]),
  ("op<>$B",  TE.tARROW([TE.tVAR 1, TE.tVAR 1], TE.boolTE), [ "eqB", "not" ]),
  ("op<>$U",  TE.tARROW([TE.tVAR 1, TE.tVAR 1], TE.boolTE), [ "eqU", "not" ]),
  ("op<>$P",  TE.tARROW([TE.tVAR 1, TE.tVAR 1], TE.boolTE), [ "eqP", "not" ]),

  ("op^",     TE.tARROW([TE.stringTE, TE.stringTE], TE.stringTE),   [ "strcat" ]),
  ("op@",     TE.tARROW([TE.listTE, TE.listTE], TE.listTE), []),
  ("op@$B",   TE.tARROW([TE.listTE, TE.listTE], TE.listTE), [ "call _$append$B" ]),
  ("op@$P",   TE.tARROW([TE.listTE, TE.listTE], TE.listTE), [ "call _$append$P" ]),
  ("op@$U",   TE.tARROW([TE.listTE, TE.listTE], TE.listTE), [ "call _$append$U" ]),

  ("int2string",    TE.tARROW([TE.intTE, TE.intTE], TE.stringTE),  ["popB 1","i2str"]),
  ("long2string",   TE.tARROW([TE.longTE,TE.intTE], TE.stringTE),  ["popB 1","l2str"]),
  ("real2string",   TE.tARROW([TE.realTE,TE.intTE], TE.stringTE),  ["popB 1","d2str"]),

  ("string2string",TE.tARROW([TE.stringTE, TE.intTE], TE.stringTE), [ "popB 1" ])
]

fun loadPredefinedFunctions() =
    let
        fun doload [] = ()
          | doload ((fname,ftype,fcode)::fs) = (IST.newFUN(fname,ftype, cINLINE fcode); doload fs)
    in
        doload predefinedFunctions
    end
(*
fun getInlineFunctionCode(aName) =
    (case List.find (fn (name,code) => name=aName) inlineFunctionCode
      of SOME(_, codelist) => SOME codelist
       | NONE => NONE
    )
*)
end
