signature ppProp =
  sig

  val ppPropText : prop -> string

  val ppPropLatex : prop -> string


  val ppJudgement
        : (string list * ((string list * string * string list) * 'a) list
           * prop list * prop list
           * ('b * (string list * string * string list)) list * string list)
            TABLEAU
          -> string


  val latexProlog : string

  val latexEpilog : string

  val latexCaption : string -> string

  end;


