signature ppProp =
  sig

  val ppPropText : hml -> string

  val ppPropLatex : hml -> string

  val ppJudgement : (hml list * hml list) TABLEAU -> string

  val latexProlog : string

  val latexEpilog : string

  val latexCaption : string -> string

  end;


