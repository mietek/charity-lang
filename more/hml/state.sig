signature StateHml =
  sig

  type state
  exception HAS_A_PROOF

  val counter : (hml list * hml list) TABLEAU -> state

  val ppStateText : state -> string

  val ppStateLatex : state -> string

  val latexProlog : string

  val latexEpilog : string

  end;


