signature ParseHml =
  sig
  val PARSE: string -> hml
  val P : hml SMLofNJ.frag list -> hml
  exception PROP_COMPILE_ERROR of string
  end;


