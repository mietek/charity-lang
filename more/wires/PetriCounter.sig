signature Counter =
  sig

  exception HAS_A_PROOF

  val counter   
  : (string list * ((string list * string * string list) * prop list) list
     * prop list * prop list
     * (prop list * (string list * string * string list)) list * string list) 
      TABLEAU -> act list 

  end;


