signature CutHml =
  sig

  exception UN_CUTABLE

  val upL : (hml list * hml list) TABLEAU -> (hml list * hml list) TABLEAU

  val upR : (hml list * hml list) TABLEAU -> (hml list * hml list) TABLEAU

  val upBoth : (hml list * hml list) TABLEAU -> (hml list * hml list) TABLEAU

  val cutElim : (hml list * hml list) TABLEAU -> (hml list * hml list) TABLEAU

  val cutElimR : (hml list * hml list) TABLEAU -> (hml list * hml list) TABLEAU

  val mkCut : hml * (hml list * hml list) TABLEAU * 
		    (hml list * hml list) TABLEAU -> 
		    (hml list * hml list) TABLEAU  

  end;


