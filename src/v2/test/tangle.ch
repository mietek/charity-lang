data tangle -> C
  = tgl : list(C) -> C. 

def start = => 
  tgl nil.
  (* tgl []. *)
  (* tgl [tgl [],tgl [tgl [],tgl []]]. *)
