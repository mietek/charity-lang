
(*
 *     THE QUEUE DATATYPE (THE SPECIFICATION)
 *
 *)

data Q -> queue(A) = enq: Q -> SF(A) => Q
                   | deq: Q -> Q * SF(A).


(* AN IMPLEMENTATION: *)

def a_queue: 1  -> queue(A)

           = () => (| l => enq: ff   => []
                              | ss a => append (l, [a])

                    |      deq: { []           => ([], ff)
                                | cons (a, l') => (l', ss a)
                                }
                                  l
                    |)
                       [].

