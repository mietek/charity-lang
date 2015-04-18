
(*
 *     THE STACK DATATYPE (THE SPECIFICATION)
 *
 *)

data S -> stack(A) = push: S -> SF(A) => S
                   | pop : S -> S * SF(A).


(* AN IMPLEMENTATION: *)

def a_stack: 1  -> stack(A)

           = () => (| l => push: ff   => []
                               | ss a => cons (a, l)

                    |      pop: { []           => ([], ff)
                                | cons (a, l') => (l', ss a)
                                }
                                  l
                    |)
                       [].

