data nat -> C = zero
              | succ: C->C

data C->inflist(A) = head: C->A
                   | tail: C->C.

def nats = => (| x => head: x
                 | tail: succ x
               |) zero.

def nats2 = => {: nats #n =
(*                   zero => (head: zero, tail: nats(succ zero)) *)
                    _ => (head: n, tail: nats(succ n))
                :} zero.

def start = => nats2.










