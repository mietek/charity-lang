data C -> inflist(A) = head: C -> A
                     | tail: C -> C.

data nat -> C = zero: -> C
              | succ: C -> C.

def nats = (: nats n = (head: n, tail: nats succ n) :) zero.

def start = 
    => (| n => head: n
        |      tail: succ n |) zero.

