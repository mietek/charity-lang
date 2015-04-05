data coprod(A, B) -> C = b0 : A -> C      % NEEDED BELOW
                       | b1 : B -> C.

data C -> exp(A, B) = fn : C -> A => B.

data S -> stack(A) = push : S -> A + 1 => S
                   | pop  : S -> S * A + 1.

data Q -> queue(A) = enqueue : Q -> A + 1 => Q
                   | dequeue : Q -> Q * A + 1.
