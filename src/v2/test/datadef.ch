%
% test inductive datatype definition
%
data nat -> C = zero: 1 -> C
              | succ: C -> C.

data nat2 -> C = zero2: -> C
              | succ2: C -> C.
data mylist(A,B) -> C = nil: -> C
                    | cons: A,B, C -> C.

data mylist2(A) -> C = nil2: 1 -> C
                     | cons2: A*C -> C.

data D-> inflist(A) = head: D-> A
                    | tail: D -> D.

data C-> exp(A,B) = fn: C->A=>B.