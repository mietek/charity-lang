%
%   success or fail datatype
%

data SF(A) -> C = ss: A -> C
                | ff: -> C.


%
%   The natural numbers
%

data nat -> C = zero: -> C
              | succ: C -> C.

def pred: nat -> nat
    = zero => zero
    | succ(n) => n.

def monus: nat * nat -> nat
    = (n,m) => {| zero:  => n
                | succ: n' => pred(n')
                |}(m).

def lt: nat * nat -> bool
    = (n,m) => { zero    => false
               | _       => true
               }(monus(m,n)).

def leq: nat * nat -> bool
    = (n,m) => { zero    => true
               | _       => false
               }(monus(n,m)).

def add: nat * nat -> nat
    = (n,m) => {| zero: => m
                | succ: x => succ(x)
                |}(n).

def mult: nat * nat -> nat
    = (n,m) => {| zero:  => zero
                | succ: x => add(m,x)
                |}(n).

def p0 = x,y => x
def p1 = x,y => y
%
%  The list datatype 
%

def lhead: list(A) -> SF(A)
    = nil       => ff
    | cons(a,_) => ss(a).

def append: list(A) * list(A) -> list(A)
    = (L1,L2) => {| nil: => L2
                  | cons: z => cons(z)
                  |}(L1).

def flatten: list(list(A)) -> list(A)
    = LL => {| nil: => nil
             | cons: z => append(z)
             |}(LL).

def length: list(A) -> nat
    = L => {| nil: => zero
            | cons: (_,n) => succ(n)
            |}(L).

def rev: list(A) -> list(A)
    = L => p1( {| nil: => (L,nil)
                | cons:{ (_,(nil,V)) => (nil,V)
                       | (_,(cons(a,R),V)) => (R,cons(a,V))
		       }
                |}(L) ).


def filter{ pred: A -> bool}: list(A) -> list(A)
    = L => {| nil:  => nil
            | cons: (a,Ls) => { true => cons(a,Ls)
                              | false => Ls
                              }(pred(a))
            |}(L).

%
%   A program  to determine list equality
%

def nil_and: bool * (list(A) * list(A)) -> bool
    = (v,(nil,nil)) => v
    | _ => false.

def eq_list{eq: A * A -> bool}: list(A) * list(A) -> bool
    = (L1,L2) => nil_and( {| zero: => (true,(L1,L2))
                           | succ: {(true,(cons(a1,R1),cons(a2,R2))) =>
                                            (eq(a1,a2),(R1,R2))
                                 | _ => (false,(nil,nil))}
                           |}(length(L1))
                         ).

%
%  Infinite lists
%

data C -> inflist(A) = head: C -> A
                     | tail: C -> C.

def get: nat * inflist(A) -> A
    = (n,L) => head( {| zero: => L
                      | succ: LL => tail(LL)
                      |}(n) ).

def start = => add(succ succ zero, succ zero)