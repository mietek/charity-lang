data nat -> C = zero
              | succ: C->C
(*
data C->exp(A,B) = fn: C->A=>B.
def testho = (x, (fn:\ f)) => f x.

def p6 = x,y => (x,{true => zero} y)
       | x,_ => (x,succ zero).

data C -> myco(A) = dtr1: C->A
                  | dtr2: C->A
                  | dtr3: C->A=>A
                  | dtr4: C->C.

def p5 = (dtr1:x, dtr3: f) =>  x.



def f:list(A)->list(A) = x=> x.
data C->inflist(A) = head: C->A
                   | tail: C->C.

def k = => (|x => head: x
                | tail: succ x
            |) zero.

def g:nat,A -> nat*A = x, y => (x,y).
def h = x,y => g(x,f(y)).

def p (*:mylist(A)->mylist(A) *)
    = nil => f cons(3,nil)
    | cons(a,x) => f x.

def p2 = nil, nil => zero
       | nil,cons _ => succ zero
       | x, nil => succ succ zero
       | _, _ => zero.

def p3 = (fn:\f), x => f x.
def p4 = (head:(x,k), tail:(head:y,tail:z)) => (x,y,z).

def length = y => {| nil: => zero
                   | cons: _, z => succ z
                   |} x.
*)
(*data mylist(A) -> C = nil
                    | cons: A,C -> C.
 *)
def app = L1,L2 => {: app #l1 =
                      nil => L2
                    | cons(a,x) => cons(a,app x)
                    :}L1
def even = {: even #t =
              zero => true
            | succ m => odd m
            : odd #p =
              zero => false
            | succ n => even n
            :}
def monus = {: monus #m n =
                zero => zero
              | succ p =>  { zero => m
                           | succ q => monus(p,q)
                           } n
             :}

def div = {: div #m n =
              zero => zero
            | succ _ => succ (div(monus(m,n),n))
           :}
def div2 = {: div #m n =
              zero => zero
            | succ _ => succ (div3(m,n,n))
            : div3 #m n1 n2 =
              zero => zero
            | succ p => n1;{ zero => div(m,n2)
                           | succ q => div3(p,q,n2)
                           }
            :}

def start = => div2(succ succ succ succ succ succ succ zero, succ succ succ zero)










