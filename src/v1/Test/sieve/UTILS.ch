(* some excepting routines (eg. hd, tl, div) depend on strings being 
   defined and so have been put at the end of STRINGS.ch *)

data coprod(A,B) -> C  = b0 : A -> C 
                       | b1 : B -> C.

data list(a) -> C =
      nil:1 -> C
    | cons:a * C -> C.

data bool -> C =
      true | false : 1 -> C.

data nat -> C =
      zero:1 -> C
    | succ:C -> C.

(*
data result(A) -> Y= 
    exception : string -> Y
  | val : A -> Y.
*)

(* some basic logical operations *)
def or : bool * bool -> bool =
      (true(), _)  => true
    | (false(), y) => y.

def not : bool -> bool =
      true()  => false
    | false() => true.

def and : bool * bool -> bool =
      (true(), true()) => true
    |  _               => false.

def xor : bool * bool -> bool =
      (true(), false()) => true
    | (false(), true()) => true
    | _                 => false.

def imp : bool * bool -> bool =
      (true(), y) => y
    |  _          => true.

(* arithmetic operations : nat * nat -> nat *)

def add : nat * nat -> nat =
    (x,y) => {| zero: ()    => y
              | succ: state => succ(state)
              |} (x).

def pred : nat -> nat =
      zero()  => zero
    | succ(n) => n.

(* subtract y from x *)

def sub : nat * nat -> nat =
    (x,y) => {| zero:()  => x
              | succ:(n) => pred(n)
              |} (y).

def mul : nat * nat -> nat =
    (x,y) => p1
	       ({| zero:()     => (x,zero)
	         | succ:(x, y) => (x,add(x, y))
		 |} (y)
	       ).

def max : nat * nat -> nat =
    (x,y) => { zero()  => y
             | succ(_) => x
             } ((sub(x, y))).

def min : nat * nat -> nat =
    (x,y) => { zero()  => x
             | succ(_) => y
             } ((sub(x, y))).

(* comparisons : nat * nat -> boot *)

def ge : nat * nat -> bool =
    (x,y) => { zero()  => true
             | succ(n) => false
             } ((sub(y, x))).

def le : nat * nat -> bool =
    (x,y) => { zero()  => true
             | succ(n) => false
             } ((sub(x, y))).
(*
def eq(x, y) =
    { zero()  => true
    | succ(n) => false
    }
     ((add(sub(x, y), sub(y, x)))).
*)
(* revised to Robin's more efficient version Sept14,92*)
def eq : nat * nat -> bool = 
    (x,y) => { (flag, zero()) => flag       
	     | (_, succ(_))   => false   
             } ({| zero : ()           => (true,y)
	         | succ : (_, zero())  => (false,zero)
	                | (_, succ(n)) => (true,n)
                 |} (x)).

def gt : nat * nat -> bool =
    (x,y) => not(le(x, y)).

def lt : nat * nat -> bool =
    (x,y) => not(ge(x, y)).

(* list operations *)

def append : list(A) * list(A) -> list(A) =
    (L1, L2) => {| nil :()     => L2
                 | cons:(a, L) => cons(a, L)
                 |} (L1).

def reverse : list(A) -> list(A) =
    L => p0
         ({| nil:() => (nil,L)
           | cons:(dum, (y0, []))            => (y0,[])
                 |(dum, (y0,  cons(a, y1'))) => (cons(a, y0), y1')
           |} (L)
         ).

def length : list(A) -> nat =
    L => {| nil:()          => zero
          | cons:(a, state) => succ(state)
          |} (L).

def exp : nat * nat -> nat = 
    (base, power) => {| zero:()  => succ(zero)
                      | succ:(v) => mul(base, v)
                      |} (power).
