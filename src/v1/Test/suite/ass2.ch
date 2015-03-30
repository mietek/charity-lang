%% Assignment 2 - Allan Dennis 870428

rf "PRELUDE.ch".

%% Question 1 - csublist(L)

(*
sublist1 is a helper function for csublist.  It returns the
new sublists introduced at each step in the fold.
*)
def sublist1: A*list(list(A)) -> list(list(A)) =
    (x,LL) => {| nil: () => [[x]]
               | cons: (L,L') => cons(cons(x,L),L')
               |} LL.

(*
In csublist below, p1 carries the sublists that x can 'cons' to
and still be adjacent
*)
def csublist: list(A) -> list(list(A)) =
    L => p0 {| nil: () => ([[]],[])
             | cons: (x,(L',LastL)) =>
                     (append (sublist1(x,LastL),L'), sublist1(x,LastL))
             |} L.

(*
Question 1 part 2 - returns csublist of a given length.  Uses int for
input instead of nat to avoid having to enter lots of succs for large
numbers, but the function should really only allow unsigned ints!
Also, using lengths greater than the length of the list (or negative!)
returns [], while using length=0 always returns [[]] no matter what the
list is.
*)
def lencsublist: list(A)*int -> list(list(A)) =
    (LL,num) => filter {L => eq (length(L),int_2_nat(num))} csublist (LL).


%% Question 2 - the group function

%% Note the last cons case - it cannot be reached, so it is a dummy case
def group {P: A*A -> bool}: list(A) -> list(list(A)) =
    L => {| nil: () => []
          | cons: (x,[]) => [[x]]
          | cons: (x,cons(cons(y,ys),zs)) =>
                        {false => cons([x],cons(cons(y,ys),zs))
                        | true => cons(cons(x,cons(y,ys)),zs)
                        } P(x,y)
          | cons: (x,_) => []
          |} L.

(* Question 2 part 2 - return number of occurences in a list of strings.
   Because of the different eq functions: eq, eq_int, eq_string,etc., I
   did not think it would be possible to do a polymorphic function.  Mine
   just works on lists of strings.  It takes in sorted lists - I assume
   that Robin meant a sort was 'given' and that writing a sort was not
   part of this question.
*)

def occurences: list(string) -> list(string*int) =
    L => {| nil: () => []
          | cons: (s,[]) => [(s,1)]
          | cons: (s,cons((t,num),ts)) =>
                        {false => cons((s,1),cons((t,num),ts))
                        | true => cons((t,succ_int num),ts)
                        } eq_string(s,t)
          | cons: (s,_) => []
          |} L.

%% Question 3 - return diagonal of inflist(inflist(nat))

data C -> inflist(A) = head: C -> A
                     | tail: C -> C.

def nats: 1 -> inflist(nat) =
    () => (| x => head: x
           |      tail: succ x
           |) zero.

%% A sample table to work with - from notes
def natsnats: 1 -> inflist(inflist(nat)) =
    () => (| x => head: (| y => head: y
                         |      tail: succ y
                         |) x
           |      tail: succ x
           |) zero.

%% Function 'get' given in class or copied from notes
def get: inflist(A)*nat -> A =
    (IL,n) => head {| zero: () => IL
                    | succ: IL' => tail IL'
                    |} n.

def diagonal: inflist(inflist(A)) -> inflist(A) =
    ILL => (| x => head: get(get(ILL,x),x)
            |      tail: succ x
            |) zero.

%% Question 4 - inverse stream function

%% squares is an example for inverse to work on
def squares = () => inflist {x => mul (x,x)} nats.

def inverse: inflist(nat) -> inflist(nat) =
    IL => (| (x,y) => head: x
           |          tail: {  true => (succ x,succ y)
                            | false => (x,succ y)
                            } ge(y,get(IL,succ x))
           |) (zero,succ zero).

%% Question 5 - Infinite diagonal traverse of inflist(inflist(A))

%% First step - onediag produces each diagonal for collectdiags
def onediag: inflist(inflist(A))*nat -> A*list(A) =
   (IL,x)=> p0 {| zero: () => ((get(get(IL,zero),x),[]),(succ zero,pred x))
                | succ: ((a,L),(x,y)) =>
                    ((a,cons(get(get(IL,x),y),L)),(succ x,pred y))
                |} x.

def collectdiags: inflist(inflist(A)) -> inflist(A*list(A)) =
    IL => (| x => head: onediag(IL,x)
           |      tail: succ x
           |) zero.

%% Second step - collect the inflist of pairs and return one inflist

(*
   headntail is a helper function for diagtrav'.  The first case is bogus, as
   is calling it with (A*list(A)) instead of just list(A)...
*)
def headntail: A*list(A) -> A*list(A)
  = (a,[]) => (a,[])
  | (_,cons(x,xs)) => (x,xs).

def diagtrav': inflist(A*list(A)) -> inflist(A) =
    IL => (| ((x,xs),y) => head: x
           |               tail: {  true => (get (IL,succ y),succ y)
                                 | false => (headntail(x,xs),y)
                                 } isEmpty(xs)
           |) (get(IL,zero),zero).

def diagtrav: inflist(inflist(A)) -> inflist(A) =
   ILL => diagtrav'(collectdiags(ILL)).

%% Question 6 - Colists of booleans

data C -> colist(A) = delist: C -> SF(A*C).

data bit -> C = B0 | B1: 1 -> C.

(*
def showbits: colist(bit) -> list(bit) =

def addbits: colist(bit)*colist(bit) -> colist(bit) =

def mulbits: colist(bit)*colist(bit) -> colist(bit) =
*)

%% Question 7 - a pushdown function

rf "ass2test.ch".

