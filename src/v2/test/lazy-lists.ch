rf "basics.ch".

%
%   Lazy products and lists
%

data C -> Lprod(A,B) = fst: C -> A
                     | snd: C -> B.

data Llist(A) -> C =  Lnil: 1 -> C
                   | Lcons: Lprod(A,C) -> C.

def Lhead: Llist(A) -> SF(A)
    = Lnil => ff
    | Lcons(z) => ss(fst(z)).

def Llength: Llist(A) -> nat
    = L => {| Lnil: () => zero
            | Lcons: x => succ(snd(x))
            |}(L).

def Lappend: Lprod(Llist(A),Llist(A)) -> Llist(A)
    = x =>  {|  Lnil: () => snd(x)
             | Lcons: z  => Lcons(z)
             |}(fst(x)).

def Lflatten: Llist(Llist(A)) -> Llist(A)
    = LL => {| Lnil: () => Lnil
             | Lcons: z => Lappend(z)
             |}(LL).

def Lfind{pred: A -> SF(B)}: Llist(A) -> SF(B)
    = L => {| Lnil: () => ff
            | Lcons: z => { ss(z')  => ss(z')
                          | ff => snd(z)
                          }(pred(fst(z)))
            |}(L).

def Lfilter{P: A -> bool}: Llist(A) -> Llist(A)
    = L => {| Lnil: () => Lnil
            | Lcons: v => { true => Lcons(v)
                          | false => snd(v)
                          }(P(fst(v)))
            |}(L).

%
%   Translation routines lists to and from lazy lists 
%

def list_2_Llist: list(A) -> Llist(A)
    = L => {| nil: () => Lnil
            | cons: (a,LL) => Lcons((fst:a,snd:LL))
            |}(L).

def Llist_2_list: Llist(A) -> list(A)
    = LL => {| Lnil: () => nil
             | Lcons: z => cons(fst(z),snd(z))
             |}(LL).


