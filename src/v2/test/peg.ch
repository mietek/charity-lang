%
%   success or fail datatype
%

data SF(A) -> C = ss: A -> C
                | ff: -> C.

data C -> exp(A,B) = fn: C ->A=>B.

%
%   The natural numbers
%

data nat -> C = zero: -> C
              | succ: C -> C.

def p0 = x,y => x.
def p1 = x,y => y.

%
%  The list datatype
%

def lhead: list(A) -> SF(A)
    = nil       => ff
    | cons(a,_) => ss a.

def append = L1, L2 => {: app #L1 =
                           nil  => L2
                        |  cons(a,z) => cons(a,app z)
                        :} L1.

def flatten
    = LL => {: flat #x =
               nil  => nil
             | cons(z,zs) => {: app #y = 
	                        nil => flat zs
                              | cons(a,as) => cons(a,app as) 
			      :} z
             :} LL.

def filter{ pred: A -> bool}: list(A) -> list(A)
    = L => {: filt #x =
              nil  => nil
            | cons(a,Ls) => pred a; { true => cons(a, filt Ls)
                                    | false => filt Ls }
            :} L.

%
%  Infinite lists
%

data C -> inflist(A) = head: C -> A
                     | tail: C -> C.

def get: nat , inflist(A) -> A
    = n,L => head {| zero:  => L
                   | succ: LL => tail LL
                   |} n.


%
%   Lazy products and lists
%

data C -> Lprod(A,B) = fst: C -> A
                     | snd: C -> B.

data Llist(A) -> C =  Lnil
                   | Lcons: Lprod(A,C) -> C.

def Lhead: Llist(A) -> SF(A)
    = L=> { Lnil => ff
          | Lcons z => ss fst z
          } L.

def Llength: Llist(A) -> nat
    = L => {| Lnil:  => zero
            | Lcons: x => succ snd x
            |} L.

def Lappend: Lprod(Llist(A),Llist(A)) -> Llist(A)
    = x =>  {|  Lnil:  => snd x
             | Lcons: z  => Lcons z
             |} fst x.

def Lflatten: Llist(Llist(A)) -> Llist(A)
    = LL => {: flat #x =
               Lnil  => Lnil
             | Lcons (fst: a, snd:b) =>
	                {: app #y = 
			   Lnil => flat b
			 | Lcons (fst: a',snd: b') => Lcons(fst: a', snd: app b')
			 :} a
             :} LL.

def Lfind{ pred: A -> SF(B)}: Llist(A) -> SF(B)
    = L => {: find #x =
              Lnil  => ff
            | Lcons z => pred fst z; { ss(z')  => ss(z')
                                     | ff => find snd(z) }
            :} L.

def Lfilter{ P: A -> bool }: Llist(A) -> Llist(A)
    = L => {: filt #x =
              Lnil => Lnil
            | Lcons v => P fst v ; { true => Lcons (fst: fst v, snd: filt snd v)
                                   | false => filt snd v }
            :} L.

%
%   Translation routines lists to and from lazy lists 
%

def list_2_Llist: list(A) -> Llist(A)
    = L => {| nil: => Lnil
            | cons: (a,LL) => Lcons(fst:a,snd:LL)
            |} L.

def Llist_2_list: Llist(A) -> list(A)
    = LL => {| Lnil:  => nil
             | Lcons: z => cons(fst z,snd z)
             |} LL.


%  
%  A search problem is expressed by a set of states with a labelled 
%  evolution, together with a goal set.  This is expressed by the charity 
%  datatype:
%

data C -> ProbTree(A) = evolve: C -> Llist(A*C)
                      |   goal: C -> bool.

%
%  The next frontier is determined by the evolution and a filtering 
%  process on paths to remove partial solutions which are not productive.
%

def frontier_step{ pred }
    = (prob,path) => {: step #x =
                        Lnil => Lnil
                      | Lcons z => fst(z); { a,s => pred(a,path) ;
                                            { true => Lcons(fst: (s,cons(a,path)),snd: step snd(z))
                                            | false => step snd(z) }}
                      :} evolve(prob).

def next_frontier{ pred }
    = LL => Lflatten Llist{ frontier_step{pred}} LL.

def search{pred}
    = prob => (| S => tail: next_frontier{pred} S
               |      head: Lfind{ x => { true => ss p1 x
                                        | false => ff
                                        } goal p0 x
                                  } S
               |) Lcons(fst:(prob,nil),snd:Lnil).

def search_at{pred}
    = (n,prob) => get(n,search{pred} prob).

def search_to{pred}
    = (n,prob) => p1 {| zero: => (search{pred} prob,ff)
                      | succ: x => { prob',y => { ff => (tail prob',head prob')
                                                  | ss(_)  => (prob',y)
                                                  } y
                                       } x
                      |} n.



%
%  The search problem for peg solitaire.
%
%  Positions on the board are named by:
%

data PEG -> C = a0|b0|b1|c0|c1|c2|d0|d1|d2|d3|e0|e1|e2|e3|e4.

%
%  The board layout is as follows:
%
%            a0
%
%          b0  b1
%
%        c0  c1  c2
%
%      d0  d1  d2  d3
%
%    e0  e1  e2  e3  e4
%
%  The normal starting state will be
%      [b0,b1,c0,c1,c2,d0,d1,d2,d3,e0,e1,e2,e3,e4]
%  a good test starting state is ..
%      [d1,e2,c1,d2]
%  another is
%      [a0,c1,c2,d1,d2,d3]
%  yet another is
%      [a0,b0,b1,c1,d0,d1,d2,d3,e1,e3,e4]
%

def PEG_num: PEG -> int
   =  a0 => 0
    | b0 => 1
    | b1 => 2
    | c0 => 3
    | c1 => 4
    | c2 => 5
    | d0 => 6
    | d1 => 7
    | d2 => 8
    | d3 => 9
    | e0 => 10
    | e1 => 11
    | e2 => 12
    | e3 => 13
    | e4 => 14.


%
%   The possible jumps are indicted by:
%



def jump: PEG, PEG -> SF(PEG)
    = a0,b0 => ss(c0)
    | a0,b1 => ss(c2)
    | b0,c0 => ss(d0)
    | b0,c1 => ss(d2)
    | b1,c1 => ss(d1)
    | b1,c2 => ss(d3)
    | c0,b0 => ss(a0)
    | c0,c1 => ss(c2)
    | c0,d1 => ss(e2)
    | c0,d0 => ss(e0)
    | c1,d1 => ss(e1)
    | c1,d2 => ss(e3)
    | c2,b1 => ss(a0)
    | c2,c1 => ss(c0)
    | c2,d2 => ss(c2)
    | c2,d3 => ss(e4)
    | d0,c0 => ss(b0)
    | d0,d1 => ss(d2)
    | d1,c1 => ss(b1)
    | d1,d2 => ss(d3)
    | d2,c1 => ss(b0)
    | d2,d1 => ss(d0)
    | d3,c2 => ss(b1)
    | d3,d2 => ss(d1)
    | e0,d0 => ss(c0)
    | e0,e1 => ss(e2)
    | e1,d1 => ss(c1)
    | e1,e2 => ss(e3)
    | e2,d1 => ss(c0)
    | e2,d2 => ss(c2)
    | e2,e1 => ss(e0)
    | e2,e3 => ss(e4)
    | e3,d2 => ss(c1)
    | e3,e2 => ss(e1)
    | e4,d3 => ss(c2)
    | e4,e3 => ss(e2)
    | _,_ => ff.


%
%  To calculate the list of possible moves one must test all the
%  possible jumps.  This means testing each pair of pegs for a jump
%  and collecting the possible jumps and their new state.
%

def filter_2{fun}: list(A) -> list(B)
    = LP => flatten flatten
              list{ x => list{ y => { ss(b) => cons(b,nil)
                                    | ff => nil
                                    } fun(x,y)
                             } LP
                   } LP.

def inboard
    = z,L =>   {: member #x = 
                  nil => false
                | cons(x,v) => (z==x); { true  => true
                                       | false => member v}
                :} L.

def new_board
    = x,y,z,L => cons(z,filter{ v => v<>x and v<>y } L ).

def move
    = Q,L => jump Q ; { ss(z) => inboard(z,L) ; { false => ss(Q, new_board(p0 Q, p1 Q, z ,L))
                                                | true => ff }
                      | ff => ff }.

def moves
    = L => filter_2{ x => move(x,L)} L.

def PEG_next_front
    = L => flatten list{Path,St => list{m,St'  => (cons(m,Path),St')
                                         } moves St
                       } L.

def SFfind{pass}
    =  L => {: find #x = nil  => ff
                      | cons(a,v) => pass a; { ss(b) => ss(b)
                                             | ff => find v }
             :} L.

%
%  The goal is to have but one peg left in a particular spot.
%

def peg_solitaire
    = L,peg => (| R => evolve: list_2_Llist moves R
                |        goal: R; { cons(peg',nil) => peg==peg'
                                  | _ => false }
                |) L.


def n13 = => succ succ succ succ succ succ succ succ succ succ succ succ succ zero.

%
%  The path filtering for peg solitaire is given by insisting that independent
%  moves occur in order.
%

def depend
    =  x,y => jump x ; { ss(v) => v==(p0 y) or v==(p1 y)
                       | ff    => false }.

def dependent
    =  x,y => depend(x,y) or depend(y,x).

def peg_filter
    = x, lst => lst ;  { nil => true
                       | cons(y,_) => (PEG_num(p0 x)>=PEG_num(p0 y)) or dependent(x,y)
                       }.

def start = => search_at{peg_filter}(n13,
             peg_solitaire([b0,b1,c0,c1,c2,d0,d1,d2,d3,e0,e1,e2,e3,e4], b1)).

