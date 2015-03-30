rf "search.ch".

%
%  The search problem for peg solitaire.
%
%  Positions on the board are named by:
%

data PEG -> C =
      a0|b0|b1|c0|c1|c2|d0|d1|d2|d3|e0|e1|e2|e3|e4: 1 -> C.

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
    = a0 => 0
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


def PEG_eq: PEG * PEG -> bool
    = (peg1,peg2) => eq_int(PEG_num peg1,PEG_num peg2).


def PEG_geq: PEG * PEG -> bool
    = (peg1,peg2) => ge_int(PEG_num peg1,PEG_num peg2).

(*
def PEG_eq: PEG * PEG -> bool
    = (a0,a0) => true
    | (b0,b0) => true
    | (b1,b1) => true
    | (c0,c0) => true
    | (c1,c1) => true
    | (c2,c2) => true
    | (d0,d0) => true
    | (d1,d1) => true
    | (d2,d2) => true
    | (d3,d3) => true
    | (e0,e0) => true
    | (e1,e1) => true
    | (e2,e2) => true
    | (e3,e3) => true
    | (e4,e4) => true
    | _  => false.

def PEG_geq: PEG * PEG -> bool
    = (a0,a0) => true
    | (a0,_) => false
    | (b0,a0) => true
    | (b0,b0) => true
    | (b0,_) => false
    | (b1,a0) => true
    | (b1,b0) => true
    | (b1,b1) => true
    | (b1,_) => false
    | (c0,a0) => true
    | (c0,b0) => true
    | (c0,b1) => true
    | (c0,c0) => true
    | (c0,_) => false
    | (c1,a0) => true
    | (c1,b0) => true
    | (c1,b1) => true
    | (c1,c0) => true
    | (c1,c1) => true
    | (c1,_) => false
    | (c2,a0) => true
    | (c2,b0) => true
    | (c2,b1) => true
    | (c2,c0) => true
    | (c2,c1) => true
    | (c2,c2) => true
    | (c2,_) => false
    | (e4, _) => true
    | (e3,e4) => false
    | (e3, _) => true
    | (e2,e4) => false
    | (e2,e3) => false
    | (e2, _) => true
    | (e1,e4) => false
    | (e1,e3) => false
    | (e1,e2) => false
    | (e1, _) => true
    | (e0,e4) => false
    | (e0,e3) => false
    | (e0,e2) => false
    | (e0,e1) => false
    | (e0, _) => true
    | (d3,e4) => false
    | (d3,e3) => false
    | (d3,e2) => false
    | (d3,e1) => false
    | (d3,e0) => false
    | (d3, _) => true
    | (d2,e4) => false
    | (d2,e3) => false
    | (d2,e2) => false
    | (d2,e1) => false
    | (d2,e0) => false
    | (d2,d3) => false
    | (d2, _) => true
    | (d1,e4) => false
    | (d1,e3) => false
    | (d1,e2) => false
    | (d1,e1) => false
    | (d1,e0) => false
    | (d1,d3) => false
    | (d1,d2) => false
    | (d1, _) => true
    | (d0,e4) => false
    | (d0,e3) => false
    | (d0,e2) => false
    | (d0,e1) => false
    | (d0,e0) => false
    | (d0,d3) => false
    | (d0,d2) => false
    | (d0,d1) => false
    | (d0, _) => true.
*)
%
%   The possible jumps are indicted by:
%
def jump: PEG * PEG -> SF(PEG)
    = (a0,b0) => ss(c0)
    | (a0,b1) => ss(c2)
    | (b0,c0) => ss(d0)
    | (b0,c1) => ss(d2)
    | (b1,c1) => ss(d1)
    | (b1,c2) => ss(d3)
    | (c0,b0) => ss(a0)
    | (c0,c1) => ss(c2)
    | (c0,d1) => ss(e2)
    | (c0,d0) => ss(e0)
    | (c1,d1) => ss(e1)
    | (c1,d2) => ss(e3)
    | (c2,b1) => ss(a0)
    | (c2,c1) => ss(c0)
    | (c2,d2) => ss(c2)
    | (c2,d3) => ss(e4)
    | (d0,c0) => ss(b0)
    | (d0,d1) => ss(d2)
    | (d1,c1) => ss(b1)
    | (d1,d2) => ss(d3)
    | (d2,c1) => ss(b0)
    | (d2,d1) => ss(d0)
    | (d3,c2) => ss(b1)
    | (d3,d2) => ss(d1)
    | (e0,d0) => ss(c0)
    | (e0,e1) => ss(e2)
    | (e1,d1) => ss(c1)
    | (e1,e2) => ss(e3)
    | (e2,d1) => ss(c0)
    | (e2,d2) => ss(c2)
    | (e2,e1) => ss(e0)
    | (e2,e3) => ss(e4)
    | (e3,d2) => ss(c1)
    | (e3,e2) => ss(e1)
    | (e4,d3) => ss(c2)
    | (e4,e3) => ss(e2)
    | _ => ff.

%
%  To calculate the list of possible moves one must test all the
%  possible jumps.  This means testing each pair of pegs for a jump
%  and collecting the possible jumps and their new state.
%

def filter_2{fun: A * A -> SF(B)}: list(A) -> list(B)
    = LP => flatten flatten
              list{ x => list{ y => { ss b => cons(b,nil)
                                    | ff => nil
                                    } fun(x,y)
                             } LP
                   } LP.

def inboard: PEG * list(PEG) -> bool
    = (z,L) => {| nil: () => false
                | cons: (x,v) => or(PEG_eq(z,x),v)
                |} L.

def new_board: ((PEG * PEG) * PEG) * list(PEG)
                        -> list(PEG)
    = (((x,y),z),L) => cons(z,filter{ v => not or(PEG_eq(v,x)
                                                 ,PEG_eq(v,y))
                                    } L
                           ).

def move: (PEG * PEG) * list(PEG)
                -> SF((PEG * PEG) * list(PEG))
    = (Q,L) => { ss(z) => { false => ss(Q,new_board((Q,z),L))
                          | true => ff
                          } inboard(z,L)
               | ff => ff
               } jump Q.

def moves: list(PEG) -> list((PEG * PEG) * list(PEG))
    = L => filter_2{ x => move(x,L)} L.

def PEG_next_front: list(list(PEG * PEG) * list(PEG))
                             -> list(list(PEG * PEG) * list(PEG))
    = L => flatten list{(Path,St) => list{(m,St')  => (cons(m,Path),St')
                                         } moves St
                       } L.

def SFfind{pass: A -> SF(B)}: list(A) -> SF(B)
    =  L => {| nil: () => ff
             | cons: (a,v) => { ss(b) => ss(b)
                              | ff() => v
                              } pass a
             |} L.

%
%  The goal is to have but one peg left in a particular spot.
%

def peg_solitaire: list(PEG) * PEG -> ProbTree(PEG * PEG)
    = (L,peg) => (| R => evolve: list_2_Llist(moves(R))
                  |        goal: { cons(peg',nil) => PEG_eq(peg,peg')
                                 | _ => false
                                 } R
                  |) L.


def n13 = () => succ(mult(succ(succ(succ(zero))),
          succ(succ(succ(succ(zero)))))).

%
%  The path filtering for peg solitaire is given by insisting that independent
%  moves occur in order.
%

def depend: (PEG * PEG) * (PEG * PEG) -> bool
    =  (x,y) => {ss(v) => or(PEG_eq(v,p0 y),PEG_eq(v,p1 y))
                |ff => false
                } jump x.

def dependent: (PEG * PEG) * (PEG * PEG) -> bool
    =  (x,y) => or(depend(x,y),depend(y,x)).

def peg_filter: (PEG * PEG) * list(PEG * PEG) -> bool
    = ( x, cons(y,_)) => or(PEG_geq(p0 x,p0 y),dependent(x,y))
    | _ => true.

%

(*
search_at{peg_filter}(n13,peg_solitaire([b0,b1,c0,c1,c2,d0,d1,d2,d3,e0,e1,e2,e3,e4]
                          ,a0)).


search_at{peg_filter}(n13,peg_solitaire([a0,b0,b1,c0,c1,c2,d0,d1,d2,d3,e0,e1,e2,e3]
                          ,e4)).

*)

def peg_soln: PEG * list(PEG) -> SF(list(PEG * PEG))
    = (peg,start) => SFfind{ (Path, cons(peg',nil())) => {true() => ss Path
                                                         |false() => ff
                                                         } PEG_eq(peg,peg')
                           |    _                     => ff
                           }  {| zero: () => [([],start)]
                               | succ: z => PEG_next_front z
                               |} n13 .

