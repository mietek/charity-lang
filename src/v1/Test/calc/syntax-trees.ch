(*
 *    Parsing using "recursive syntax diagrams"
 *    Author:  Robin Cockett
 *    Date: 25 Sept '96
 *
 *    Recursive syntax diagrams were invented and used by Wirth to
 *    write the syntax of Pascal.  The way they are defined here
 *    guarantees an LL(1) (one token look ahead grammar)
 *    and it allows a very simple implementation.  Attributes
 *    can be added quite easily after the fact ....
 *)

%  Some utilities

def Id = x => x.

def foldleft{h: C * A -> C}: C * list(A) -> C
    = (c,L)  => fn(c, {|  nil:   ()  => (fn: x => x)
                       | cons: (a,f) => (fn: x => fn(h(x,a),f) )
                       |} L ).

def tail: list(A) -> list(A)
    = nil => nil
    | cons(_,L) => L.


%   Data structures for recursive syntax diagrams
%   After a token is taken in various things can happen
%     (1) The parser can FAIL
%     (2) The parser can decide the token is meant for the next
%         parsing step and it can PASS the token and any structures
%         it has built forward.
%     (3) The parser can eat a token and continue asking for MORE
%     (4) It can recursively call a substructure before continuing
%         with the parse.
%   At any stage in the parse one can ask supply a token (the tok
%   destructor) or see whether one can legally end (the end destructor
%   indicates the final states).

data FOLLOW(A,R,S) -> C =  PASS: A * R -> C
                        |  FAIL: 1 -> C
                        |  MORE: S -> C
                        | RMORE: S * exp(R,S) -> C.

%   Recursive syntax trees
data C -> rS(A,R) = tok: C -> A => FOLLOW(A,R,C)
                  | end: C -> SF(R).

%  An attribute recursive syntax diagrams allows an input attribute to
%  transform the diagram (inherited attributes and synthesized attributes
%  handled in this manner).

data C -> rSA(A,R) = rsa: C -> R => rS(A,R).

%  Two basic attribute recursive syntax diagrams
%  Failure (always fails)
def rSA_NULL: 1 -> rSA(A,R)
    = () => (rsa:_ => (tok: _ => FAIL,end: ff)).

%  Pass (always passes)
def rSA_PASS: 1 -> rSA(A,R)
    = () => (rsa:r => (tok: a => PASS(a,r), end: ss r)).


%  Sequencing recursive syntax tree generators:
%     When the syntax tree of one ends it passes the last
%     token and the result it is building to the next syntax tree
%     generator.

data SUM(A,B) -> C = b_0: A -> C
                   | b_1: B -> C.

def seq: rSA(T,R) * rSA(T,R) -> rSA(T,R)
  = (p,q) => (rsa: r =>
      (| b_0 t => tok: a =>
                { PASS(a',r') => FOLLOW{Id,Id&Id,b_1} tok(a',rsa(r',q))
                | MORE t' => MORE b_0 t'
                | RMORE(t1,c) => RMORE(b_1 t1,(fn: r => b_0 fn(r,c)))
                | FAIL => FAIL
                } tok(a,t)
       |          end: flatten_SF SF{ r => end rsa(r,q) } end t
       | b_1 t => tok: a => FOLLOW{Id,Id&Id,b_1} tok(a,t)
       |          end: end t
       |) b_0 rsa(r,p)
              ).

%  Kleene's star operator: repeating an attribute recursive syntax tree.
%  When the tree passes a symbol immediately it is done!

def star: rSA(T,R) -> rSA(T,R)
    = q => (rsa: r =>
        (| t  =>   tok: a => { PASS(a',r') => tok(a',rsa(r',q))
                             | z => z
                             } tok(a,t)
         |         end: end t
         |) rsa(r,q) ).

%  Alternating over recursive syntax trees:
%  If the first syntax tree fail immediately the second is used.

def alt: rSA(T,R) * rSA(T,R) -> rSA(T,R)
    = (p,q) => (rsa: r => (tok: a => {FAIL => tok(a,rsa(r,q))
                                     | z => z
                                     } tok(a,rsa(r,p))
                          ,end: {ff => end rsa(r,q)
                                | z => z
                                } end rsa(r,p)
                           )
               ).


%  Parsing using an attribute recursive syntax tree:
%      The parsing uses a stack of recursive syntax tree generators which
%      it develops every time it hits a recursive sub-syntax-diagram and
%      pops every time it finishes a parse pushed by a recursive
%      sub-syntax-diagram.

%    This routine presents the PASSed values to each member of the stack
%    until one of the generators does not simply pass it on ...

def use_stack
    = ((a,r),St) => { (MORE T',St) => ss(T',St)
                    | (RMORE(T,fT),St) => ss(T,cons(fT,St))
                    | _ => ff }
          foldleft{ ((PASS(a,r),St),c) => (tok(a,fn(r,c)),tail St)
                  | (z,_)              => z
                  } ((PASS(a,r),St),St).

%     This routine checks that a parse has successfully ended:
%     to do this we must check that what remains on the stack agrees
%     that the current state is a successful end point!

def end_stack
    = (T,St) =>
          foldleft{ (ss r,c) => end fn(r,c)
                  |    _     => ff
                  } (end T,St).

%    The parsing uses a stack (St) to hold the "continuation" syntax trees

def PARSE: rS(A,R) * list(A) -> SF(R) * list(A)
    = (T,L) => { (ss(T,St),L)  => (end_stack(T,St),L)
               | (_, L) => (ff,L) }
   foldleft{ ((ss(T,St),L),a) => { MORE T' => (ss(T',St),tail L)
                                 | RMORE(T,fT) => (ss(T,cons(fT,St)),tail L)
                                 | PASS(a,r) => { ss x => (ss x,tail L)
                                                | ff => (ff,L)
                                                } use_stack((a,r),St)
                                 | FAIL => (ff,L)
                                 } tok(a,T)
            | ((ff,L),a) => (ff,L)
            } ((ss(T,[]),L),L).

