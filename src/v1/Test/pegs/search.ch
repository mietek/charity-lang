rf "lazy-lists.ch".


%  
%  A search problem is expressed by a set of states with a labelled 
%  evolution, together with a goal set.  This is expressed by the charity 
%  datatype:
%

data C -> ProbTree(A) = evolve: C -> Llist(A * C)
                      |   goal: C -> bool.

%
%  The next frontier is determined by the evolution and a filtering 
%  process on paths to remove partial solutions which are not productive.
%

def frontier_step{pred:A * list(A) -> bool}: 
       ProbTree(A) * list(A) -> Llist(ProbTree(A) * list(A))
    = (prob,path) => {|Lnil:() => Lnil
                      |Lcons:(fst:(a,s),snd:LL) => 
                                 { true => Lcons(fst:(s,cons(a,path)),snd:LL)
                                 | false => LL
                                 } pred(a,path)
                      |} evolve prob.

def next_frontier{pred:A * list(A) -> bool}:
  Llist(ProbTree(A) * list(A)) -> Llist(ProbTree(A) * list(A))
    = LL => Lflatten Llist{frontier_step{pred}} LL.

def search{pred:A * list(A) -> bool}: ProbTree(A) -> inflist(SF(list(A)))
    = prob => (| S => tail: next_frontier{pred} S
               |      head: Lfind{ x => { true => ss p1 x
                                        | false => ff
                                        } goal p0 x
                                  } S
               |) Lcons(fst:(prob,[]),snd:Lnil).

def search_at{pred:A * list(A) -> bool}: nat * ProbTree(A) -> SF(list(A))
    = (n,prob) => get(n,search{pred} prob).

def search_to{pred:A * list(A) -> bool}: nat * ProbTree(A) -> SF(list(A))
    = (n,prob) => p1 {| zero: () => (search{pred} prob,ff)
                      | succ: (prob',ff) => (tail prob',head prob')
                            | (prob',v)  => (prob',v)
                      |} n.

