#include <basics.ch>

(******************************************************************************

knuth.ch

Author: Marc A. Schroeder
        marc@cpsc.ucalgary.ca

        Dept. of Computer Science
        The University of Calgary, Canada

Version 1.0

Last Update: 24 Aug. 1992

DESCRIPTION:

The following Charity program tests an attribute grammar, taken as input,
and returns true if that grammar is well defined, or false if not.

The program is invoked with:

        WellDef(alphabet, productions).

For an explanation of attribute grammars, and the algorithm which is used
here, see the following paper:

        Semantics of Context-Free Grammars
        by Donald E. Knuth

        Journal: Mathematical Systems Theory, Vol. 2, No. 2 (1968)

For a complete description of how said algorithm is implemented here as a
Charity program, see:

        Testing Attribute Grammars in Charity
        by Marc A. Schroeder

        (unpublished)

******************************************************************************)

def StartOk(al) =
    { nil         => false
    | cons(el, _) => not(p1(el))
    }
      (al).

def ProdsForNonterms(al, pr) =
        p1({| nil  : ()                    => (succ(zero), true)
            | cons : (el, (index, result)) =>
                                              if result then
                                                   if p1(el) then
                                                        (succ(index), true)
                                                   else
                                                        (succ(index), {| nil  : ()                 => false
                                                                       | cons : (prod, foundOnLHS) =>
                                                                                                      if foundOnLHS then
                                                                                                           true
                                                                                                      else
                                                                                                           { nil               => false
                                                                                                           | cons((lhs, _), _) => eq(index, int_2_nat(lhs))
                                                                                                           }
                                                                                                             (prod)
                                                                       |}
                                                                          (pr)
                                                        )
                                              else
                                                   (index, false)
            |}
               (reverse(al))
          ).

def TestAlpha(al, pr) =
        if StartOk(al) then
             ProdsForNonterms(al, pr)
        else
             false.

def TerminalList(al) =
        p1({| nil  : ()                    => (1, [])
            | cons : (el, (index, result)) =>
                                              if p1(el) then
                                                   (succ_int(index), cons(index, result))
                                              else
                                                   (succ_int(index), result)
            |}
               (reverse(al))
          ).

def NoExtraSymbols(numSymb, sy) =
        and(not(eq_int(sy, 0)), le_int(sy, numSymb)).

def IOk(sy, (i, al)) =
        if eq_int(i, 0) then
             false
        else
             p1({| nil  : ()                    => (1, true)
                 | cons : (el, (index, result)) =>
                                                   if result then
                                                        if eq_int(sy, index) then
                                                             if le_int(i, p0(el)) then
                                                                  (succ_int(index), true)
                                                             else
                                                                  (index, false)
                                                        else
                                                             (succ_int(index), true)
                                                   else
                                                        (index, false)
                 |}
                    (reverse(al))
               ).

def I1Ok(i1, prod) =
        lt_int(i1, nat_2_int(length(prod))).

def I2Ok((i1, prod), (i2, al)) =
        if eq_int(i2, 0) then
             false
        else
             p1({| nil  : ()                    => (0, true)
                 | cons : (el, (index, result)) =>
                                                   if result then
                                                        if eq_int(i1, index) then
                                                             (succ_int(index), p1({| nil  : ()                       => (1, true)
                                                                                   | cons : (el2, (index2, result2)) =>
                                                                                                                        if result2 then
                                                                                                                             if eq_int(index2, p0(el)) then
                                                                                                                                  (succ_int(index2), le_int(i2, p0(el2)))
                                                                                                                             else
                                                                                                                                  (succ_int(index2), true)
                                                                                                                        else
                                                                                                                             (index2, false)
                                                                                   |}
                                                                                      (reverse(al))
                                                                                 )
                                                             )
                                                        else
                                                             (succ_int(index), true)
                                                   else
                                                        (index, false)
                 |}
                    (reverse(prod))
               ).

def ValidDependencies(depList, (sy, (al, prod))) =
        {| nil  : ()                     => true
         | cons : ((i, (i1, i2)), valid) =>
                                            if valid then
                                                 if IOk(sy, (i, al)) then
                                                      if I1Ok(i1, prod) then
                                                           I2Ok((i1, prod), (i2, al))
                                                      else
                                                           false
                                                 else
                                                      false
                                            else
                                                 false
         |}
            (depList).

def Member{EqFunc}(el, lst) =
        {| nil  : ()           => false
         | cons : (this, rest) =>
                                  if rest then
                                       true
                                  else
                                       EqFunc(el, this)
         |}
            (lst).

def NoTerminalDerivations(terminals, sy) =
        not(Member{x => eq_int(x)}(sy, terminals)).

def NoRecursiveStart(sy) =
        not(eq_int(1, sy)).

def TestProdEl(numSymb, (terminals, (index, (al, (prod, (sy, depList)))))) =
        if NoExtraSymbols(numSymb, sy) then
             if ValidDependencies(depList, (sy, (al, prod))) then
                  if eq_int(index, 0) then
                       NoTerminalDerivations(terminals, sy)
                  else
                       NoRecursiveStart(sy)
             else
                  false
        else
             false.

def TestProd(numSymb, (terminals, (al, prod))) =
        p1({| nil  : ()                       => (0, true)
            | cons : (prodEl, (index, valid)) =>
                                                 if valid then
                                                      (succ_int(index), TestProdEl(numSymb, (terminals, (index, (al, (prod, prodEl))))))
                                                 else
                                                      (index, false)
            |}
               (reverse(prod))
          ).

def TestProds(numSymb, (terminals, (al, pr))) =
        {| nil  : ()           => true
         | cons : (prod, rest) =>
                                  if rest then
                                       TestProd(numSymb, (terminals, (al, prod)))
                                  else
                                       false
         |}
            (pr).

def SimpleGrammar(pr) =
        list{prod => list{(sy, _) => int_2_nat(sy)}(prod)}(pr).

def StripSy(sy, derv) =
        {| nil  : ()         => []
         | cons : (el, rest) =>
                                if eq(el, sy) then
                                     rest
                                else
                                     cons(el, rest)
         |}
            (derv).

def LHS(prod) =
        { nil          => zero
        | cons(lhs, _) => lhs
        }
          (prod).

def RHS(prod) =
        { nil          => []
        | cons(_, rhs) => rhs
        }
          (prod).

def GetDerv(sy, (derv, (done, prods))) =
        {| nil  : ()            => StripSy(sy, derv)
         | cons : (prod, derv') =>
                                   if eq(sy, LHS(prod)) then
                                        {| nil  : ()              => derv'
                                         | cons : (newSy, derv'') =>
                                                                      if and(not(eq(sy, newSy)), and(not(Member{x => eq(x)}(newSy, derv'')), not(Member{x => eq(x)}(newSy, done)))) then
                                                                           cons(newSy, derv'')
                                                                      else
                                                                           derv''
                                         |}
                                            (RHS(prod))
                                   else
                                        derv'
         |}
            (prods).

def GetProds(sy, prods) =
        {| nil  : ()             => []
         | cons : (prod, result) =>
                                    if eq(sy, LHS(prod)) then
                                         result
                                    else
                                         cons(prod, result)
         |}
            (prods).

def TestDerivations'(bound, pr) =
        p0(p1({| zero : ()                    => ([succ(zero)], ([], SimpleGrammar(pr)))
               | succ : (derv, (done, prods)) =>
                                                 {| nil  : ()                             => (derv, (done, prods))
                                                  | cons : (sy, (derv', (done', prods'))) => (GetDerv(sy, (derv', (done', prods'))), (cons(sy, done'), GetProds(sy, prods')))
                                                  |}
                                                     (derv)
               |}
                  (bound)
             )
          ).

def TestDerivations(lenAl, pr) =
        eq(lenAl, length(TestDerivations'(lenAl, pr))).

def InputOk(al, pr) =
        if TestAlpha(al, pr) then
             if TestProds(nat_2_int(length(al)), (TerminalList(al), (al, pr))) then
                  TestDerivations(length(al), pr)
             else
                  false
        else
             false.

def GetDepGraph(prod) =
        p1({| nil  : ()                               => (zero, [])
            | cons : ((_, depList), (prEl, depGraph)) =>
                                                         (succ(prEl), append(depGraph, {| nil  : ()                 => []
                                                                                        | cons : ((i, (i1, i2)), e) => cons(((int_2_nat(i1), int_2_nat(i2)), (prEl, int_2_nat(i))), e)
                                                                                        |}
                                                                                           (depList)
                                                                            )
                                                         )
            |}
               (reverse(prod))
          ).

def GetProdData(al, pr) =
        {| nil  : ()               => []
         | cons : (prod, prodList) => cons((GetDepGraph(prod), list{(s, d) => (zero, int_2_nat(s))}(reverse(prod))), prodList)
         |}
            (pr).

def Sqr(n) =
        mul(n, n).

def Exp2(n) =
        {| zero : ()     => succ(zero)
         | succ : (prev) => mul(prev, succ(succ(zero)))
         |}
            (n).

def GetBound(al) =
        {| nil  : ()                  => zero
         | cons : ((numAttr, _), sum) => add(Exp2(Sqr(int_2_nat(numAttr))), sum)
         |}
            (al).

def GetDepSets(al) =
        {| nil  : ()           => []
         | cons : (this, rest) =>
                                  if p1(this) then
                                       cons([[]], rest)
                                  else
                                       cons([], rest)
         |}
            (reverse(al)).

def BuildData(al, pr) =
        (GetProdData(al, pr), (GetBound(al), GetDepSets(al))).

def GetNewProd(depGraph, (prod, sets)) =
        (depGraph, {| nil  : ()                 => []
                    | cons : ((_, sy), newProd) =>
                                                   cons((p1({| nil  : ()                  => (succ(zero), zero)
                                                             | cons : (st, (index, size)) =>
                                                                                             if eq(index, sy) then
                                                                                                  (succ(index), length(st))
                                                                                             else
                                                                                                  (succ(index), size)
                                                             |}
                                                                (sets)
                                                           ), sy), newProd)
                    |}
                       (prod)
        ).

def NewGraphs(last, (st, new)) =
    if new then
        true
    else
        gt(length(st), last).

def TestSet(last, (st, new)) =
    ({ nil  => false
     | cons => true
     }
       (st)
     ,
     NewGraphs(last, (st, new))
    ).

def Symbol2Set(sy, sets) =
    p1({| nil  : ()                    => (succ(zero), [])
        | cons : (currSt, (index, st)) => if eq(index, sy) then
                                              (succ(index), currSt)
                                          else
                                              (succ(index), st)
        |}
           (sets)
      ).

def AttemptsExist(prod, sets) =
    and(p0({| nil  : ()                                   =>
                  ((true, false), true)
            | cons : ((last, sy), ((nonEmpty, new), lhs)) =>
                  if lhs then
                      ((nonEmpty, new), false)
                  else
                      if nonEmpty then
                          (TestSet(last, (Symbol2Set(sy, sets), new)), false)
                      else
                          ((false, new), false)
            |}
               (prod)
          )
       ).

(*****

Function: PermuteGraphs



*****)

def PermuteGraphs(listOfList) =
     { nil  => []

     | cons => {| nil  : ()
                      => [[]]

                | cons : (graphs, permutes)
                      => {| nil  : ()
                                => []

                          | cons : (graph, prev)
                                => append({| nil  : ()
                                                 => []

                                           | cons : (perm, rest)
                                                 => cons(cons(graph, perm), rest)
                                           |}
                                              (permutes)
                                         ,
                                          prev
                                         )
                          |}
                             (graphs)
                |}
                   (listOfList)
     }
       (listOfList).

(*****

Function: New

New takes a natural, 'n', and a set, 'st'. It will ignore the first n
graphs in the set, and return the rest in a list.

*****)

def New(n, st) =
     p1({| nil  : ()
               => (succ(zero), [])

         | cons : (graph, (index, newGraphs))
               => if gt(index, n) then
                       (succ(index), cons(graph, newGraphs))
                  else
                       (succ(index), newGraphs)
         |}
            (st)
       ).

(*****

Function: Old

Old takes a natural, 'n', and a set, 'st'. It will return the first n
graphs in the set in a list, and ignore the rest.

*****)

def Old(n, st) =
     p1({| nil  : ()
               => (succ(zero), [])

         | cons : (graph, (index, oldGraphs))
               => if le(index, n) then
                       (succ(index), cons(graph, oldGraphs))
                  else
                       (succ(index), oldGraphs)
         |}
            (st)
       ).

(*****

Function: GraphsToUse

Based on which invokation of p we are doing, and which symbol of the
right hand side of the production we are refering to, GraphsToUse returns
either o(i), n(i), or s(i), for the given set, 'st'. Note that s(i) is
simply 'st', while o(i) and n(i) are computed with the Old and New calls,
respectivley. See Attempts.

*****)

def GraphsToUse((i, i'), (last, st)) =
     if gt(i, i') then
          Old(last, st)
     else
          if lt(i, i') then
               st
          else
               New(last, st).

(*****

Function: Cons

This function cons' a list of graphs to a list of list of graphs. However,
if the list of graphs is empty, we set an error flag, as needed by the
MoreAttempts function.

*****)

def Cons(graphsToUse, listOfGraphs) =
     { nil  => ([], true)
     | cons => (cons(graphsToUse, listOfGraphs), false)
     }
       (graphsToUse).

(*****

Function: MoreAttempts

MoreAttempts constructs each p([...]) (see Attempts). For each of the m
symbols on the right hand side of 'prod', we take the appropriate list
of graphs (either o(i), n(i), or s(i), based which symbol we are at, and
how many invokations of MoreAttempts have been made, based on 'index'),
and cons that list to a list of list of graphs.

Finally, that list of list of graphs is permutted with the p function,
PermuteGraphs.

Note that the current invokation of MoreAttempts is short circuited, and
returns an empty list, if any o(j), n(j), or s(j) is empty. This is
accomplished through the 'ss' flag, and the Cons call.

*****)

def MoreAttempts(index, (prod, sets)) =
     PermuteGraphs(p0(p1({| nil  : ()
                                => (zero, ([], false))

                          | cons : ((last, sy), (index', (listOfGraphs, ss)))
                                => if ss then
                                        (index', ([], true))
                                   else
                                        if eq(index', zero) then
                                             (succ(index'), (listOfGraphs, ss))
                                        else
                                             (succ(index')
                                             ,
                                              Cons(GraphsToUse((index, index')
                                                              ,
                                                               (last, Symbol2Set(sy, sets))
                                                              )
                                                  ,
                                                   listOfGraphs
                                                  )
                                             )
                          |}
                             (prod)
                        )
                     )
                  ).

(*****

Function: Attempts

Attempts generates a list of lists of graphs. Each list of graphs will be used
in a call to Paste. These are the graphs that we overlay on top of the
dependancy graph for the given production, 'prod' (see Paste).

Each time we examine a given production, as we are doing Knuth's algorithm, we
are attempting to enlarge the set of graphs corresponding to the symbol on the
left hand side of that production (each symbol in the grammar has an associated
set of graphs). This is done by attempting to add new graphs to that set. We
find new graphs by taking the dependancy graph for the given production, pasting
on a list of other graphs (over the same set of vertices as the dependancy
graph), and performing other operations on the new graph that is generated (see
the TransClose, CycleTest, Strip, Grow, GrowSet, GrowSets, Knuth, and Paste
algorithms for more details on how Knuth's algorithm functions).

The "list of other graphs" mentionned above is an element of the list
generated by this function. Each element is one possible list of graphs which
we will attempt, and which has not yet been tried before. There may be many
such attempts possible for each call to Attempts. There is exactly one graph
in a list for each symbol on the right hand side of a production. This graph
is taken from the set of graphs for the symbol at hand. Further, these graphs
are in order within the list (the same order that the symbols are in in the
production. The Paste function requires this (see Paste).

Each production carries accounting information with it: a natural number for
each symbol in the production. This number indicates the number of graphs in
the set of graphs for the given symbol, the last time Attempts was called.
Thus, we know if these sets, in turn, were enlarged. So new graphs can be
used in combination with other graphs from other sets, to get new possible
attempts. Accounting information is updated in GetNewProd.

If the given production has m symbols on its right hand side, then we say
the set corresponding to the i'th symbol on the right hand side is s(i).
It can be partitionned into two disjoint subsets, o(i) (the set of old
graphs), and n(i) (the set of new graphs). The following construction
illustrates how Attempts generates its output:

     append( p([n(1), s(2), s(3), ..., s(m)]),
             p([o(1), n(2), s(3), ..., s(m)]),
             p([o(1), o(2), n(3), ..., s(m)]),
             .
             .
             .
             p([o(1), o(2), o(3), ..., n(m)])
           )

That is, we can encode each s(i), o(i), and n(i) as a list of graphs. Hence,
we group m of these into a list of list of graphs, according to the above
pattern. 'p' is a function which takes this list, and returns a list of
list of graphs, each having m graphs. 'p' is actually PermuteGraphs, which is
called from MoreAttempts. By calling p m times, and appending all the results
together, it can be seen that we are generating a list of attempts, where each
attempt is a brand new combination of graphs from the sets for symbols on the
right hand side of the production.

If any sets o(j), or n(j), for any 1 <= j <= m, for a given invokation of p,
are empty, then that call to p simply returns an empty list.

Attempts simply steps through the m calls to p, appending all the results
together. MoreAttempts actually handles the individual calls to p.

Of course, this whole procedure need not even be attempted if any relevant
set of graphs is empty, or if no relevant sets have any new graphs.
AttemptsExist puts a stop to Attempts, and returns an empty list of attempts,
in this case.

*****)

def Attempts(prod, sets) =
     if AttemptsExist(prod, sets) then
          p1({| nil  : ()
                    => (zero, [])

              | cons : (_, (index, attempts))
                    => if eq(index, zero) then
                            (succ(index), attempts)
                       else
                            (succ(index)
                            ,
                             append(attempts, MoreAttempts(index, (prod, sets)))
                            )
              |}
                 (prod)
            )
     else
          [].

(*****

Function: EqEdge

This function simply returns true if the two given edges are identical,
othersise false is returned.

*****)

def EqEdge(((prodEl1, a1), (prodEl2, a2)), ((prodEl3, a3), (prodEl4, a4))) =
     if eq(prodEl1, prodEl3) then
          if eq(prodEl2, prodEl4) then
               if eq(a1, a3) then
                    eq(a2, a4)
               else
                    false
         else
              false
     else
          false.

(*****

Function: Paste

The Paste algorithm actually splices, or pastes, two or more graphs together.

As we perform Knuth's "well defined" algorithm, we periodically need to take
the dependancy graph for a given production, and overlay other graphs (who's
vertices are a subset of the vertices of the dependancy graph, but which may
have a totally different set of edges). The algorithm simply takes each edge
in each of the graphs, and adds it to the dependancy graph if it is not
already present, to create a new, "spliced" graph.

Because in this implementation we represent a graph as a list of edges, we
simply take the dependancy graph, and for each graph in the list of graphs
to add, for each element in that list, cons it to the dependancy graph if
it is not already present.

Note that each edge in the dependancy graph is a pair of vertices, each
vertex being a pair specifying which symbol in the production it refers to,
as well as which attribute of that symbol. However, the graphs in the list
of graphs to add are only graphs relating to specific symbols (ie. they are
taken from the list of sets for symbols, indirectly). Thus, the edges are
only pairs, giving the source attribute and the destination attribute for
an edge. Consequently, when we add these edges to the dependancy graph, we
must add in the sybol information. This is done by relying on the guarantee
that the graphs in 'graphList' are ordered, such that each graph in the list
is in the correct position relative to the corresponding sybol in the
production. This is assured by the Attempts function, which prepares these
lists of graphs.

*****)

def Paste(depGraph, graphList) =
     p1({| nil  : ()
               => (succ(zero), depGraph)

         | cons : (graph, (index, edges))
               => (succ(index)
                  ,
                   {| nil  : ()
                          => edges

                    | cons : ((a1, a2), edges')
                          => if Member{x => EqEdge(x)}(((index, a1), (index, a2)), edges') then
                                  edges'
                             else
                                  cons(((index, a1), (index, a2)), edges')
                    |}
                       (graph)
                  )
         |}
            (graphList)
       ).

(*****

Function: flatten_list

Function flatten_list is used by the pairs function to take a list of lists,
and return them all as one flat list.

*****)

def flatten_list(L) =
     {| nil  : ()     => []
      | cons : (a, L) => append(a, L)
      |}
         (L).

(*****

Function: pairs

Given two lists, pairs returns a list of all pairs of elements from those two
lists.

*****)

(*

def pairs(z0, z1) =
     flatten_list(list{x0 => list{x1 => (x0,x1)}(z1)}(z0)).

*)

(*****

Function: Compatible

Compatible takes two edges, and returns true if the first edge goes to the
vertex that the second edge goes from, otherwise it returns false.

*****)

def Compatible((_, (ProdEl1, a1)), ((ProdEl2, a2), _)) =
     if eq(ProdEl1, ProdEl2) then
          eq(a1, a2)
     else
          false.

(*****

Function: NewEdge

If there is an edge from vertex A to vertex B, and one from vertex B to
vertex C, NewEdge can be used to return a new edge from A to C.

*****)

def NewEdge(((ProdEl1, a1), _), (_, (ProdEl2, a2))) =
     ((ProdEl1, a1), (ProdEl2, a2)).

(*****

Function: TransClose

This algorithm generates the transitive closure of the given graph. It iterates
over the number of edges in the initial graph, modifying a copy of the graph
for each interation, and passing that new version on to the next iteration (or
returning it if the iterations are up).

For each intermediary graph, we take each possible pair of edges. If the pair
is compatible (ie. if the first edge goes to the vertex that the second goes
from), then we add that new edge (ie. a new transitive hop) to the new graph,
if that edge is not already present.

We know we will have found the transitive closure after iterating over all
edges in the initial graph, since we can see that it only infact takes lg(n)
iterations to find the transitive closure. This is because for each iteration,
if there is a path from vertex A to vertex B, the process of adding an edge
for each two consecutive edges in that path will half the distance between A
and B. That is, we will have added a transitive jump from each A to B (as
long as B is reachable from A) after lg(length(path)) iterations. In the worst
case, the longest possible path has as many edges as are present in the
initial graph itself.

It is clear that this algorithm is not optimal. It will be improved in the
next update.

*****)

def TranClose(graph) =
     {| nil  : ()
            => graph

      | cons : (_, graph')
            => {| nil  : ()
                      => graph'

                | cons : (pair, graph'')
                      => if Compatible(pair) then
                              if not(Member{x => EqEdge(x)}(NewEdge(pair), graph'')) then
                                   cons(NewEdge(pair), graph'')
                              else
                                   graph''
                         else
                              graph''
                |}
                   (pairs(graph', graph'))
      |}
         (graph).

(*****

Function: Strip

Since Strip's input is CycleTest's output, it takes a 'graph' and a 'cyclic'
flag. It must pass 'cyclic' back out unchanged, for the sake of Grow, but
it must remove any extraneous information from 'graph'.

Each edge in the graph is an ordered pair of ordered pairs. The first ordered
pair is the source of an edge, the second is the destination (in other words,
each is a vertex in the graph). Each vertex has a first element (a natural
number) which represents which symbol of the production we are talking about,
and a second element which represents which attribute of the given symbol we
are talking about (that is, a vertex in an unstripped graph is represented
by a symbol, and an attribute of that symbol).

In a stripped graph, we only want edges who's vertices both belong to the
symbol on the left hand side of the pertinent production. Thus, each edge
in a stripped graph need only be an ordered pair of naturals, each denoting
a vertex, and each indicating an attribute of the given symbol.

That is, once a graph has been stripped, its edges represent the inter-
dependance of a symbol's attributes on themselves. It is this type of
graph which we try to enlarge the sets with.

*****)

def Strip(graph, cyclic) =
     ({| nil  : ()
             => []

       | cons : (((s1, a1), (s2, a2)), rest)
             => if and(eq(zero, s1), eq(zero, s2)) then
                     cons((a1, a2), rest)
                else
                     rest
       |}
          (graph)
     ,
      cyclic
     ).

(*****

Function: WhichSet

WhichSet looks at the first symbol in a production (which is always the
element of the list which points to nil), and returns that symbol. Note
that the symbol is simply a natural number, 'sy', which indicates the
index of the corresponding set in the list of sets.

*****)

def WhichSet(prod) =
     { nil              => zero
     | cons((_, sy), _) => sy
     }
       (reverse(prod)).

(*****

Function: CycleTest

This function is given a 'graph', and must return a copy of that graph
unaltered, with a flag which indicates if a cycle is present. This is
accomplished by looking at each edge in the graph (which is just a list
of edges), and determining if that edge points back on itself. If any
one edge does, the graph is cyclic.

*****)

def CycleTest(graph) =
     (graph, {| nil  : ()
                    => false

              | cons : (((p1, a1), (p2, a2)), cyclic)
                    => if cyclic then
                            true
                       else
                            and(eq(p1, p2), eq(a1, a2))
              |}
                 (graph)
     ).

(*****

Function: Enlarge

Enlarge is called by Grow to add a 'graph' to the set with the given index
'setNum', in 'sets''. It rebuilds 'sets'' from scratch, adding 'graph' to
the appropriate set.

*****)

def Enlarge(setNum, (sets', graph)) =
     p1({| nil  : ()
               => (succ(zero), [])

         | cons : (st, (index, sets))
               => if eq(setNum, index) then
                       (succ(index), cons(cons(graph, st), sets))
                  else
                       (succ(index), cons(st, sets))
         |}
            (sets')
       ).

(*****

Function: EqEdge'

EqEdge' determines if two edges are equal, in a graph which has
already been stripped.

*****)

def EqEdge'((a1, a2), (a3, a4)) =
     if eq(a1, a3) then
          eq(a2, a4)
     else
          false.

(*****

Function: Member'

Member' returns an ordered pair. The second element tells us if the
element 'el' was found in the list 'lst''. The first element is a
list, identical to 'lst'', except that it will have one element
stripped from it if 'el' is a member of 'lst''. That element which
is stripped is the instance of 'el' that we are looking for (this
ensures the uniquness property in EqGraph is satisfied, when matching
up edges from one graph to the other).

*****)

def Member'{EqFunc}(el, lst') =
     {| nil  : ()                  => ([], false)
      | cons : (this, (lst, rest)) => if rest then
                                           (cons(this, lst), rest)
                                      else
                                           if EqFunc(el, this) then
                                                (lst, true)
                                           else
                                                (cons(this, lst), false)
      |}
         (lst').

(*****

Function: EqGraph

If two graphs have a different number of edges, we know right away that they
are not equal, and return false. Otherwise, for each edge in the first graph,
we check the second graph to see if a unique copy of that edge is also present.
Thus, if the two graphs have the same number of edges, and each edge in 'g1'
corresponds to an edge in 'g2', we know the graphs are equal.

*****)

def EqGraph(g1, g2') =
     if eq(length(g1), length(g2')) then
          p1({| nil  : ()
                    => (g2', true)

              | cons : (edge, (g2, sameSoFar))
                    => if sameSoFar then
                            Member'{x => EqEdge'(x)}(edge, g2)
                       else
                            (g2, sameSoFar)
              |}
                 (g1)
            )
     else
          false.

(*****

Function: SetMember

SetMember simply examines each graph in the set (a set is just
a list of graphs), and returns true is the given 'graph' equals
some other graph already present in the list.

*****)

def SetMember(graph, st) =
     {| nil  : ()                 => false
      | cons : (setGraph, member) => if member then
                                          member
                                     else
                                          EqGraph(graph, setGraph)
      |}
         (st).

(*****

Function: Enlargeable

This function determines if the set with a given index, 'setNum',
in the list of 'sets', can be enlarged by 'graph', by checking
to see if that graph is a member of that set (call SetMember).

*****)

def Enlargeable(setNum, (sets, graph)) =
     p1({| nil  : ()
               => (succ(zero), false)

         | cons : (st, (index, result))
               => if eq(setNum, index) then
                       (succ(index), not(SetMember(graph, st)))
                  else
                       (succ(index), result)
         |}
            (sets)
       ).

(*****

Function: Grow

Grow takes a newly constructed graph, a boolean flag indicating if that
graph is cyclic, the list of sets, the done flag, and the index of the
set to be enlarged.

If the graph is cyclic, then we return a set 'done' flag and a set 'cyclic'
flag (ie. the grammar is NOT well defined), and enlarge the given set
by the new graph (since it must be new). Otherwise, if this graph is not
already in the set to be enlarged, enlarge it. Thus we know that we are
not yet done, and the grammar has not yet been found to be cyclic.
However, if we can't enlarge the given set, we returns 'sets' in their
original state, leave the 'done' flag alone (it may either be true or
false), and return a false 'cyclic' flag.

*****)

def Grow((graph, cyclic), (sets, (done, setNum))) =
     if cyclic then
          (Enlarge(setNum, (sets, graph)), (true, true))
     else
          if (Enlargeable(setNum, (sets, graph))) then
               (Enlarge(setNum, (sets, graph)), (false, false))
          else
               (sets, (done, false)).

(*****

Function: GrowSet

GrowSet attempts to enlarge the set corresponding to the symbol on the left
hand side of the given production that is passed in (a production is passed
in as an ordered pair: the dependency graph for that production, 'depGraph',
and the syntactic and accounting information contained in the 'prod' list).
This is accomplished with a call to Grow, which takes a newly constructed
graph, the 'sets', the 'done' flag, and the index of the set to be enlarged,
which is identified with a call to 'WhichSet'. The newly constructed graph is
obtained by pasting an 'attempt' graph to 'depGraph', finding the
transitive closure, testing for a cycle in the closure graph (if one is found,
the grammar is not well defined), and stripping away all unneeded information
from the closure graph.

Note: (1) For a given production, there may be many possible new attempts
          to try, returned by Attempts, and so we do them all.
      (2) GrowSet must also pass back an updated production for accounting
          purposes, and thus calls GetNewProd to accomplish this.

*****)

def GrowSet((depGraph, prod), (sets', done')) =
     (GetNewProd(depGraph, (prod, sets'))
     ,
      {| nil  : ()
             => (sets', (done', false))

       | cons : (attempt, (sets, (done, cyclic)))
             => if cyclic then
                     (sets, (true, cyclic))
                else
                     Grow(Strip(CycleTest(TranClose(Paste(depGraph, attempt))))
                         ,
                          (sets, (done, WhichSet(prod)))
                         )
       |}
          (Attempts(prod, sets'))
     ).

(*****

Function: GetNewProds

GetNewProds takes the updated new production (passed back to us from GrowSet,
in GrowSets), and adds it to the list of productions being rebuilt by
GrowSets (for accounting purposes). Further, it passes back 'newSets',
'done', and 'cyclic' as they were passed in, but in the proper order which
GrowSets must return them in. Note that these variables were also passed
in from GrowSet.

*****)

def GetNewProds((newProd, (newSets, (done, cyclic))), newProds) =
     (cons(newProd, newProds), (newSets, (done, cyclic))).

(*****

Function: GrowSets

For each production in the grammar, GrowSets tries to enlarge the set
corresponding to the symbol on the left hand side, by calling GrowSet.

Note: (1) The list of productions is rebuilt each time GrowSets is called,
          via GetNewProds, to update needed accounting information.
      (2) We initially set the 'done' flag to true, so that if no
          set enlargement takes place during GrowSets, we know that the
          Knuth algorithm is finished (and hence the grammar is well
          defined). 'done' is set to false during GrowSet if an enlargement
          takes place.
      (3) If a cycle is detected during GrowSet, we are similarly finished
          Knuth, since this tells us that the grammar is not well defined.

*****)

def GrowSets(prods, sets) =
     {| nil  : ()
            => ([], (sets, (true, false)))

      | cons : (prod, (newProds, (newSets, (done, cyclic))))
            => if cyclic then
                    (prods, (newSets, (true, true)))
               else
                    GetNewProds(GrowSet(prod, (newSets, done)), newProds)
      |}
         (prods).

(*****

Function: Knuth

This algorithm repeatedly tries to enlarge the 'sets' of graphs (one set of
graphs exists for each symbol in the attribute grammar's alphabet), by
calling GrowSets, until the bound on this repetition expires, or the
algorithm discovers it is 'done' (ie. the grammar is or is not well
defined). Knuth returns the value of the boolean 'cyclic' flag only, as
this is set if and only if the grammar is well defined.

*****)

def Knuth(prods', (bound, sets')) =
     not(p1(p1(p1({| zero : ()
                         => (prods', (sets', (false, false)))

                   | succ : (prods, (sets, (done, cyclic)))
                         => if done then
                                 (prods, (sets, (done, cyclic)))
                            else
                                 GrowSets(prods, sets)
                   |}
                      (bound)
                 )
              )
           )
        ).

(*****

Function: WellDef

WellDef takes an attribute grammar as input. If the input is initially
improperly structured (checked by InputOk), false is returned immediately.
Otherwise, some intermediary data is generated (by BuildData), and passed
to Knuth, which tests the attribute grammar to ensure it is infact well
defined.

*****)

def WellDef(alphabet, productions) =
     if InputOk(alphabet, productions) then
         Knuth(BuildData(alphabet, productions))
     else
         false.
