csublist([]).
csublist([1]).
csublist([1,2,3,4,5,6]).

lencsublist([],0).
lencsublist([],3).
lencsublist([1],1).
lencsublist([1,2,3,4,5,6],3).
lencsublist([1,2,3,4,5,6],7).

group{z=>eq_int z}([1,1,2,3,4,4,4,6]).
group{z=>eq_int z}([]).
group{z=>eq_int z}([1,2,3,4,5,6]).

occurences([]).
occurences(["john","john","susan","mary"]).

diagonal(natsnats).

inverse(squares).

diagtrav(natsnats).
