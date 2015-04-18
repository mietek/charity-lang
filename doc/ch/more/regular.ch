#include <basics.ch>

(******

The following Charity program describes a recognizer (DFA) for the regular
language L specified by the regular expression: (A u B) B A* B.

eg. ABAAAB is an element of L,
    AAB is NOT an element of L.

The program is general, in the sense that the specification of the DFA can
be changed (in order to construct a recognizer for a different regular
language) without altering the rest of the code.

The datatypes 'symbol' and 'state', the state 'start', the list of states
'goals', and the function 'NextState' serve as the specification of the DFA.

The rest of the code drives the DFA, and should never need to be altered to
accomodate new regular language.

******)

(* define the alphabet for L *)

data symbol -> T =
          A     : 1 -> T
        | B     : 1 -> T.

(* define the set of states for the DFA *)

data state -> T =
          stErr : 1 -> T
        | st1   : 1 -> T
        | st2   : 1 -> T
        | st3   : 1 -> T
        | st4   : 1 -> T
        | st5   : 1 -> T.

def start = st1.       (* define the start state *)
def goals = [st5].     (* define the set of goal states *)

(* define the transition function for the DFA *)

def NextState(st, sy) =
        { A =>
                { stErr => stErr
                | st1   => st2
                | st2   => stErr
                | st3   => st4
                | st4   => st4
                | st5   => stErr
                } (st)
        | B =>
                { stErr => stErr
                | st1   => st2
                | st2   => st3
                | st3   => st5
                | st4   => st5
                | st5   => stErr
                } (st)
        } (sy).

(* function to determine if two states are equal *)

def SameState(stA, stB) =
        { stErr =>
                { stErr => true
                | st1   => false
                | st2   => false
                | st3   => false
                | st4   => false
                | st5   => false
                } (stB)
        | st1   =>
                { stErr => false
                | st1   => true
                | st2   => false
                | st3   => false
                | st4   => false
                | st5   => false
                } (stB)
        | st2   =>
                { stErr => false
                | st1   => false
                | st2   => true
                | st3   => false
                | st4   => false
                | st5   => false
                } (stB)
        | st3   =>
                { stErr => false
                | st1   => false
                | st2   => false
                | st3   => true
                | st4   => false
                | st5   => false
                } (stB)
        | st4   =>
                { stErr => false
                | st1   => false
                | st2   => false
                | st3   => false
                | st4   => true
                | st5   => false
                } (stB)
        | st5   =>
                { stErr => false
                | st1   => false
                | st2   => false
                | st3   => false
                | st4   => false
                | st5   => true
                } (stB)
        } (stA).

(* function to determine if a state is in a list of states *)

def Member(el, list) =
        {| nil  : ()          => false
         | cons : (el', rest) =>
                if SameState(el, el') then
                        true
                else
                        rest
         |} (list).

(* function to return the state which the input drives the DFA to *)

def Check'(input) =
        {| nil  : ()           => start
         | cons : (symb, rest) => NextState(rest, symb)
         |} (reverse(input)).

(* function which either accepts the input, or rejects it, as a string in
   the language *)

def Check(input) =
        Member(Check'(input), goals).
