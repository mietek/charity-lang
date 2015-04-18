#include <basics.ch>

(******

lex.ch

AUTHOR: Marc A. Schroeder
        marc@cpsc.ucalgary.ca

26 May 1992

DESCRIPTION:

The following Charity program specifies a lexical analyzer. The lexical
analyzer is layered on top of a DFA (minus a set of goal states), and
has been written so to be general, in the sense that one need only
reconfigure the DFA to obtain a lexical analyzer for any given language
(in theory).

The lexical analyzer can be called either on individual words, with:

        Lex(list(symbol))

where list(symbol) is the word we wish to analyze, or on a larger list
of words, separated by the 'sep' symbol, with:

        Scan(list(symbol).

Lex() returns a token.
Scan() returns a list of tokens.

The example language given here (over the set of symbols: a, b, sep)
recognizes [a, a] as the keyword 'aa', [b, b] as the keyword 'bb', and
all other non-empty strings of symbols as identifiers.

In the case of an identifier, the list of symbols which makes up the
identifier's name is returned within the token. This is not done for
keywords (although the feature could trivially be added). Any other
kind of token (ie. integer, real, error, etc..) could also be added
trivially, as needed.

******)

(* define the alphabet for the DFA *)

data symbol -> T =
          a     : 1 -> T
        | b     : 1 -> T
        | sep   : 1 -> T.

(* define the set of states for the DFA *)

data state -> T =
          st1   : 1 -> T
        | st2   : 1 -> T
        | st3   : 1 -> T
        | st4   : 1 -> T
        | st5   : 1 -> T
        | st6   : 1 -> T.

def start = st1.     (* the start state for the DFA *)

(* define the transition function for the DFA *)

def NextState(sy, st) =
        { a =>
                { st1 => st2
                | st2 => st3
                | st3 => st6
                | st4 => st6
                | st5 => st6
                | st6 => st6
                } (st)
        | b =>
                { st1 => st4
                | st2 => st6
                | st3 => st6
                | st4 => st5
                | st5 => st6
                | st6 => st6
                } (st)
        | sep =>
                st1
        } (sy).

(******

Drive(): Given a word, use it as the input string to the DFA, and return
         the state that the string drives the DFA into. Note: We start at
the start state for each individual word.

******)

def Drive(word) =
        {| nil  : ()           => start
         | cons : (symb, rest) => NextState(symb, rest)
         |} (reverse(word)).

(* define the various tokens that Lex() can return *)

data token -> T =
          aa    : 1            -> T
        | bb    : 1            -> T
        | ident : list(symbol) -> T
        | empty : 1            -> T.

(******

Lex(): Map the state that Drive(word) drives the DFA into, to the token
       we wish to return.

******)

def Lex(word) =
        { st1 => empty
        | st2 => ident(word)
        | st3 => aa
        | st4 => ident(word)
        | st5 => bb
        | st6 => ident(word)
        } (Drive(word)).

(******

DoLex(): When 'scanning' an input string into a list of tokens, DoLex()
         converts the latest 'sep' separated word into a token, and adds
it to the list of tokens already scanned.

******)

def DoLex(word, tokenList) =
        { nil  => tokenList
        | cons => cons(Lex(word), tokenList)
        } (word).

(******

Shift(): When scanning an input string (of possibly multiple words), Shift()
         breaks the input string down into 'sep' separated words, so they can
be analyzed individually.

******)

def Shift(sy, (newWord, tokenList)) =
        if
                { a   => false
                | b   => false
                | sep => true
                } (sy)
        then
                ([], DoLex(newWord, tokenList))
        else
                (cons(sy, newWord), tokenList).

(******

Scan(): Given a list of symbols, return the corresponding list of tokens.

******)

def Scan(input) =
        p1 (
                {| nil  : ()         => ([], [])
                 | cons : (sy, rest) => Shift(sy, rest)
                 |} (cons(sep, input))
           ).
