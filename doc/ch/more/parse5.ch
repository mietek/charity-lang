#include <basics.ch>

data parseTree(P) -> T =
          leaf  : P           -> T
        | node  : P * list(T) -> T.

data state -> T =
          p     : 1 -> T
        | q     : 1 -> T.

data symbol -> T =
          a     : 1 -> T
        | b     : 1 -> T
        | A     : 1 -> T
        | B     : 1 -> T
        | S     : 1 -> T.

data rule -> T =
          rule1 : 1 -> T
        | rule2 : 1 -> T
        | rule3 : 1 -> T
        | rule4 : 1 -> T
        | rule5 : 1 -> T
        | rule6 : 1 -> T
        | rule7 : 1 -> T
        | rule8 : 1 -> T.

def rules = [rule1, rule2, rule3, rule4, rule5, rule6, rule7].

def EqualStates(state1, state2) =
        { p =>
               { p => true
               | q => false
               }
                 (state2)
        | q =>
               { p => false
               | q => true
               }
                 (state2)
        }
          (state1).

def mypairs(configs, rls) =
        {| nil  : ()                                   => []
         | cons : ((state, (input, (stS, stT))), rest) =>
                                                          if EqualStates(state, q) then
                                                               cons(((state, (input, (stS, stT))), rule8), rest)
                                                          else
                                                               append(list{rl => ((state, (input, (stS, stT))), rl)}(rls), rest)
         |}
            (configs).

def Valid1(symbols) =
        { nil           => false
        | cons(head, _) =>
                           { a => true
                           | _ => false
                           }
                             (head)
        }
          (symbols).

def Valid2(symbols) =
        { nil           => false
        | cons(head, _) =>
                           { b => true
                           | _ => false
                           }
                             (head)
        }
          (symbols).

def Valid3(symbols) =
        { nil              => false
        | cons(head, rest) =>
                              { b =>
                                     { nil            => false
                                     | cons(head2, _) =>
                                                         { B => true
                                                         | _ => false
                                                         }
                                                           (head2)
                                     }
                                       (rest)
                              | _ => false
                              }
                                (head)
        }
          (symbols).

def Valid4(symbols) =
        { nil              => false
        | cons(head, rest) =>
                              { B =>
                                     { nil            => false
                                     | cons(head2, _) =>
                                                         { A => true
                                                         | _ => false
                                                         }
                                                           (head2)
                                     }
                                       (rest)
                              | _ => false
                              }
                                (head)
        }
          (symbols).

def Empty(input) =
        { nil  => true
        | cons => false
        }
          (input).

def DoneParse(stack) =
        { nil              => false
        | cons(symb, rest) =>
                              { S =>
                                     { nil  => true
                                     | cons => false
                                     }
                                       (rest)
                              | _ => false
                              }
                                (symb)
        }
          (stack).

def Rule1Applicable(state, (input, (stS, stT))) =
        and(EqualStates(state, p), Valid1(input)).

def Rule2Applicable(state, (input, (stS, stT))) =
        and(EqualStates(state, p), Valid2(input)).

def Rule3Applicable(state, (input, (stS, stT))) =
        and(EqualStates(state, p), Valid1(stS)).

def Rule4Applicable(state, (input, (stS, stT))) =
        and(EqualStates(state, p), Valid2(stS)).

def Rule5Applicable(state, (input, (stS, stT))) =
        and(EqualStates(state, p), Valid3(stS)).

def Rule6Applicable(state, (input, (stS, stT))) =
        and(EqualStates(state, p), Valid4(stS)).

def Rule7Applicable(state, (input, (stS, stT))) =
        and(EqualStates(state, p), and(Empty(input), DoneParse(stS))).

def Rule8Applicable(state, (input, (stS, stT))) =
        EqualStates(state, q).

def PopN(stack, n) =
        foldint{ zero        => (stack, []),
                 (stk, dump) =>
                                { nil               => ([], dump)
                                | cons(top, bot) => (bot, cons(top, dump))
                                }
                                  (stk)
               }
                 (n).

def BuildTree(root, (stack, subTrees)) =
        cons(node(root, subTrees), stack).

def ApplyRule1(state, (input, (stS, stT))) =
        (p, (p0(PopN(input, 1)), (cons(a, stS), cons(leaf(a), stT)))).

def ApplyRule2(state, (input, (stS, stT))) =
        (p, (p0(PopN(input, 1)), (cons(b, stS), cons(leaf(b), stT)))).

def ApplyRule3(state, (input, (stS, stT))) =
        (p, (input, (cons(A, p0(PopN(stS, 1))), BuildTree(A, PopN(stT, 1))))).

def ApplyRule4(state, (input, (stS, stT))) =
        (p, (input, (cons(B, p0(PopN(stS, 1))), BuildTree(B, PopN(stT, 1))))).

def ApplyRule5(state, (input, (stS, stT))) =
        (p, (input, (cons(B, p0(PopN(stS, 2))), BuildTree(B, PopN(stT, 2))))).

def ApplyRule6(state, (input, (stS, stT))) =
        (p, (input, (cons(S, p0(PopN(stS, 2))), BuildTree(S, PopN(stT, 2))))).

def ApplyRule7(state, (input, (stS, stT))) =
        (q, ([], ([], stT))).

def Apply(config, whichRule) =
        { rule1 =>
                   if Rule1Applicable(config) then
                        b0(ApplyRule1(config))
                   else
                        b1
        | rule2 =>
                   if Rule2Applicable(config) then
                        b0(ApplyRule2(config))
                   else
                        b1
        | rule3 =>
                   if Rule3Applicable(config) then
                        b0(ApplyRule3(config))
                   else
                        b1
        | rule4 =>
                   if Rule4Applicable(config) then
                        b0(ApplyRule4(config))
                   else
                        b1
        | rule5 =>
                   if Rule5Applicable(config) then
                        b0(ApplyRule5(config))
                   else
                        b1
        | rule6 =>
                   if Rule6Applicable(config) then
                        b0(ApplyRule6(config))
                   else
                        b1
        | rule7 =>
                   if Rule7Applicable(config) then
                        b0(ApplyRule7(config))
                   else
                        b1
        | rule8 =>
                   if Rule8Applicable(config) then
                        b0(config)
                   else
                        b1
        }
          (whichRule).

def parse'(input) =
        {| zero : ()        => [(p, (input, ([], [])))]
         | succ : (configs) =>
                               {| nil  : ()           => []
                                | cons : (this, rest) =>
                                                         { b0(newConfig) => cons(newConfig, rest)
                                                         | b1            => rest
                                                         }
                                                           (this)
                                |}
                                   (list{attempt => Apply(attempt)}(mypairs(configs, rules)))
         |}
            (add(int_2_nat(4), mul(int_2_nat(5), length(input)))).

def parse(input) =
        {| nil  : ()           => []
         | cons : (this, rest) =>
                                  if EqualStates(q, p0(this)) then
                                       append(p1(p1(p1(this))), rest)
                                  else
                                       rest
         |}
            (parse'(input)).
