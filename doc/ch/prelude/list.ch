
(*
 *     THE LIST DATATYPE (BUILTIN)
 *
 *     data list A -> C = nil : 1     -> C
 *                      | cons: A * C -> C.
 *
 *)


def reverse: list A -> list A

           = l      => p0 {| nil : ()                     => ([],          l)
                           | cons: (_, (r, cons (a, l'))) => (cons (a, r), l')
                                 | _                      => ([],          l)
                           |}
                              l.


def isEmpty: list A -> bool

           = nil    => true
           | _      => false.


def length: list A -> nat

          = l      => {| nil : ()     => zero
                       | cons: (_, n) => succ n
                       |}
                          l.


def hd: list A      -> SF A

      = nil         => ff
      | cons (a, _) => ss a.


def tl: list A      -> SF list A

      = nil         => ff
      | cons (_, l) => ss l.


def init: list A -> SF list A

    = l => {| nil : ()         => ff
            | cons: (_, ff)    => ss []
                  | (a, ss l') => ss cons (a, l')
            |}
               l.


def last: list A -> SF A

        = l      =>  hd reverse l.


def append: list A * list A -> list A

          = (l1,      l2)   => {| nil : () => l2
                                | cons: p  => cons p
                                |}
                                   l1.


def flatten: list list A -> list A

           = ll          => {| nil : () => []
                             | cons: p  => append p
                             |}
                                ll.


def remove{p: A -> bool}: list A -> list A

    = l => {| nil : ()           => []
            | cons: (a, r) | p a => p1 #
                           | ..  => cons (a, r)
            |}
               l.


def filter{p: A -> bool}: list A -> list A

    = l => {| nil : ()           => []
            | cons: (a, r) | p a => cons (a, r)
                           | ..  => r
            |}
               l.


def takewhile{p: A -> bool}: list A -> list A

    = l => {| nil : ()           => []
            | cons: (a, r) | p a => cons (a, r)
                           | ..  => []
            |}
               l.


def dropwhile{p: A -> bool}: list A -> list A

    = l => {| nil : ()           => []
            | cons: (a, r) | p a => r
                           | ..  => cons #
            |}
               l.


def OR: list bool -> bool

      = lb        => {| nil : ()          => false
                      | cons: (b, r) | b  => true
                                     | .. => r
                      |}
                         lb.


def AND: list bool -> bool

       = lb        => {| nil : ()          => true
                       | cons: (b, r) | b  => r
                                      | .. => false
                       |}
                          lb.


def ADD: list nat -> nat

       = ln       => {| nil : () => zero
                      | cons: pn => add pn
                      |}
                         ln.


def MUL: list nat -> nat

       = ln       => {| nil : () => one
                      | cons: pn => mul pn
                      |}
                         ln.


def rep: nat * A  -> list A

       = (n,   a) => {| zero: () => []
                      | succ: l  => cons (a, l)
                      |}
                         n.


def test_list{p: A -> bool}: list A -> bool

    = l => {| nil : ()           => false
            | cons: (a, r) | p a => true
                           | ..  => r
            |}
               l.


def eq_list{eq_A: A * A -> bool}: list A * list A -> bool

    = (l1, l2) => {| nil : ()           => (fn: nil                        => true
                                              | cons _                     => false
                                           )
                   | cons: (a, (fn: f)) => (fn: nil                        => false
                                              | cons (b, bs) | eq_A (a, b) => f bs
                                                             | ..          => false
                                           )
                   |}
                      l1

    ; (fn: f) => f l2.


def zip: list A * list B -> list (A * B)

    = (l1, l2) => {| nil : ()           => (fn: _            => [])
                   | cons: (a, (fn: f)) => (fn: nil          => []
                                              | cons (b, bs) => cons ((a, b), f bs)
                                           )
                   |}
                      l1

    ; (fn: f) => f l2.


% cartesian product, sub_list (--),
% relational operators (<, >, =>, <=)
% noDups, take, drop, index, max_list, min_list

