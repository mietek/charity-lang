
data bTree (A, B) -> C = leaf: A           -> C
                       | node: B * (C * C) -> C.


data bTree_LN A = bTree (A, A).     % LEAF DATA = NODE DATA
data bTree_N  A = bTree (1, A).     % NODE DATA ONLY
data bTree_L  A = bTree (A, 1).     % LEAF DATA ONLY


def traverse{order: A * (list A * list A) -> list A}: bTree_LN A -> list A

    = t => {| leaf: a => [a]
            | node: n => order n
            |}
               t.


def preorder: A * (list A * list A) -> list A

            = (a, pl)               => cons (a, append pl).


def inorder: A * (list A * list A) -> list A

           = (a, (l1,      l2))    => append (l1, cons (a, l2)).


def postorder: A * (list A * list A) -> list A

             = (a, pl)               => append (append pl, [a]).


def OR_bTree: bTree_LN bool -> bool

            = tb            => {| leaf: b       => b
                                | node: (b, pb) => or (b, or pb)
                                |}
                                   tb.


def AND_bTree: bTree_LN bool -> bool

             = tb            => {| leaf: b       => b
                                 | node: (b, pb) => and (b, and pb)
                                 |}
                                    tb.

