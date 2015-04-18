
rf "Lid.ch".


data Llist(A) -> L = Lnil : 1          -> L
                   | Lcons: A * Lid(L) -> L.


def list_2_Llist: list(A) -> Llist(A)

                = l       => {| nil : ()      => Lnil
                              | cons: (a, Ll) => Lcons (a, (lid: Ll))
                              |}
                                 l.


def Llist_2_list: Llist(A) -> list(A)

                = Ll       => {| Lnil : ()     => nil
                               | Lcons: (a, l) => cons (a, lid l)
                               |}
                                  Ll.


def Lappend: Llist(A) * Llist(A) -> Llist(A)

           = (l1,       l2)      => {| Lnil : () => l2
                                     | Lcons: p  => Lcons p
                                     |}
                                        l1.


def Lflatten: Llist(Llist(A)) -> Llist(A)

            = ll              => {| Lnil : ()     => Lnil
                                  | Lcons: (l, r) => Lappend (l, lid r)
                                  |}
                                     ll.


def LisElement{eq_A: A * A -> bool}: A * Llist(A) -> bool

    = (a, l) => {| Lnil : ()      => false
                 | Lcons: (a', b) => { true  => true
                                     | false => lid b
                                     }
                                       eq_A (a, a')
                 |}
                    l.

