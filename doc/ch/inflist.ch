
data I -> inflist(A) = head: I -> A
                     | tail: I -> I.


def nats: 1  -> inflist(nat)

        = () => (| n => head: n
                 |      tail: succ n
                 |)
                    zero.


def array_nats: 1  -> inflist(inflist(nat))

              = () => inflist{m => inflist{n => add (m, n)} nats} nats.


def get_inflist: nat * inflist(A) -> A

               = (n,   il)        => head {| zero: ()  => il
                                           | succ: il' => tail il'
                                           |}
                                              n.

