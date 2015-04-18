
rf "Lid.ch".


data Lnat -> N = Lzero: 1      -> N
               | Lsucc: Lid(N) -> N.


def nat_2_Lnat: nat -> Lnat

              = n   => {| zero: () => Lzero
                        | succ: Ln => Lsucc (lid: Ln)
                        |}
                           n.


def Lnat_2_nat: Lnat -> nat

              = Ln   => {| Lzero: () => zero
                         | Lsucc: n  => succ lid n
                         |}
                            Ln.


def Lone   = () => Lsucc (lid: Lzero).
def Ltwo   = () => Lsucc (lid: Lone).
def Lthree = () => Lsucc (lid: Ltwo).
def Lfour  = () => Lsucc (lid: Lthree).
def Lfive  = () => Lsucc (lid: Lfour).
def Lsix   = () => Lsucc (lid: Lfive).
def Lseven = () => Lsucc (lid: Lsix).
def Leight = () => Lsucc (lid: Lseven).
def Lnine  = () => Lsucc (lid: Leight).
def Lten   = () => Lsucc (lid: Lnine).


def Lpred: Lnat    -> Lnat

         = Lzero   => Lzero
         | Lsucc n => lid n.


def Ladd: Lnat * Lnat -> Lnat

        = (m,    n)   => {| Lzero: () => n
                          | Lsucc: x  => Lsucc x
                          |}
                             m.


def Lmul: Lnat * Lnat -> Lnat

        = (m,    n)   => {| Lzero: () => Lzero
                          | Lsucc: x  => Ladd (n, lid x)
                          |}
                             m.


def Lpow: Lnat * Lnat -> Lnat

        = (m,    n)   => {| Lzero: () => Lone
                          | Lsucc: x  => Lmul (m, lid x)
                          |}
                             n.


def Lsub: Lnat * Lnat -> Lnat

        = (m,    n)   => {| Lzero: () => m
                          | Lsucc: x  => Lpred lid x
                          |}
                             n.


def Lbig = () => Lpow (Lten, Lten).

