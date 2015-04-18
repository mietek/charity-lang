
data C -> conat = denat: C -> SF(C).


def nat_2_conat: nat -> conat

               = n   => (| zero    => denat: ff
                         | succ n' => denat: ss n'
                         |)
                            n.


def nat_2_conat': nat -> conat

                = n   => {| zero: () => (denat: ff)
                          | succ: cn => (denat: ss cn)
                          |}
                             n.


def cozero  = () => nat_2_conat zero.
def coone   = () => nat_2_conat one.
def cotwo   = () => nat_2_conat two.
def cothree = () => nat_2_conat three.
def cofour  = () => nat_2_conat four.
def cofive  = () => nat_2_conat five.
def cosix   = () => nat_2_conat six.
def coseven = () => nat_2_conat seven.
def coeight = () => nat_2_conat eight.
def conine  = () => nat_2_conat nine.
def coten   = () => nat_2_conat ten.


def copred: conat         -> conat

          = (denat: ff)   => (denat: ff)
          | (denat: ss n) => n.


def coadd: conat * conat -> conat

         = (m,     n)    => (| (ss m', n')    => denat: ss (denat m', n')
                             | (ff,    ss n') => denat: ss (ff, denat n')
                             | (ff,    ff)    => denat: ff
                             |)
                                (denat m, denat n).


def comul: conat * conat -> conat

    = (m, (denat: ff))   => (denat: ff)
    | (m, (denat: ss n)) => (| (ff,    _)     => denat: ff
                             | (ss m', ff)    => denat: ss (denat m', denat n)
                             | (ss m', ss n') => denat: ss (ss m', denat n')
                             |)
                                (denat m, denat n).

