data C -> inflist(A) = head: C -> A | tail: C -> C. 

data success_or_fail(A) -> C = ss: A -> C| ff: 1 -> C.


def nats2 =
    () => (| n => head: n
           |      tail: succ(n)
           |)(succ(succ(zero))).

def half_nats2 =
    () => (| (n,m) => head: n
           |          tail: (m,succ(n))
           |)(succ(zero),succ(zero)).


def forage =
    ((S,B),N) =>
    {| zero: () => (((S,B),N),ff())
     | succ: (((S,B),N),Val) => { ss(V) => (((S,B),N),Val)
                                | ff() => (((tail(S),tail(B)),tail(N))
                                          ,{true() => ss(head(S))
                                           |false() => ff()
                                           }(head(B))
                                          )
                                }(Val)
     |}(head(N)).


def sieve =
    T0 => (| (T,Val) => head: Val
           |            tail: forage(T)
           |)(forage(T0)).


def cyc = 
    (n,m) => { zero()   => (succ(zero),pred(m))
             | succ(m1) => { zero() => (zero,succ(n))
                           | succ(m2) => (succ(n),pred(m))
                           }(m1)
             }(m).
           

def AND =
    (L) => {| nil: () => true
            | cons: (b,v) => and(b,v)
            |}(L).

def tail_prime_pred =
    ((S,N),F) =>
   {true()  => ((list{z => cyc(z)}(cons((zero,head(N)),S)),tail(N)),true)              
   |false() => ((list{z => cyc(z)}(S),tail(N)),false)
   }(AND(list{x => { zero() => false|succ(_) => true}(p0(x))}(S))).


def prime_pred =
    () => (| (S,F) => head: F
	   |          tail: tail_prime_pred(S,F)
           |)(tail_prime_pred(([],nats2),false)).

(*  Finally the sieve is given by: *)


def primes = () => (sieve(( nats2, prime_pred), half_nats2)).



def list_primes = n => p0({| zero: () => ([],primes)
                           | succ: (L,P) =>(cons(head(P),L),tail(P))
                           |}(int_2_nat(n))).

def lp1 = n => list{ss(x) => ss(nat_2_int(x))
                   | _    => ff} (list_primes(n)).

def primes' = () => inflist{ss(x) => ss(nat_2_int(x)) | _ => ff} (primes).


