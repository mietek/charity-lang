data C -> exp(A, B) = fn: C -> A => B.


def curry{f: A -> B}: 1  -> exp(A, B)

                    = () => (fn: x => f x).


def compose: exp(A, B) * exp(B, C) -> exp(A, C)

           = (f,         g)        => (fn: a => fn (fn (a, f), g)).


def twice: exp(A, A) -> exp(A, A)

         = f         => compose (f, f).


%%%%%


data nat -> C = o: 1 -> C
              | s: C -> C.


def succ = () => curry{s}().


%%%%%


(*
def test1 = () => twice.           % IS A LEGITIMATE ERROR
def test2 = () => twice twice.     % IS A LEGITIMATE ERROR
*)

def test3 = () => twice succ.
def test4 = () => twice twice succ.

def test3' = () => fn (o, test3).
def test4' = () => fn (o, test4).

def test5 = () => curry{twice}().
def test6 = () => twice curry{twice}().

def test7 = () => fn (succ, test5).
def test8 = () => fn (succ, test6).

def test7' = () => fn (o, test7).
def test8' = () => fn (o, test8).
