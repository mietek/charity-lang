def member{eq}(el, lst) =
     {| nil  : ()              => false
      | cons : (currEl, found) => if found then
                                       found
                                  else
                                       eq(currEl, el)
      |}
         (lst).

def empty(lst) =
     { nil  => true
     | cons => false
     }
       (lst).

data C -> colist(T) =
       cohead : C -> T
     | cotail : C -> C + 1.

data I -> inflist(T) =
       head : I -> T
     | tail : I -> I.

def fib' =
     (| ((x, y), z) =>
             head : z
      |      tail : ((y, z), add(y, z))
      |)
         ((zero, succ(zero)), succ(zero)).

def fib =
     (head: zero, tail: (head: succ(zero), tail: fib')).

def fib_int =
     inflist{n => nat_2_int(n)}(fib).
