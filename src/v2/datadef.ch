%
% test inductive datatype definition
%
data nat -> C = zero: -> C
              | succ: C -> C.

data nat2 -> C = zero2: -> C
              | succ2: C -> C.
%data mylist(A,B) -> C = nil: -> C
%                    | cons: A,B, C -> C.

data mylist2(A) -> C = nil2: 1 -> C
                     | cons2: A*C -> C.

data D-> inflist(A) = head: D-> A
                    | tail: D -> D.

data C-> exp(A,B) = fn: C->A=>B.

def plus(#x, @y) = 
     zero => y
    |succ m => succ plus(m,y)

def plus2 = {: plus(#x,@y) = 
             | zero => y
	     | succ m => succ plus(m,y)
	     :}

def append (#_,_): list(A), list(A) -> list(A)
     = nil , L => L
     | cons(a,as),L  => cons(a,append(as,L)).
	    
def reverse: list(A) -> list(A)
    = L => {: rev(#_,@A) = 
                         |
                         |  nil => A
                         | cons(a,as) => rev(as,cons(a,A))
			 |
            :} (L,[]).

def start = =>reverse([1,2,3,4])
%plus2(succ zero, succ succ zero)