(* define integer numbers and some operations on them *)
(*  no efficiency considerations *)

data digit -> C =
      d0 | d1 | d2 | d3 | d4 | d5 | d6 | d7 | d8 | d9 : 1 -> C.

data sign -> Y =
    positive | negative : 1 -> Y.

data int -> C =
    INT : sign * list(digit) -> C.

def digit_2_nat : digit -> nat =
      d0() => zero
    | d1() => succ(zero)
    | d2() => succ(succ(zero))
    | d3() => succ(succ(succ(zero)))
    | d4() => succ(succ(succ(succ(zero))))
    | d5() => succ(succ(succ(succ(succ(zero)))))
    | d6() => succ(succ(succ(succ(succ(succ(zero))))))
    | d7() => succ(succ(succ(succ(succ(succ(succ(zero)))))))
    | d8() => succ(succ(succ(succ(succ(succ(succ(succ(zero))))))))
    | d9() => succ(succ(succ(succ(succ(succ(succ(succ(succ(zero))))))))).

def succ_digit : digit -> bool * digit =
      d0() => (false,d1)
    | d1() => (false,d2)
    | d2() => (false,d3)
    | d3() => (false,d4)
    | d4() => (false,d5)
    | d5() => (false,d6)
    | d6() => (false,d7)
    | d7() => (false,d8)
    | d8() => (false,d9)
    | d9() => (true,d0).

def pred_digit : digit -> bool * digit =
      d0() => (true,d9)
    | d1() => (false,d0)
    | d2() => (false,d1)
    | d3() => (false,d2)
    | d4() => (false,d3)
    | d5() => (false,d4)
    | d6() => (false,d5)
    | d7() => (false,d6)
    | d8() => (false,d7)
    | d9() => (false,d8).

def add_digit : digit * digit -> digit * digit =
  (dd1,dd2) =>
    { d0() => (d0,dd2)
    | d1() => 
       { d0() => (d0,d1) | d1() => (d0,d2) | d2() => (d0,d3) | d3() => (d0,d4) | d4() => (d0,d5)
       | d5() => (d0,d6) | d6() => (d0,d7) | d7() => (d0,d8) | d8() => (d0,d9) | d9() => (d1,d0)} (dd2)
    | d2() => 
       { d0() => (d0,d2) | d1() => (d0,d3) | d2() => (d0,d4) | d3() => (d0,d5) | d4() => (d0,d6)
       | d5() => (d0,d7) | d6() => (d0,d8) | d7() => (d0,d9) | d8() => (d1,d0) | d9() => (d1,d1)} (dd2)
    | d3() => 
       { d0() => (d0,d3) | d1() => (d0,d4) | d2() => (d0,d5) | d3() => (d0,d6) | d4() => (d0,d7)
       | d5() => (d0,d8) | d6() => (d0,d9) | d7() => (d1,d0) | d8() => (d1,d1) | d9() => (d1,d2)} (dd2)
    | d4() => 
       { d0() => (d0,d4) | d1() => (d0,d5) | d2() => (d0,d6) | d3() => (d0,d7) | d4() => (d0,d8)
       | d5() => (d0,d9) | d6() => (d1,d0) | d7() => (d1,d1) | d8() => (d1,d2) | d9() => (d1,d3)} (dd2)
    | d5() => 
       { d0() => (d0,d5) | d1() => (d0,d6) | d2() => (d0,d7) | d3() => (d0,d8) | d4() => (d0,d9)
       | d5() => (d1,d0) | d6() => (d1,d1) | d7() => (d1,d2) | d8() => (d1,d3) | d9() => (d1,d4)} (dd2)
    | d6() => 
       { d0() => (d0,d6) | d1() => (d0,d7) | d2() => (d0,d8) | d3() => (d0,d9) | d4() => (d1,d0)
       | d5() => (d1,d1) | d6() => (d1,d2) | d7() => (d1,d3) | d8() => (d1,d4) | d9() => (d1,d5)} (dd2)
    | d7() => 
       { d0() => (d0,d7) | d1() => (d0,d8) | d2() => (d0,d9) | d3() => (d1,d0) | d4() => (d1,d1)
       | d5() => (d1,d2) | d6() => (d1,d3) | d7() => (d1,d4) | d8() => (d1,d5) | d9() => (d1,d6)} (dd2)
    | d8() => 
       { d0() => (d0,d8) | d1() => (d0,d9) | d2() => (d1,d0) | d3() => (d1,d1) | d4() => (d1,d2)
       | d5() => (d1,d3) | d6() => (d1,d4) | d7() => (d1,d5) | d8() => (d1,d6) | d9() => (d1,d7)} (dd2)
    | d9() => 
       { d0() => (d0,d9) | d1() => (d1,d0) | d2() => (d1,d1) | d3() => (d1,d2) | d4() => (d1,d3)
       | d5() => (d1,d4) | d6() => (d1,d5) | d7() => (d1,d6) | d8() => (d1,d7) | d9() => (d1,d8)} (dd2)
    } (dd1).


def sub_digit : digit * digit -> digit * digit =
  (dd1,dd2) =>
    { d0() => (d0,dd1)
    | d1() => 
       { d0() => (d1,d9) | d1() => (d0,d0) | d2() => (d0,d1) | d3() => (d0,d2) | d4() => (d0,d3)
       | d5() => (d0,d4) | d6() => (d0,d5) | d7() => (d0,d6) | d8() => (d0,d7) | d9() => (d0,d8)} (dd1)
    | d2() => 
       { d0() => (d1,d8) | d1() => (d1,d9) | d2() => (d0,d0) | d3() => (d0,d1) | d4() => (d0,d2)
       | d5() => (d0,d3) | d6() => (d0,d4) | d7() => (d0,d5) | d8() => (d0,d6) | d9() => (d0,d7)} (dd1)
    | d3() => 
       { d0() => (d1,d7) | d1() => (d1,d8) | d2() => (d1,d9) | d3() => (d0,d0) | d4() => (d0,d1)
       | d5() => (d0,d2) | d6() => (d0,d3) | d7() => (d0,d4) | d8() => (d0,d5) | d9() => (d0,d6)} (dd1)
    | d4() => 
       { d0() => (d1,d6) | d1() => (d1,d7) | d2() => (d1,d8) | d3() => (d1,d9) | d4() => (d0,d0)
       | d5() => (d0,d1) | d6() => (d0,d2) | d7() => (d0,d3) | d8() => (d0,d4) | d9() => (d0,d5)} (dd1)
    | d5() => 
       { d0() => (d1,d5) | d1() => (d1,d6) | d2() => (d1,d7) | d3() => (d1,d8) | d4() => (d1,d9)
       | d5() => (d0,d0) | d6() => (d0,d1) | d7() => (d0,d2) | d8() => (d0,d3) | d9() => (d0,d4)} (dd1)
    | d6() => 
       { d0() => (d1,d4) | d1() => (d1,d5) | d2() => (d1,d6) | d3() => (d1,d7) | d4() => (d1,d8)
       | d5() => (d1,d9) | d6() => (d0,d0) | d7() => (d0,d1) | d8() => (d0,d2) | d9() => (d0,d3)} (dd1)
    | d7() => 
       { d0() => (d1,d3) | d1() => (d1,d4) | d2() => (d1,d5) | d3() => (d1,d6) | d4() => (d1,d7)
       | d5() => (d1,d8) | d6() => (d1,d9) | d7() => (d0,d0) | d8() => (d0,d1) | d9() => (d0,d2)} (dd1)
    | d8() => 
       { d0() => (d1,d2) | d1() => (d1,d3) | d2() => (d1,d4) | d3() => (d1,d5) | d4() => (d1,d6)
       | d5() => (d1,d7) | d6() => (d1,d8) | d7() => (d1,d9) | d8() => (d0,d0) | d9() => (d0,d1)} (dd1)
    | d9() => 
       { d0() => (d1,d1) | d1() => (d1,d2) | d2() => (d1,d3) | d3() => (d1,d4) | d4() => (d1,d5)
       | d5() => (d1,d6) | d6() => (d1,d7) | d7() => (d1,d8) | d8() => (d1,d9) | d9() => (d0,d0)} (dd1)
    } (dd2).

def mul_digit : digit * digit -> digit * digit =
  (dd1,dd2) =>
    { d0() => (d0,d0)
    | d1() => 
	  { d0() => (d0,d0) | d1() => (d0,d1) | d2() => (d0,d2) | d3() => (d0,d3)
          | d4() => (d0,d4) | d5() => (d0,d5) | d6() => (d0,d6) | d7() => (d0,d7)
          | d8() => (d0,d8) | d9() => (d0,d9) } (dd2)
    | d2() => 
	  { d0() => (d0,d0) | d1() => (d0,d2) | d2() => (d0,d4) | d3() => (d0,d6)
          | d4() => (d0,d8) | d5() => (d1,d0) | d6() => (d1,d2) | d7() => (d1,d4)
          | d8() => (d1,d6) | d9() => (d1,d8) } (dd2)
    | d3() => 
	  { d0() => (d0,d0) | d1() => (d0,d3) | d2() => (d0,d6) | d3() => (d0,d9)
          | d4() => (d1,d2) | d5() => (d1,d5) | d6() => (d1,d8) | d7() => (d2,d1)
          | d8() => (d2,d4) | d9() => (d2,d7) } (dd2)
    | d4() => 
	  { d0() => (d0,d0) | d1() => (d0,d4) | d2() => (d0,d8) | d3() => (d1,d2)
          | d4() => (d1,d6) | d5() => (d2,d0) | d6() => (d2,d4) | d7() => (d2,d8)
          | d8() => (d3,d2) | d9() => (d3,d6) } (dd2)
    | d5() => 
	  { d0() => (d0,d0) | d1() => (d0,d5) | d2() => (d1,d0) | d3() => (d1,d5)
          | d4() => (d2,d0) | d5() => (d2,d5) | d6() => (d3,d0) | d7() => (d3,d5)
          | d8() => (d4,d0) | d9() => (d4,d5) } (dd2)
    | d6() => 
	  { d0() => (d0,d0) | d1() => (d0,d6) | d2() => (d1,d2) | d3() => (d1,d8)
          | d4() => (d2,d4) | d5() => (d3,d0) | d6() => (d3,d6) | d7() => (d4,d2)
          | d8() => (d4,d8) | d9() => (d5,d4) } (dd2)
    | d7() => 
	  { d0() => (d0,d0) | d1() => (d0,d7) | d2() => (d1,d4) | d3() => (d2,d1)
          | d4() => (d2,d8) | d5() => (d3,d5) | d6() => (d4,d2) | d7() => (d4,d9)
          | d8() => (d5,d6) | d9() => (d6,d3) } (dd2)
    | d8() => 
	  { d0() => (d0,d0) | d1() => (d0,d8) | d2() => (d1,d6) | d3() => (d2,d4)
          | d4() => (d3,d2) | d5() => (d4,d0) | d6() => (d4,d8) | d7() => (d5,d6)
          | d8() => (d6,d4) | d9() => (d7,d2) } (dd2)
    | d9() => 
	  { d0() => (d0,d0) | d1() => (d0,d9) | d2() => (d1,d8) | d3() => (d2,d7)
          | d4() => (d3,d6) | d5() => (d4,d5) | d6() => (d5,d4) | d7() => (d6,d3)
          | d8() => (d7,d2) | d9() => (d8,d1) } (dd2)
    } (dd1).

def max_digit : digit * digit -> digit =
 (dd1,dd2) =>
    { d0() => dd2
    | d1() => 
	  { d0() => d1 | _ => dd2} (dd2)
    | d2() => 
	  { d0() => d2 | d1() => d2 | _ => dd2} (dd2)
    | d3() => 
	  { d0() => d3 | d1() => d3 | d2() => d3 | _ => dd2} (dd2)
    | d4() => 
	  { d0() => d4 | d1() => d4 | d2() => d4 | d3() => d4 | _ => dd2} (dd2)
    | d5() => 
	  { d0() => d5 | d1() => d5 | d2() => d5 | d3() => d5 | d4() => d5 | _ => dd2} (dd2)
    | d6() => 
	  { d0() => d6 | d1() => d6 | d2() => d6 | d3() => d6 | d4() => d6 | d5() => d6
          | _ => dd2} (dd2)
    | d7() => 
	  { d0() => d7 | d1() => d7 | d2() => d7 | d3() => d7 | d4() => d7 | d5() => d7
          | d6() => d7 | _ => dd2} (dd2)
    | d8() => 
	  { d0() => d8 | d1() => d8 | d2() => d8 | d3() => d8 | d4() => d8 | d5() => d8
          | d6() => d8 | d7() => d8 | _ => dd2} (dd2)
    | d9() => d9
    } (dd1).

def min_digit = (dd1,dd2) =>
    { d0() => d0
    | d1() =>
	  { d0() => d0 | _ => d1} (dd2)
    | d2() =>
	  { d0() => d0 | d1() => d1 | _ => d2} (dd2)
    | d3() =>
	  { d0() => d0 | d1() => d1 | d2() => d2 | _ => d3} (dd2)
    | d4() =>
	  { d0() => d0 | d1() => d1 | d2() => d2 | d3() => d3 | _ => d4} (dd2)
    | d5() =>
	  { d0() => d0 | d1() => d1 | d2() => d2 | d3() => d3 | d4() => d4 | _ => d5} (dd2)
    | d6() =>
	  { d0() => d0 | d1() => d1 | d2() => d2 | d3() => d3 | d4() => d4 | d5() => d5
          | _ => d6} (dd2)
    | d7() =>
	  { d0() => d0 | d1() => d1 | d2() => d2 | d3() => d3 | d4() => d4 | d5() => d5
          | d6() => d6 | _ => d7} (dd2)
    | d8() =>
	  { d0() => d0 | d1() => d1 | d2() => d2 | d3() => d3 | d4() => d4 | d5() => d5
          | d6() => d6 | d7() => d7 | _ => d8} (dd2)
    | d9() => dd2
    } (dd1).


def eq_digit : digit * digit -> bool =
      (d0(), d0()) => true 
    | (d1(), d1()) => true
    | (d2(), d2()) => true
    | (d3(), d3()) => true
    | (d4(), d4()) => true
    | (d5(), d5()) => true
    | (d6(), d6()) => true
    | (d7(), d7()) => true
    | (d8(), d8()) => true
    | (d9(), d9()) => true
    |  _       => false.

def gt_digit = (dd1,dd2) =>
    { d0() => { _ => false } (dd2)
    | d1() => { d0() => true | _ => false } (dd2)
    | d2() => { d0() => true | d1() => true | _ => false } (dd2)
    | d3() => { d0() => true | d1() => true | d2() => true | _ => false } (dd2)
    | d4() => { d0() => true | d1() => true | d2() => true | d3() => true | _ => false } (dd2)
    | d5() => { d0() => true | d1() => true | d2() => true | d3() => true | d4() => true 
	   | _ => false } (dd2)
    | d6() => { d6() => false | d7() => false | d8() => false | d9() => false 
           | _ => true } (dd2)
    | d7() => { d7() => false | d8() => false | d9() => false | _ => true } (dd2)
    | d8() => { d8() => false | d9() => false | _ => true } (dd2)
    | d9() => { d9() => false | _ => true } (dd2)
    } (dd1).

def le_digit = (dd1,dd2) =>
    { d0() => { _ => true } (dd2)
    | d1() => { d0() => false | _ => true } (dd2)
    | d2() => { d0() => false | d1() => false | _ => true } (dd2)
    | d3() => { d0() => false | d1() => false | d2() => false | _ => true } (dd2)
    | d4() => { d0() => false | d1() => false | d2() => false | d3() => false 
           | _ => true } (dd2)
    | d5() => { d0() => false | d1() => false | d2() => false | d3() => false | d4() => false 
	   | _ => true } (dd2)
    | d6() => { d6() => true | d7() => true | d8() => true | d9() => true 
           | _ => false } (dd2)
    | d7() => { d7() => true | d8() => true | d9() => true | _ => false } (dd2)
    | d8() => { d8() => true | d9() => true | _ => false } (dd2)
    | d9() => { d9() => true | _ => false } (dd2)
    } (dd1).

def ge_digit = (dd1,dd2) =>
    { d0() => { d0() => true | _ => false } (dd2)
    | d1() => { d0() => true | d1() => true | _ => false } (dd2)
    | d2() => { d0() => true | d1() => true | d2() => true |_ => false } (dd2)
    | d3() => { d0() => true | d1() => true | d2() => true | d3() => true |_ => false } (dd2)
    | d4() => { d0() => true | d1() => true | d2() => true | d3() => true | d4() => true 
           | _ => false } (dd2)
    | d5() => { d6() => false | d7() => false | d8() => false | d9() => false 
	   | _ => true } (dd2)
    | d6() => { d7() => false | d8() => false | d9() => false 
           | _ => true } (dd2)
    | d7() => { d8() => false | d9() => false | _ => true } (dd2)
    | d8() => { d9() => false | _ => true } (dd2)
    | d9() => { _ => true } (dd2)
    } (dd1).

def lt_digit = (dd1,dd2) =>
    { d0() => { d0() => false | _ => true } (dd2)
    | d1() => { d0() => false | d1() => false | _ => true } (dd2)
    | d2() => { d0() => false | d1() => false | d2() => false | _ => true } (dd2)
    | d3() => { d0() => false | d1() => false | d2() => false | d3() => false 
           | _ => true } (dd2)
    | d4() => { d0() => false | d1() => false | d2() => false | d3() => false | d4() => false 
           | _ => true } (dd2)
    | d5() => { d6() => true | d7() => true | d8() => true | d9() => true 
	   | _ => false } (dd2)
    | d6() => { d7() => true | d8() => true | d9() => true 
           | _ => false } (dd2)
    | d7() => { d8() => true | d9() => true | _ => false } (dd2)
    | d8() => { d9() => true | _ => false } (dd2)
    | d9() => { _ => false } (dd2)
    } (dd1).

def nat_2_digit2 = (n) =>
    {| zero:() => (false,d0)
     | succ:(c, n) => {(c', n)=>(or(c', c),n)}( (succ_digit(n)))
     |}
       (n).

def compress_digits : list(digit) -> list(digit) =
    L => {| nil : () => L
          | cons: (_,L) => { nil() => nil
                           | cons(a,L') => { d0() => L'
		                           | _  => L
                                           } (a)
                           } (L)
          |}(L).
	        
def make_length_digits = (n) =>
    {| zero:() => nil
     | succ:(L) => cons(d0, L)
     |}
       (n).

def make_same_length_digits = (L1, L2) =>
    {(len_L1, len_L2)=>
         {(n1, n2)=>(append(make_length_digits(n1), L1),
                     append(make_length_digits(n2), L2))
         }
          ( {maxx=>(sub(maxx, len_L1),sub(maxx, len_L2))}
	      ((max(len_L1, len_L2))))
    }
     ( (length(L1),length(L2))).

(* define our own head and tail which react appropriately when we get into *)
(*  an error state -- in these cases we just ignore the problem *)
def head_digits : list(digit) -> digit = 
     nil()     => d0
   | cons(a,_) => a.

def tail_digits : list(digit) -> list(digit) = 
     nil()     => nil
   | cons(_,l) => l.

def succ_digits : list(digit) -> list(digit) = 
    L => { (cout,num) => { d0() => num
                         | _    => cons(cout,num)
			 } (cout)
         } ({| nil : () => (d1,nil)
             | cons: (a,(c,L)) => {(carry,sum) => (carry,cons(sum,L))}(add_digit(c,a))
             |} (L)).

def pred_digits : list(digit) -> list(digit) =
    L => { nil()      => nil
         | cons(a,L') => {d0() => L'
                         |_  => cons(a,L')
                         } (a)
	 } ({ (borrow,num) => { d0() => num
    	                      | _  => cons(d0,nil)
			      } (borrow)
	    } ({| nil : () => (d1,nil)
                | cons: (a,(b,L)) => {(bor,num) => (bor,cons(num,L))}(sub_digit(a,b))
                |} (L)
	       )
           ).

def add_digits = (L1,L2) =>
    {(carry,res) => 
	{ d0() => res
        | _  => cons(carry,res)
        }(carry)
    }
    ({((carry,res),leftover) =>
	{| nil : () => (carry,res)
         | cons: (a,(c,r)) =>
	   { (tens,ones) => (tens,cons(ones,r)) } (add_digit(a,c))
         |} (reverse(leftover))}
    ({| nil : () => ((d0,nil),reverse(L2))
      | cons: (a1,((carry,res),L2)) =>
	   { nil() => ({(tens,ones) => (tens,cons(ones,res))}
                          (add_digit(a1,carry)),nil)
           | cons(a2,L2') =>
		 (({(carry1,sum1) =>
		     { (carry2,sum2) =>
			 (max_digit(carry1,carry2),cons(sum2,res))
                     } (add_digit(a2,sum1))
                   } (add_digit(carry,a1)))
                 , L2')
           } (L2)
      |}(L1))).

def sub_digits  = (L1,L2) =>
compress_digits(
    {(borrow,res) => 
	{ d0() => res
        | _  => cons(d0,nil)
        }(borrow)
    }
    ({((borrow,res),leftover) =>
	{| nil : () => (borrow,res)
         | cons:(a,(b,res)) => 
	       {(borrow,ones) => (borrow,cons(ones,res))}(sub_digit(a,b))
         |}(reverse(leftover))
     }
     ({| nil : () => ((d0,nil),reverse(L1))
       | cons: (n2, ((bor,res),L1)) =>
                { nil() => ((d0,cons(d0,nil)),nil)
                | cons(n1,L1') =>
			 ({(bor',diff') => 
			     (bor',cons(diff',res))
 			  }
			   ({ (bor1,diff1) =>
			      { (bor2,diff2) => 
				  (max_digit(bor1,bor2),diff2)
			      } (sub_digit(diff1,n2))
                            } (sub_digit(n1,bor)))
                         ,L1')
                } (L1)
       |} (L2)
    ))).

def nat_2_digits = (n) =>
    {| zero:() => cons(d0,nil)
     | succ:(d) => add_digits(d, cons(d1,nil))
     |}
       (n).

def digits_2_nat = (L) =>
    {d10 =>
        p1
        ({| nil:() => (succ(zero),zero)
          | cons:(a, (power, acc)) => (mul(d10, power),add(mul(digit_2_nat(a), power), acc))
          |}
            (L)
        )
    }
     ( succ(succ(succ(succ(succ(succ(succ(succ(succ(succ(zero))))))))))).

def nat_2_int = v => INT(positive,nat_2_digits(v)).

def int_2_nat = (v) =>
    digits_2_nat(
          {INT(sgn,x) => 
	      { negative() => nil
              | positive() => x
              } (sgn)
          }(v)).


(* go down the list of elements and find the first non equal el's and then  *)
(* set the flag accordingly *)

def cmp_digits{cmp} = (L1, L2) =>
    p0
    ({(L1, L2)=>
         {| zero:() => (cmp(d0,d0),(L1,L2))
          | succ:(b, (L1, L2)) => 
              { nil() => (b,(nil,nil))
              | cons(a1,L1') =>
		    { nil() => (b,(nil,nil))
                    | cons(a2,L2') => 
			  { true  () => (b,(L1',L2'))
			  | false () => (cmp(a1,a2),(nil,nil))
			  } (eq_digit(a1,a2))
                    } (L2)
              } (L1)
          |}
            (length(L1))
     }
      ( make_same_length_digits(L1, L2))
    ).


def folddigits {f,g} = (x) =>
    {| zero : () => f
     | succ : (v) => g(v)
     |} (digits_2_nat(x)).

def mul_digits = (L1,L2) =>
   { nil() => nil
   | cons(a,L) =>
	 { d0() => L
         | _  => cons(a,L)
         }(a)
   }
   (p1({| nil : () => (nil,nil)
        | cons: (n,(base,acc)) => (cons(d0,base),
	   add_digits(acc,
               cons
               ({| nil : () => (d0,base)
                 | cons: (m,(carry,acc')) =>
    	          { (tens,ones) => 
		      { (carry',ones) =>
		          (p1(add_digit(carry',tens)),cons(ones,acc'))
                      } (add_digit(ones,carry))
                  }(mul_digit(m,n))
                 |} (L1)
               )
               ))
       |} (L2))).

def div_mod_digits' = (L1,L2) =>
    {| zero : () => (d0,L1)
     | succ : (q,n) => 
	   { true  () => (q,n)
           | false () => (p1(add_digit(q,d1)),sub_digits(n,L2))
           } (cmp_digits{lt_digit} (n,L2))
     |}(succ(succ(succ(succ(succ(succ(succ(succ(succ(zero)))))))))).

def succ_int = (v) =>
INT( {INT(sign,n) =>
      {negative() => 
           { true  () => (positive,cons(d1,nil))
           | false () => (negative,pred_digits(n))
	   } (cmp_digits{x => eq_digit(x)}(n,cons(d0,nil)))
      |positive() => (positive,succ_digits(n))
      } (sign)
     } (v)).

def pred_int = (v) =>
INT( {INT(sign,n) =>
      {negative() => (negative,succ_digits(n))
      |positive() => 
          { true () => (negative,cons(d1,nil))
	  | false() => (positive,pred_digits(n))
          } (cmp_digits{x => eq_digit(x)}(n,cons(d0,nil)))
      } (sign)
     } (v)).

def add_int = (v1,v2) =>
INT(
    {INT(sign1,n1) =>
	{INT(sign2,n2) => 
	    { negative() => 
		{ negative() => (negative,add_digits(n1,n2))
                | positive => 
		      { true () => (negative,sub_digits(n1,n2))
		      | false() => (positive,sub_digits(n2,n1))
		      } (cmp_digits{x => gt_digit(x)}(n1,n2))
                } (sign2)
            | positive() => 
		{ negative() =>
		      { true () => (positive,sub_digits(n1,n2))
		      | false() => (negative,sub_digits(n2,n1))
		      } (cmp_digits{x => gt_digit(x)}(n1,n2))
                | positive() => (positive,add_digits(n1,n2))
                } (sign2)
            } (sign1)
        } (v2)
    } (v1)
   ).

def mul_int = (v1,v2) =>
INT(
    {INT(sign1,n1) =>
	{INT(sign2,n2) => 
	    {n =>
		{ negative() => 
		    { negative() => (positive,n)
		    | positive() => (negative,n)
		    } (sign2)
	        | positive() =>
                    { negative() => (negative,n)
		    | positive() => (positive,n)
                    } (sign2)
                }(sign1)
            }(mul_digits(n1,n2))
         } (v2)
    }(v1)
   ).

def neg_int = v1 =>
    INT( {INT(sign,n) => ({negative() => positive | positive() => negative}(sign), n)} (v1)).

def sub_int = (v1,v2) => add_int(v1,neg_int(v2)).

def foldint {f,g} = (x) =>
    {| zero : () => f
     | succ : (v) => g(v)
     |} (int_2_nat(x)).

def pos_0_int = (v) =>
    INT({INT(sign,n) => 
	 { true () => (positive,nil)
	 | false() => (sign,n)
         } (cmp_digits{x => eq_digit(x)} (compress_digits(n),nil))
        }(v)).

def cmp_int{cmp}  = ((negpos,posneg),(v1,v2)) =>
    {INT(sign1,n1) =>
         {INT(sign2,n2) =>
	      { negative() =>
		  { positive() => negpos
                  | negative() => cmp_digits{cmp}(n2,n1)
                  } (sign2)
              | positive() =>
		  { positive() => cmp_digits{cmp}(n1,n2)
                  | negative() => posneg
                  } (sign2)
              } (sign1)
         } (pos_0_int(v2))
    } (pos_0_int(v1)).


def gt_int = (v1,v2) => cmp_int{gt_digit}((false,true),(v1,v2)).

def ge_int = (v1,v2) => cmp_int{ge_digit}((false,true),(v1,v2)).

def lt_int = (v1,v2) => cmp_int{lt_digit}((true,false),(v1,v2)).

def le_int = (v1,v2) => cmp_int{le_digit}((true,false),(v1,v2)).

def eq_int = (v1,v2) => cmp_int{eq_digit}((false,false),(v1,v2)).
