structure VMCCode = 
struct

	datatype VCode = vConstI of int
	               | vConstA of string
				   | vNewstr of string
				   | vDupB of int
				   | vDupU of int
				   | vDupP of int
				   | vMoveB of int * int
				   | vMoveU of int * int
				   | vMoveP of int * int
				   | vPopB of int
				   | vPopP of int
				   | vCall of string
				   | vGoto of string
				   | vIfnonzero of string
				   | vRetB of int * int
				   | vRetU of int * int
				   | vRetP of int * int
				   | vCase of string list
				   | vNewtuple of int * int
				   | vDetuple
				   | vGetfieldB of int
				   | vGetfieldP of int
                   | vSetfieldB of int 
                   | vSetfieldP of int
				   | vLabel of string
				   | vOther of string

	fun optiCodeSeq [] = []
	  | optiCodeSeq ((vNewtuple _)::(vDetuple)::cs) = optiCodeSeq(cs)
      | optiCodeSeq (vNewtuple(0,0)::cs) = (vOther("nulltuple"))::optiCodeSeq(cs)
	  | optiCodeSeq (vPopB(0)::cs) = optiCodeSeq(cs)
	  | optiCodeSeq (vPopP(0)::cs) = optiCodeSeq(cs)
	  | optiCodeSeq (vMoveB(x1,x2)::cs) = if x1=x2 then optiCodeSeq(cs)
												 else vMoveB(x1,x2)::optiCodeSeq(cs)
	  | optiCodeSeq (vMoveP(x1,x2)::cs) = if x1=x2 then optiCodeSeq(cs)
												  else vMoveP(x1,x2)::optiCodeSeq(cs)
	  | optiCodeSeq (vMoveU(x1,x2)::cs) = if x1=x2 then optiCodeSeq(cs)
												  else vMoveU(x1,x2)::optiCodeSeq(cs)											   
	  | optiCodeSeq (vGoto(label)::vLabel(s)::cs) = 
			if label= s then optiCodeSeq(vLabel(s)::cs)
						else vGoto(label)::optiCodeSeq(vLabel(s)::cs)
	  | optiCodeSeq (vCase([l1,l2])::vLabel(s)::cs) =
			if l1=s then
				vIfnonzero(l2)::optiCodeSeq(vLabel(s)::cs)
			else
				vCase([l1,l2])::vLabel(s)::cs
      | optiCodeSeq (vConstI(x)::vNewtuple(1,0)::cs) = 
			vOther("newint "^Int.toString(x))::optiCodeSeq(cs)
	  | optiCodeSeq (x::xs) = x::optiCodeSeq(xs)

    fun toString code =
		let fun opNONE(opname) = "    " ^ opname
            fun opI (opname,i) = "    " ^ opname ^ " " ^Int.toString(i)
			fun opII(opname,i,j) = "    " ^ opname ^ " " ^Int.toString(i) ^ " " ^ Int.toString(j)
			fun opS (opname,s)  = "    " ^ opname ^ " \"" ^ String.toCString(s) ^ "\""
			fun opL (opname,s)  = "    " ^ opname ^ " " ^ s
            fun catstr [] = ""
			  | catstr(x::xs) = " " ^ x ^ catstr(xs)
			fun opLL (opname,l) = "    " ^ opname ^ catstr(l)
		in
			case code 
			  of vConstI i	=> opI("constI",i)
			   | vConstA s	=> opL("constA",s)
			   | vNewstr s	=> opS("newstr", s)
			   | vDupB i	=> opI("dupB", i)
			   | vDupP i	=> opI("dupP",i)
			   | vDupU i	=> opI("dupU",i)
			   | vMoveB(i,j) => opII("moveB",i,j)  
			   | vMoveU(i,j) => opII("moveU",i,j)  
			   | vMoveP(i,j) => opII("moveP",i,j)
			   | vPopB i	=> opI("popB", i)
			   | vPopP i	=> opI("popP", i)
			   | vCall s	=> opL("call", s)
			   | vGoto s	=> opL("goto", s)
			   | vIfnonzero s  => opL("ifnonzero", s)
			   | vRetB(i,j) => opII("retB",i,j)
			   | vRetU(i,j) => opII("retU",i,j)
			   | vRetP(i,j) => opII("retP",i,j)
			   | vCase l	=> opLL("case",l) 
			   | vNewtuple(i,j)	=> opII("newtuple",i,j)
			   | vDetuple	=> opNONE("detuple")
			   | vGetfieldB(i)	=> opI("getfieldB", i)  
			   | vGetfieldP(i)  => opI("getfieldP",i)
               | vSetfieldP(i)  => opI("setfieldP",i)
               | vSetfieldB(i)   => opI("setfieldB", i)
			   | vLabel s =>  s ^ ":"
			   | vOther s	=> opNONE(s)
		end

	fun printVCode [] = ()
      | printVCode (x::xs) = (print (toString x); print "\n"; printVCode xs)
    fun toText(s,[]) = s
	  | toText(s, x::xs) = toText(s ^"\n"^ (toString x), xs)	
end

