(* operations on sets, with a set represented as a list of items.
   The list may contain duplicate items, but is considered equivalent
   to the list with duplicate items removed.
   If the original sets contains no duplicate items, the result
   is guaranteed to contain no duplicates.
 *)
structure Set =
struct

fun isMember(aItem, []) = false
  | isMember(aItem, x::xs) = aItem=x orelse isMember(aItem, xs)

(* the union of two sets *)
fun union(set1, []) = set1
  | union(set1, x::xs) =
        if isMember(x, set1) then
            union(set1, xs)
        else
            union(x::set1, xs)
(* set1 minus set2 *)
fun minus([], set2) = []
  | minus(x::xs, set2) =
        if isMember(x, set2) then
            minus(xs, set2)
        else
            x::minus(xs, set2)
(* the intersection of set1 and set2 *)
fun intersect([], set2) = []
  | intersect(x::xs, set2) =
        if isMember(x, set2) then
            x::intersect(xs, set2)
        else
            intersect(xs,set2)

fun insert(x, aSet) = if isMember(x, aSet) then aSet else x::aSet

fun remove(x, []) = []
  | remove(x, y::ys) = if x=y then ys else y::remove(x,ys)

(* whether two sets are equivalent *)
fun equivalent(set1, set2) = minus(set1, set2)=[]  andalso minus(set2, set1)=[]

(*fun findDuplicates [] = []
  | findDuplicates(x::xs) =
        if isMember(x,xs) then x::(findDuplicates xs)
                           else findDuplicates xs

fun hasDuplicates [] = false
  | hasDuplicates(x::xs) =  isMember(x, xs) orelse hasDuplicates xs
*)
(* remove duplicated items in the list representing a set*)
fun removeDuplicates [] = []
  | removeDuplicates (x::xs) = if isMember(x, xs) then
                                   removeDuplicates(xs)
                               else
                                   x::removeDuplicates(xs)
end
(* A map is a set of pairs, each pair contains a key and a value *)
structure Map =
struct
(* find the value associated with a key, return NONE if not found *)
fun find(aKey, []) = NONE
  | find(aKey, (k,v)::kvs) = if aKey=k then SOME v
                                       else find(aKey, kvs)

fun getValue(aKey, aMap) = valOf(find(aKey, aMap))

(* find all the value associated with a key, return a list of the values *)
fun findAll(aKey, []) = []
  | findAll(aKey, (k,v)::kvs) = if aKey=k then v::findAll(aKey, kvs)
                                          else findAll(aKey, kvs)
(* remove the first entry of a key *)
fun remove(aKey, []) = []
  | remove(aKey, (k,v)::kvs) = if aKey=k then kvs
                                         else (k,v)::remove(aKey, kvs)
(* remove all the entry of the key *)
fun removeAll(aKey, []) = []
  | removeAll(aKey, (k,v)::kvs) = if aKey=k then removeAll(aKey, kvs)
                                            else (k,v):: removeAll(aKey, kvs)

fun insert(aKey, aValue, aMap) = (aKey, aValue)::aMap

end

structure ErrorSet =
struct
    local val errmsgPtr: (string list) ref = ref []
    in
        fun resetErrs() = errmsgPtr:=[]
        fun getErrs() = (!errmsgPtr)
        fun hasErr() = getErrs()<>[]
        fun newErr(s) = errmsgPtr:=((!errmsgPtr) @ [s])
        fun newErrs(el) = errmsgPtr := ((!errmsgPtr) @ el)
    end
end

structure Utility =
struct
(* [x0,x1,...xn] => [ f(x0,0), f(x1,1), ..., f(xn,n) ]*)
fun from0toN f l  =
    let fun dofrom0toN(i,[]) = []
          | dofrom0toN(i, x::xs) = f(x,i)::dofrom0toN(i+1,xs)
    in
        dofrom0toN(0,l)
    end

fun enum0toN l = from0toN (fn x=>x) l

fun flattenList [] = []
  | flattenList (l::ls) = l @ (flattenList ls)

(* Sort a list in ascending order, duplicate items are removed *)
fun distinctSort [] = []
  | distinctSort (item::itemlist) =
    let
        fun insert(x,[]) = [x]
          | insert(x,y::ys)=
                if x=y then
                    y::ys
                else if x<y then
                    x::y::ys
                else
                    y::(insert(x,ys))
    in
        insert(item, distinctSort itemlist)
    end

(* find duplicate items in a list
   return [] if no duplicates are found,
   otherwise return a list of distinct items that are duplicate in
   the input list
 *)
fun findDup aList =
    let
        fun dofind(duplist, []) = duplist
          | dofind(duplist, x::xs) =
                if Set.isMember(x, xs) andalso not (Set.isMember(x, duplist)) then
                    dofind(x::duplist, xs)
                else
                    dofind(duplist, xs)
    in
        dofind([], aList)
    end
(* remove duplicate items in a list, the first appearance of an item is kept *)
fun removeDup [] = []
  | removeDup (x::xs) = if List.exists (fn y=>x=y) xs then
                            removeDup(xs)
                        else
                            x::removeDup(xs)

fun getMaxItem(x, []) = x
  | getMaxItem(x, y::ys) = if x<y then getMaxItem(y, ys)
                                  else getMaxItem(x, ys)
(* Get a unique ID *)
local val uniqueID= ref 100
in
    fun resetNewID(id) = (uniqueID:=id; ())
    fun newID () = (uniqueID:=(!uniqueID)+1; (!uniqueID))
    fun newIDList n = List.tabulate(n, fn _=> newID())
    fun newName() = Int.toString(newID())
    fun newNameList n = List.tabulate(n, fn _ => newName())
end

fun getPosition(aItem, aList) =
    let fun getp(_,[]) = ~1
          | getp(n, y::ys) = if aItem=y then n else getp(n+1, ys)
    in
        getp(0, aList)
    end

end (* Utility *)
