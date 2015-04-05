(* This file shows how the charity interpreter commands work *)

(* Commands that change the charity category are preceeded with a ":" *)

(* For Example:
:R "UTILS.ch"        % reads in the file UTILS.ch
:rf "UTILS.ch"       % reads in the file UTILS.ch
:r "UTILS.ch"        % reads in the file UTILS.ch
:readfile "UTILS.ch" % reads in the file UTILS.ch

:q      % quits charity
:Q      % quits charity
:quit   % quits charity
*)

% get help for commands
:?.

(* Commands that change the state of the interpreter are preceded with a
   ":set" *)
:set replace functions true.
:set replace functions false.

% set searchpath. Directories are searched in order from left to right.
:set searchpath ".", "Examples".

% append a directory to the searchpath
:set appendpath "..".

% reset the searchpath
:set searchpath ".".

% get help for set commands
:set ?.

(* Query commands let you look at various thing in charity. They are
   all preceded with a "?".*)

% See all entries in symbol table.
? dump table.

% See type of something in symbol table
def foo = x => x.
data coprod(A,B) -> C = b0: A -> C
                      | b1: B -> C.
? foo.
?coprod.
?b1.

% See combinator code of a function.
? comb foo.

% See type of operational combinators of datatypes.
? comb coprod.

% Will we be prompted to replace functions?
? replace.
:set replace functions true.
? replace.

% What's the current searchpath?
? searchpath.
:set appendpath "Examples".
? set searchpath.

% Where is memory being used?
? mem use.

% Will I have troubles if I define a function called "comb" or "set"?
def comb = x => foo(x).
def set = x => foo(comb(x)).
?comb.
?comb comb.
?set.
?comb set.
