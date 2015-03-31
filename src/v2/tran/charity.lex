structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 1
val strbuf = ref ""
val comment_level = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)
val error = fn str => (print ("Unknown token:["^String.toCString(str) ^ "]"))
fun resetLineNo() = pos:=1
(*
<INITIAL>"{\\"    => (Tokens.L_MACBRACE(!pos, !pos));
<INITIAL>"\\}"    => (Tokens.R_MACBRACE(!pos, !pos));
<INITIAL>"1"      => (Tokens.ONE(!pos, !pos));
*)

%%
%header (functor chLexFun(structure Tokens: ch_TOKENS));
%s COMMENT;
%s STRENV;

ident   =       [A-Za-z_][A-Za-z0-9_']*;
decNum  =       [0-9]+;
hexNum  =       [0-9A-Fa-f]+;
printableChar= [\ -\~];
ws              =   [\ \t\013];
briefcomment=  \%.*\n|\%.*;

%%
<INITIAL>\"       => (YYBEGIN STRENV; strbuf:=""; lex());
<STRENV>\\\"      => (strbuf:= !strbuf ^ "\""; lex());
<STRENV>\n        => (print("missing right quote:" ^ !strbuf ^"\n");
                      YYBEGIN INITIAL; pos:=(!pos)+1; Tokens.STRING(!strbuf,!pos,!pos));
<STRENV>\"        => (YYBEGIN INITIAL;
                      case String.fromCString (!strbuf)
                        of NONE => (print("unknown escape sequence in string"^(!strbuf));
                                         Tokens.STRING(!strbuf, !pos, !pos))
                         | SOME s=> Tokens.STRING(s, !pos, !pos)
                     );
<STRENV>.         => (strbuf := (!strbuf) ^ yytext ; lex());
<INITIAL>"(*"     => (comment_level:= 1; YYBEGIN COMMENT; lex());
<COMMENT>"(*"     => (comment_level:=(!comment_level)+1; lex());
<COMMENT>"*)"     => (comment_level:=(!comment_level)-1;
                                      if (!comment_level)=0 then (YYBEGIN INITIAL;lex())
                                                            else lex());
<COMMENT>\n       => (pos:=(!pos)+1; lex());
<COMMENT>.        => (lex());
<INITIAL>\n       => (pos:= (!pos) + 1; lex());
<INITIAL>{ws}+    => (lex());
<INITIAL>"data"   => (Tokens.DATA(!pos,!pos));
<INITIAL>"def"    => (Tokens.DEF(!pos,!pos));
<INITIAL>".."     => (Tokens.RANGE(!pos, !pos));
<INITIAL>"->"     => (Tokens.S_ARROW(!pos,!pos));
<INITIAL>"=>"     => (Tokens.D_ARROW(!pos,!pos));
<INITIAL>"("      => (Tokens.L_PAR(!pos,!pos));
<INITIAL>")"      => (Tokens.R_PAR(!pos,!pos));
<INITIAL>"["      => (Tokens.L_BRACKET(!pos,!pos));
<INITIAL>"]"      => (Tokens.R_BRACKET(!pos,!pos));
<INITIAL>"{"      => (Tokens.L_BRACE(!pos,!pos));
<INITIAL>"}"      => (Tokens.R_BRACE(!pos,!pos));
<INITIAL>"{|"     => (Tokens.L_FOLD(!pos,!pos));
<INITIAL>"|}"     => (Tokens.R_FOLD(!pos,!pos));
<INITIAL>"(|"     => (Tokens.L_UNFOLD(!pos,!pos));
<INITIAL>"|)"     => (Tokens.R_UNFOLD(!pos,!pos));
<INITIAL>"{:"     => (Tokens.L_RECUR(!pos,!pos));
<INITIAL>":}"     => (Tokens.R_RECUR(!pos,!pos));
<INITIAL>"="      => (Tokens.EQUALS(!pos,!pos));
<INITIAL>"|"      => (Tokens.BAR(!pos,!pos));
<INITIAL>","      => (Tokens.COMMA(!pos,!pos));
<INITIAL>":"      => (Tokens.COLON(!pos,!pos));
<INITIAL>":\\"    => (Tokens.HIGHERORDERCOLON(!pos, !pos));
<INITIAL>";"      => (Tokens.SEMICOLON(!pos,!pos));
<INITIAL>"."      => (Tokens.STOP(!pos,!pos));

<INITIAL>"+"      => (Tokens.ADD(!pos, !pos));
<INITIAL>"+."      => (Tokens.FADD(!pos, !pos));
<INITIAL>"-"      => (Tokens.SUB(!pos, !pos));
<INITIAL>"-."      => (Tokens.FSUB(!pos, !pos));
<INITIAL>"*"      => (Tokens.MUL(!pos,!pos));
<INITIAL>"*."      => (Tokens.FMUL(!pos,!pos));
<INITIAL>"/"      => (Tokens.DIV(!pos, !pos));
<INITIAL>"/."      => (Tokens.FDIV(!pos, !pos));
<INITIAL>"and"      => (Tokens.AND(!pos, !pos));
<INITIAL>"or"      => (Tokens.OR(!pos, !pos));
<INITIAL>"xor"      => (Tokens.XOR(!pos, !pos));
<INITIAL>"not"    => (Tokens.NOT(!pos, !pos));
<INITIAL>"^"      => (Tokens.STRCAT(!pos, !pos));
<INITIAL>"~"      => (Tokens.NEG(!pos, !pos));
<INITIAL>">"      => (Tokens.GT(!pos, !pos));
<INITIAL>">="     => (Tokens.GE(!pos, !pos));
<INITIAL>"<"      => (Tokens.LT(!pos, !pos));
<INITIAL>"<="     => (Tokens.LE(!pos, !pos));
<INITIAL>"=="     => (Tokens.EQV(!pos, !pos));
<INITIAL>"<>"     => (Tokens.NEQV(!pos, !pos));
<INITIAL>"::"     => (Tokens.LISTINS(!pos, !pos));
<INITIAL>{decNum} => (Tokens.NUM(yytext, !pos, !pos));
<INITIAL>"_"      => (Tokens.DONTCARE(!pos, !pos));
<INITIAL>"#"      => (Tokens.SHARP(!pos, !pos));
<INITIAL>"@"      => (Tokens.AT(!pos, !pos));
<INITIAL>{ident}"{" => (Tokens.COMB_ID(substring(yytext,0, (size yytext)- 1), !pos, !pos));
<INITIAL>{ident}  => (Tokens.ID(yytext,!pos,!pos));
<INITIAL>{briefcomment} => (pos:=(!pos)+1; lex());
<INITIAL>.        => (error(yytext); Tokens.CHAR(yytext, !pos, !pos));
