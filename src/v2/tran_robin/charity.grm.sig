signature ch_TOKENS =
sig
type ('a,'b) token
type svalue
val APP:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val DEF:  'a * 'a -> (svalue,'a) token
val DATA:  'a * 'a -> (svalue,'a) token
val LISTINS:  'a * 'a -> (svalue,'a) token
val STRCAT:  'a * 'a -> (svalue,'a) token
val APPEND:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val XOR:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val NEQV:  'a * 'a -> (svalue,'a) token
val EQV:  'a * 'a -> (svalue,'a) token
val FLE:  'a * 'a -> (svalue,'a) token
val FLT:  'a * 'a -> (svalue,'a) token
val FGE:  'a * 'a -> (svalue,'a) token
val FGT:  'a * 'a -> (svalue,'a) token
val LE:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val GE:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val FMOD:  'a * 'a -> (svalue,'a) token
val FDIV:  'a * 'a -> (svalue,'a) token
val FMUL:  'a * 'a -> (svalue,'a) token
val FSUB:  'a * 'a -> (svalue,'a) token
val FADD:  'a * 'a -> (svalue,'a) token
val NEG:  'a * 'a -> (svalue,'a) token
val MOD:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val SUB:  'a * 'a -> (svalue,'a) token
val ADD:  'a * 'a -> (svalue,'a) token
val HIGHERORDERCOLON:  'a * 'a -> (svalue,'a) token
val STOP:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val EQUALS:  'a * 'a -> (svalue,'a) token
val R_RECUR:  'a * 'a -> (svalue,'a) token
val L_RECUR:  'a * 'a -> (svalue,'a) token
val R_MACBRACE:  'a * 'a -> (svalue,'a) token
val L_MACBRACE:  'a * 'a -> (svalue,'a) token
val R_UNFOLD:  'a * 'a -> (svalue,'a) token
val L_UNFOLD:  'a * 'a -> (svalue,'a) token
val R_FOLD:  'a * 'a -> (svalue,'a) token
val L_FOLD:  'a * 'a -> (svalue,'a) token
val R_BRACE:  'a * 'a -> (svalue,'a) token
val L_BRACE:  'a * 'a -> (svalue,'a) token
val R_BRACKET:  'a * 'a -> (svalue,'a) token
val L_BRACKET:  'a * 'a -> (svalue,'a) token
val R_PAR:  'a * 'a -> (svalue,'a) token
val L_PAR:  'a * 'a -> (svalue,'a) token
val D_ARROW:  'a * 'a -> (svalue,'a) token
val S_ARROW:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val CHAR: (string) *  'a * 'a -> (svalue,'a) token
val OPER: (string) *  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val FLOAT: (string) *  'a * 'a -> (svalue,'a) token
val NUM: (string) *  'a * 'a -> (svalue,'a) token
val RANGE:  'a * 'a -> (svalue,'a) token
val DONTCARE:  'a * 'a -> (svalue,'a) token
val AT:  'a * 'a -> (svalue,'a) token
val SHARP:  'a * 'a -> (svalue,'a) token
val COMB_ID: (string) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
end
signature ch_LRVALS=
sig
structure Tokens : ch_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
