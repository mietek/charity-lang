//Each instruction in VMC is composed of an operator and 0 or more operands.
//this file defines each operator's name, corresponding byte code, and operand type
//Each operator corresponds to only one operand type, i.e. no overloaded operator
#include "vmc.h"

VInstrSpec _instrSpec[]={
    //mathematical instructions for + - * / % , two operands are on top of B stack
    "addI", v_addI, otNone,
    "addL", v_addL, otNone,
    "addF", v_addF, otNone,
    "addD", v_addD, otNone,
    "subI", v_subI, otNone,
    "subL", v_subL, otNone,
    "subF", v_subF, otNone,
    "subD", v_subD, otNone,
    "mulI", v_mulI, otNone,
    "mulL", v_mulL, otNone,
    "mulF", v_mulF, otNone,
    "mulD", v_mulD, otNone,
    "divI", v_divI, otNone,
    "divL", v_divL, otNone,
    "divF", v_divF, otNone,
    "divD", v_divD, otNone,
    "modI", v_modI, otNone,
    "modL", v_modL, otNone,
    "modF", v_modF, otNone,
    "modD", v_modD, otNone,
    
    //get negative of the top element on B stack
    "negI", v_negI, otNone,
    "negL", v_negL, otNone,
    "negF", v_negF, otNone,
    "negD", v_negD, otNone,
    
    //shift left(<<) and shift right(>>) , two operands on top of B stack
    "shl", v_shl, otNone,
    "shr", v_shr, otNone,
    "not" , v_not,  otNone,
    "and", v_and, otNone,
    "or" , v_or,  otNone,
    "bitnot", v_bitnot, otNone,    
    "bitand", v_bitand, otNone,
    "bitor" , v_bitor,  otNone,
    "bitxor", v_bitxor, otNone,
    "eqB", v_eqB, otNone,
    "eqI", v_eqB, otNone,
	"eqF", v_eqB, otNone,
    "eqU", v_eqU, otNone,
	"eqL", v_eqU, otNone,
	"eqD", v_eqU, otNone,
	"eqP", v_eqP, otNone,
	//value comparision = < <=     (!=,>,>= can be get by using 'not')
    "gtI", v_gtI, otNone,
    "gtL", v_gtL, otNone,
    "gtF", v_gtF, otNone,
    "gtD", v_gtD, otNone,
    "geI", v_geI, otNone,
    "geL", v_geL, otNone,
    "geF", v_geF, otNone,
    "geD", v_geD, otNone,

    //data type conversion
    "c2i", v_c2i, otNone,
	"i2c", v_i2c, otNone,
	"s2i", v_s2i, otNone,
	"i2s", v_i2s, otNone,
	"i2l", v_i2l, otNone,
    "i2f", v_i2f, otNone,
    "i2d", v_i2d, otNone,
    "l2i", v_l2i, otNone,
    "l2f", v_l2f, otNone,
    "l2d", v_l2d, otNone,
    "f2i", v_f2i, otNone,
    "f2l", v_f2l, otNone,
    "f2d", v_f2d, otNone,
    "d2i", v_d2i, otNone,
    "d2l", v_d2l, otNone,
    "d2f", v_d2f, otNone,
    //pointer manipulation(NEW, August 6,2001)
//    "tag", v_tag, otNone,
//    "untag", v_untag, otNone,
//    "iftagged", v_iftagged, otLabel,
    //push constant
    "constI", v_constI, otInt,
    "constL", v_constL, otLong,
    "constF", v_constF, otFloat,
    "constD", v_constD, otDouble,
    "constA", v_constA, otLabel,
    
//    "ppushnull", v_ppushnull, otNone,
    //pop elements off stack
    "popB",   v_popB,   otIndex,
    "popP",  v_popP,  otIndex,
    
    //duplicate
    "dupB",  v_dupB,  otIndex,
    "dupU",  v_dupU,  otIndex,
    "dupP",  v_dupP,  otIndex,
    //move element
    "moveB",  v_moveB,  otTwoIndex,
    "moveU",  v_moveU,  otTwoIndex,
    "moveP",  v_moveP,  otTwoIndex,

    //swap top 2 element
    "swapB",  v_swapB,  otNone,
    "swapU", v_swapU, otNone,
    "swapP", v_swapP, otNone,
    
//    "pushframe", v_pushframe, otNone,
//    "popframe",  v_popframe,  otNone,
//    "loadenv",   v_loadenv,   otIndex,
//    "loadenv2",  v_loadenv2,  otIndex,
//    "ploadenv",  v_ploadenv,  otIndex,
//    "saveenv",   v_saveenv,   otIndex,
//    "saveenv2",  v_saveenv2,  otIndex,
//    "psaveenv",  v_psaveenv,  otIndex,

    //flow control
    "ifnonzero",    v_ifnonzero,    otLabel,
    "ifzero",    v_ifzero,    otLabel,
	"ifpos",     v_ifpos, 	  otLabel,
	"ifneg",     v_ifneg,     otLabel,
	"goto",      v_goto,      otLabel,
    "case",      v_case,      otMultiLabel,
    "call",      v_call,      otLabel,
    "xcall",     v_xcall,     otNone,
    "ret",       v_ret,       otNone,
	"retB",      v_retB,      otTwoIndex,
	"retP",      v_retP,      otTwoIndex,
	"retU", 	 v_retU,      otTwoIndex,
	"retM",      v_retM,      otFourIndex,
    "callnative",v_callnative,otLabelTwoIndex,

    //tuple
    "newtuple",  v_newtuple,  otTwoIndex,
	"nulltuple", v_nulltuple, otNone,
    "detuple",   v_detuple,   otNone,
	"tuplesizeB",v_tuplesizeB,otNone,
	"tuplesizeP",v_tuplesizeP,otNone,
    "getfieldB", v_getfieldB, otIndex,
    "getfieldU", v_getfieldU, otIndex,
    "getfieldP", v_getfieldP, otIndex,
    "setfieldB", v_setfieldB, otIndex,
    "setfieldU", v_setfieldU, otIndex,
    "setfieldP", v_setfieldP, otIndex,
	
    "newint",     v_newint,    otInt,
    "newstr",     v_newstr,    otString,
	"strlen",     v_strlen,    otNone,
	"strcat",     v_strcat,    otNone,
	"strcmp",     v_strcmp,    otNone,
    "str2i",      v_str2i,     otNone,
	"i2str",      v_i2str,     otNone,

    "newarrayC",  v_newarrayC,  otNone,
    "newarrayS",  v_newarrayS,  otNone,
    "newarrayB",  v_newarrayB,  otNone,
    "newarrayU",  v_newarrayU,  otNone,
    "newarrayP",  v_newarrayP,  otNone,

    "getitemC",  v_getitemC,  otNone,
    "getitemS",  v_getitemS,  otNone,
    "getitemB",  v_getitemB,  otNone,
    "getitemU",  v_getitemU,  otNone,
    "getitemP",  v_getitemP,  otNone,
    
    "setitemC",  v_setitemC,  otNone,
    "setitemS",  v_setitemS,  otNone,
    "setitemB",  v_setitemB,  otNone,
    "setitemU",  v_setitemU,  otNone,
    "setitemP",  v_setitemP,  otNone,

    "arraysizeC", v_arraysizeC, otNone,
    "arraysizeS", v_arraysizeS, otNone,
    "arraysizeB", v_arraysizeB, otNone,
    "arraysizeU", v_arraysizeU, otNone,
    "arraysizeP", v_arraysizeP, otNone,
    
    /*
    "io_open",    v_io_open,    otNone,
    "io_close",   v_io_close,   otNone,
    "io_read",    v_io_read,    otNone,
    "io_readline",v_io_readline,otNone, 
    "io_write",   v_io_write,   otNone,
    "io_seekg",   v_io_seekg,   otNone,
    "io_seekp",   v_io_seekp,   otNone,
    "io_getc", v_io_getc, otNone,
    "io_putc", v_io_putc, otNone,
    "io_getw", v_io_getw, otNone, 
    "io_putw", v_io_putw, otNone,
    "io_gets", v_io_gets, otNone,
    "io_puts", v_io_puts, otNone,
    "io_geti", v_io_geti, otNone,
    "io_puti", v_io_puti, otNone,
    "io_getf", v_io_getf, otNone,
    "io_putf", v_io_putf, otNone,
    "io_getl", v_io_getl, otNone,
    "io_putl", v_io_putl, otNone,
    "io_getd", v_io_getd, otNone,
    "io_putd", v_io_putd, otNone,
    "io_getstr",  v_io_getstr,  otNone,
    "io_putstr",  v_io_putstr,  otNone,
    "io_getline",  v_io_getline, otNone,
    "io_putline",  v_io_putline, otNone,
    "printstr",    v_printstr, otString,
    */
	  
	//thread
    "startthread",  v_startthread,  otLabelTwoIndex,
    "stopthread",   v_stopthread,   otNone,
    "halt",         v_halt,         otNone,
    "send",    v_send,    otIndex,
    "receive", v_receive, otIndex,
    
    //misc
    "checkstack",  v_checkstack,  otIndex,

    //internal
    "",  v_BREAK, otNone,
    0,   v_lastInstruction, otNone

};
