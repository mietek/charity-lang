
/*  A Bison parser, made from ../ParseLex/term.y
 by  GNU Bison version 1.27
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	HO_ID	257
#define	FO_ID	258
#define	MAPP	259
#define	PROD	260
#define	SUM	261
#define	STRING	262
#define	INTNEG	263
#define	INTPOS	264
#define	IMP	265
#define	STRNG	266
#define	SYMTABENTRY	267
#define	BOOL	268
#define	DIR	269
#define	BINCHARREP	270
#define	OCTCHARREP	271
#define	DECCHARREP	272
#define	HEXCHARREP	273
#define	IDCHARREP	274
#define	CHAR	275
#define	LPAR	276
#define	RPAR	277
#define	COMMA	278
#define	LBRA	279
#define	RBRA	280
#define	COLON	281
#define	SEMI	282
#define	EQUALS	283
#define	LBRACE	284
#define	RBRACE	285
#define	OR	286
#define	QUOTE	287
#define	OROR	288
#define	NOT	289
#define	UNDERSCORE	290
#define	DATA	291
#define	RANGE	292
#define	DEF	293
#define	SET	294
#define	READFILE	295
#define	LUNFOLD	296
#define	RUNFOLD	297
#define	LFOLD	298
#define	RFOLD	299
#define	ILLEGALCHAR	300
#define	QUIT	301
#define	EXPR	302
#define	EMPTY_INPUT	303
#define	SHOWMEM	304
#define	COMMAND	305
#define	SETCOMMAND	306
#define	QUERY	307
#define	ABOUT	308
#define	SHOWCOMB	309
#define	REPLACE	310
#define	PRINTCTEXPR	311
#define	INCLUDEDIRS	312
#define	DUMPTABLE	313
#define	APPENDDIRS	314
#define	AMPER	315
#define	ALIAS	316
#define	CONSTR	317

#line 1 "../ParseLex/term.y"


#include <stdio.h>
#include "types.h"
#include "commands.h"
#include "parse.h"
#include "pmem.h"
#include "ioChar.h"

extern FILE *yyin, *yyout;

static BBOOL pb_SetCommand = BFALSE;


#line 16 "../ParseLex/term.y"
typedef union {
    char                   *string;
    int                     integer;
    long                    longInt;

    STR_LIST               *strList;

    PE_DATA                *datatype;
    PE_ALIAS               *alias;
    PE_TYPE                *type;
    PE_LIST_TYPE           *typeList;

    PE_TYPE_SIG            *type_sig;
    PE_STRUCTOR_TYPE_SIG   *structor_type_sig;    /* [H-O] ADDED (SEE BELOW) */

    PE_LIST_STRUCTOR       *structorList;

    PE_DEF                 *function;

    PE_MACRO               *mcro;
    PE_LIST_MACRO          *macroList;

    PE_VAR_BASE            *var_base;

    PE_PATT                *patt;
    P_STRUCTOR_ARRAY       *p_structorArr;

    PE_T_PHRASE            *t_phrase;
    PE_LIST_T_PHRASE       *t_phraseList;

    PE_LIST_LIST_T_PHRASE  *t_phraseListList;  /* an array of phraselists */

    PE_TERM                *term;
    PE_LIST_FUN_PHRASE     *macroTerms;     /* [H-O] ALTERED TYPE (SEE BELOW) */
    PE_LIST_TERM           *termList;

    PE_FUN_PHRASE          *funMacroVariant;     /* [H-O] ADDED (SEE BELOW) */

    PE_LIST_RECORD         *recordList;
    PE_LIST_FOLD           *foldList;

    PE_LIST_UNFOLD         *unfoldList;
    PE_CASES_AND_UNFOLDS   *casesAndUnfolds;     /* [H-O] ADDED (SEE BELOW) */

    PE_EXPR                *expr;
} YYSTYPE;
#ifndef YYDEBUG
#define YYDEBUG 1
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		317
#define	YYFLAG		-32768
#define	YYNTBASE	64

#define YYTRANSLATE(x) ((unsigned)(x) <= 317 ? yytranslate[x] : 125)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     3,     4,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
    37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
    47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
    57,    58,    59,    60,    61,    62,    63
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     7,    10,    13,    15,    18,    21,    24,
    27,    29,    31,    34,    38,    41,    44,    47,    49,    51,
    55,    57,    59,    61,    64,    67,    69,    72,    75,    78,
    80,    82,    84,    86,    88,    90,    92,   100,   104,   107,
   109,   110,   114,   116,   120,   124,   128,   132,   138,   140,
   144,   149,   152,   156,   160,   162,   166,   170,   174,   178,
   180,   185,   190,   197,   199,   203,   205,   209,   211,   215,
   218,   219,   221,   223,   226,   228,   231,   235,   241,   245,
   249,   255,   259,   265,   267,   269,   271,   273,   275,   277,
   281,   284,   286,   290,   292,   294,   296,   300,   303,   306,
   308,   310,   312,   314,   316,   318,   322,   324,   327,   329,
   332,   336,   339,   341,   343,   346,   348,   354,   358,   362,
   365,   369,   372,   374,   376,   378,   380,   384,   388,   390,
   396,   400,   402,   406,   410,   411,   412,   418,   419,   420,
   426,   431,   433,   435,   439,   441,   443,   447,   449,   451,
   453,   457,   461,   465,   468,   473,   479,   482,   486,   492,
   494,   496,   500,   504,   510,   516,   520,   526,   532,   538,
   542,   544,   547,   551,   555,   559,   561,   565,   569,   575,
   581,   583
};

static const short yyrhs[] = {    65,
     0,    47,     0,    37,    71,     0,    37,    80,     0,    39,
    81,     0,   100,     0,    41,    98,     0,    51,    66,     0,
    53,    69,     0,    41,    98,     0,    47,     0,    53,     0,
    52,    67,     0,    56,    13,    14,     0,    57,    14,     0,
    58,    68,     0,    60,    68,     0,    53,     0,    98,     0,
    98,    24,    68,     0,    70,     0,    54,     0,    52,     0,
    52,    56,     0,    52,    58,     0,    55,     0,    55,    70,
     0,    55,    55,     0,    55,    52,     0,    59,     0,    50,
     0,    53,     0,   124,     0,     6,     0,    10,     0,     7,
     0,   124,    72,     5,   124,    72,    29,    73,     0,    22,
    79,    23,     0,    22,    23,     0,   124,     0,     0,    74,
    32,    73,     0,    74,     0,   124,    32,    74,     0,   124,
    27,    76,     0,    77,     5,    77,     0,    77,     5,    77,
     0,    77,     5,    77,    11,    77,     0,   124,     0,   124,
    22,    23,     0,   124,    22,    78,    23,     0,   124,    77,
     0,    77,     6,    77,     0,    77,     7,    77,     0,    10,
     0,    22,    77,    23,     0,    77,    24,    78,     0,    77,
    24,    77,     0,   124,    24,    79,     0,   124,     0,   124,
    72,    29,    77,     0,   124,    85,    29,    82,     0,   124,
    85,    27,    75,    29,    82,     0,   113,     0,   124,    27,
    75,     0,   124,     0,    83,    24,    84,     0,    83,     0,
    30,    84,    31,     0,    30,    31,     0,     0,    87,     0,
    88,     0,   124,    87,     0,    90,     0,    22,    23,     0,
    22,    87,    23,     0,    22,    87,    24,    87,    23,     0,
    22,    89,    23,     0,     4,    27,    87,     0,     4,    27,
    87,    24,    89,     0,     3,    27,   124,     0,     3,    27,
   124,    24,    89,     0,   124,     0,    36,     0,    91,     0,
    94,     0,    96,     0,    99,     0,    25,    92,    26,     0,
    25,    26,     0,    87,     0,    87,    24,    92,     0,     9,
     0,    10,     0,    93,     0,    93,    38,    93,     0,    93,
    38,     0,    38,    93,     0,    16,     0,    17,     0,    18,
     0,    19,     0,    20,     0,    95,     0,    95,    38,    95,
     0,    21,     0,    21,    97,     0,    95,     0,    95,    97,
     0,    33,    97,    33,     0,    33,    33,     0,    98,     0,
   101,     0,   105,   100,     0,   102,     0,    22,   100,    24,
   100,    23,     0,    22,   100,    23,     0,    22,   123,    23,
     0,    22,    23,     0,    25,   104,    26,     0,    25,    26,
     0,   124,     0,    98,     0,    93,     0,    95,     0,   100,
    28,   103,     0,    30,   113,    31,     0,   114,     0,    30,
   113,    31,    28,   103,     0,   114,    28,   103,     0,   100,
     0,   100,    24,   104,     0,    30,   113,    31,     0,     0,
     0,    44,   106,   117,   107,    45,     0,     0,     0,    42,
   108,   119,   109,    43,     0,   124,    30,   110,    31,     0,
   124,     0,   111,     0,   111,    24,   110,     0,    36,     0,
   112,     0,   112,    61,   112,     0,   105,     0,   113,     0,
   114,     0,   114,    32,   113,     0,    86,    11,   100,     0,
    86,    32,   115,     0,    38,   116,     0,   100,   116,    32,
   115,     0,    35,   100,   116,    32,   115,     0,    11,   100,
     0,   124,    27,   114,     0,   124,    27,   114,    32,   118,
     0,   117,     0,   114,     0,   114,    32,   118,     0,    86,
    11,   120,     0,     4,    27,   121,    32,   120,     0,     4,
    27,   121,    32,   119,     0,     4,    27,   121,     0,     3,
    27,   114,    32,   122,     0,     3,    27,   114,    32,   120,
     0,     3,    27,   114,    32,   119,     0,     3,    27,   114,
     0,   100,     0,    32,   115,     0,   114,    32,   122,     0,
   114,    32,   120,     0,   114,    32,   119,     0,   114,     0,
     4,    27,   100,     0,     3,    27,   113,     0,     4,    27,
   100,    24,   123,     0,     3,    27,   113,    24,   123,     0,
     3,     0,     4,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   133,   134,   139,   142,   145,   148,   152,   156,   163,   169,
   171,   173,   174,   180,   184,   187,   190,   193,   196,   197,
   202,   204,   205,   207,   208,   209,   211,   213,   215,   217,
   218,   219,   221,   222,   223,   224,   230,   233,   234,   235,
   236,   239,   240,   246,   247,   250,   255,   256,   260,   261,
   262,   263,   264,   266,   268,   269,   273,   274,   278,   279,
   287,   296,   297,   300,   302,   303,   306,   307,   310,   311,
   312,   319,   323,   324,   325,   329,   330,   331,   332,   337,
   338,   339,   340,   345,   346,   347,   348,   349,   350,   359,
   360,   364,   365,   375,   376,   380,   381,   382,   383,   393,
   394,   395,   396,   397,   401,   402,   412,   413,   414,   415,
   419,   420,   424,   433,   435,   436,   440,   441,   446,   447,
   451,   452,   453,   454,   455,   456,   457,   461,   462,   463,
   464,   468,   471,   493,   494,   494,   494,   495,   495,   495,
   497,   498,   502,   503,   507,   508,   509,   513,   514,   523,
   524,   528,   529,   533,   536,   537,   541,   554,   555,   560,
   561,   562,   583,   588,   591,   594,   597,   604,   611,   614,
   619,   620,   623,   624,   625,   626,   646,   649,   652,   655,
   661,   662
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","HO_ID",
"FO_ID","MAPP","PROD","SUM","STRING","INTNEG","INTPOS","IMP","STRNG","SYMTABENTRY",
"BOOL","DIR","BINCHARREP","OCTCHARREP","DECCHARREP","HEXCHARREP","IDCHARREP",
"CHAR","LPAR","RPAR","COMMA","LBRA","RBRA","COLON","SEMI","EQUALS","LBRACE",
"RBRACE","OR","QUOTE","OROR","NOT","UNDERSCORE","DATA","RANGE","DEF","SET","READFILE",
"LUNFOLD","RUNFOLD","LFOLD","RFOLD","ILLEGALCHAR","QUIT","EXPR","EMPTY_INPUT",
"SHOWMEM","COMMAND","SETCOMMAND","QUERY","ABOUT","SHOWCOMB","REPLACE","PRINTCTEXPR",
"INCLUDEDIRS","DUMPTABLE","APPENDDIRS","AMPER","ALIAS","CONSTR","START","cmd",
"command","setcommand","dirList","query","querystring","data","tvars","struct_list",
"struct","stype","stype2","dom","dom_list","id_list","alias","def","fun_body",
"macro","macro_list","macros","patt","patty","pattx","pattd","patt_ext","listPatt",
"listPattx","int","intPatt","char","charPatt","stringx","string","strPatt","term",
"termx","term_ext","semColPhrases","term_list","fun","@1","@2","@3","@4","fun_macros",
"fun_macro_variant","fun_macro","cases","casephrase","guardBool","guardBoolx",
"fld","fldphrase","unfld","threads","guardUnfld","morecases","recrds","ID", NULL
};
#endif

static const short yyr1[] = {     0,
    64,    64,    65,    65,    65,    65,    65,    65,    65,    66,
    66,    66,    66,    67,    67,    67,    67,    67,    68,    68,
    69,    69,    69,    69,    69,    69,    69,    69,    69,    69,
    69,    69,    70,    70,    70,    70,    71,    72,    72,    72,
    72,    73,    73,    74,    74,    75,    76,    76,    77,    77,
    77,    77,    77,    77,    77,    77,    78,    78,    79,    79,
    80,    81,    81,    82,    83,    83,    84,    84,    85,    85,
    85,    86,    87,    87,    87,    88,    88,    88,    88,    89,
    89,    89,    89,    90,    90,    90,    90,    90,    90,    91,
    91,    92,    92,    93,    93,    94,    94,    94,    94,    95,
    95,    95,    95,    95,    96,    96,    97,    97,    97,    97,
    98,    98,    99,   100,   100,   100,   101,   101,   101,   101,
   102,   102,   102,   102,   102,   102,   102,   103,   103,   103,
   103,   104,   104,   105,   106,   107,   105,   108,   109,   105,
   105,   105,   110,   110,   111,   111,   111,   112,   112,   113,
   113,   114,   114,   115,   115,   115,   116,   117,   117,   118,
   118,   118,   119,   120,   120,   120,   120,   120,   120,   120,
   121,   121,   122,   122,   122,   122,   123,   123,   123,   123,
   124,   124
};

static const short yyr2[] = {     0,
     1,     1,     2,     2,     2,     1,     2,     2,     2,     2,
     1,     1,     2,     3,     2,     2,     2,     1,     1,     3,
     1,     1,     1,     2,     2,     1,     2,     2,     2,     1,
     1,     1,     1,     1,     1,     1,     7,     3,     2,     1,
     0,     3,     1,     3,     3,     3,     3,     5,     1,     3,
     4,     2,     3,     3,     1,     3,     3,     3,     3,     1,
     4,     4,     6,     1,     3,     1,     3,     1,     3,     2,
     0,     1,     1,     2,     1,     2,     3,     5,     3,     3,
     5,     3,     5,     1,     1,     1,     1,     1,     1,     3,
     2,     1,     3,     1,     1,     1,     3,     2,     2,     1,
     1,     1,     1,     1,     1,     3,     1,     2,     1,     2,
     3,     2,     1,     1,     2,     1,     5,     3,     3,     2,
     3,     2,     1,     1,     1,     1,     3,     3,     1,     5,
     3,     1,     3,     3,     0,     0,     5,     0,     0,     5,
     4,     1,     1,     3,     1,     1,     3,     1,     1,     1,
     3,     3,     3,     2,     4,     5,     2,     3,     5,     1,
     1,     3,     3,     5,     5,     3,     5,     5,     5,     3,
     1,     2,     3,     3,     3,     1,     3,     3,     5,     5,
     1,     1
};

static const short yydefact[] = {     0,
   181,   182,    94,    95,   100,   101,   102,   103,   104,     0,
     0,     0,     0,     0,     0,     0,   138,   135,     2,     0,
     0,     1,   125,   126,   124,     6,   114,   116,     0,   142,
   181,   182,   120,     0,     0,   122,   132,     0,     0,     0,
    85,     0,     0,    72,    73,    75,    86,    96,    87,   105,
    88,   113,    89,     0,   150,    84,   107,   112,   109,     0,
     3,     4,    41,     5,    71,     7,     0,     0,     0,    11,
     0,    12,     8,    34,    36,    35,    31,    23,    32,    22,
    26,    30,     9,    21,    33,     0,   115,     0,     0,     0,
   118,     0,   119,     0,   121,   181,   182,    76,     0,     0,
    91,    92,     0,    99,     0,     0,    98,     0,   134,     0,
    74,   108,   110,   111,     0,     0,    40,     0,     0,     0,
   139,   136,     0,    10,    18,     0,     0,     0,     0,    13,
    24,    25,    29,    28,    27,     0,   127,   129,    85,   148,
     0,   143,   146,   149,   142,   178,   177,     0,   133,     0,
     0,    77,     0,    79,     0,    90,   152,     0,     0,     0,
   153,    97,   106,   151,    39,     0,    60,     0,     0,    70,
    68,     0,    66,     0,     0,     0,     0,     0,     0,     0,
    15,    16,    19,    17,     0,     0,   141,     0,     0,     0,
     0,   117,    82,    80,     0,    93,     0,     0,   154,     0,
    38,     0,    41,    55,     0,    61,    49,     0,    69,     0,
     0,     0,    62,    64,     0,     0,   163,   140,   137,   158,
    14,     0,   128,   131,   144,   147,     0,     0,   180,   179,
     0,     0,    78,     0,   157,     0,    59,     0,     0,     0,
     0,     0,    52,    67,    65,     0,     0,     0,     0,     0,
    20,     0,     0,     0,    83,    81,     0,   155,     0,    56,
    53,    54,    50,     0,     0,    63,    46,   170,     0,   171,
   166,   161,   160,   159,    84,   130,   156,    37,    43,     0,
     0,    51,     0,   172,     0,     0,     0,     0,     0,    58,
    57,   181,   182,     0,   176,   169,   168,   167,   165,   164,
   162,    42,    45,     0,    44,     0,     0,     0,   175,   174,
   173,    47,     0,    48,     0,     0,     0
};

static const short yydefgoto[] = {   315,
    22,    73,   130,   182,    83,    84,    61,   116,   278,   279,
   211,   303,   212,   265,   166,    62,    64,   213,   171,   172,
   119,    43,    44,    45,   100,    46,    47,   103,    48,    49,
    50,    51,    60,    52,    53,   160,    27,    28,   137,    38,
    29,    68,   178,    67,   177,   141,   142,   143,   144,    55,
   161,   199,   273,   274,   121,   217,   271,   298,    35,    56
};

static const short yypact[] = {   370,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   531,
   562,   816,   339,   169,   169,     1,-32768,-32768,-32768,   110,
   312,-32768,-32768,-32768,-32768,    95,-32768,-32768,   624,   300,
    15,   118,-32768,   137,    84,-32768,    38,   122,   716,   741,
-32768,   191,    21,-32768,-32768,-32768,-32768,    94,-32768,   114,
-32768,-32768,-32768,   125,   127,   816,   235,-32768,   235,   142,
-32768,-32768,    35,-32768,   140,-32768,   816,   169,     1,-32768,
   167,-32768,-32768,-32768,-32768,-32768,-32768,    28,-32768,-32768,
    20,-32768,-32768,-32768,-32768,   766,-32768,   428,   816,   624,
-32768,   624,-32768,   624,-32768,   152,   154,-32768,   188,   164,
-32768,   180,   183,-32768,   624,   464,   191,   244,-32768,   816,
-32768,-32768,-32768,-32768,    46,     8,-32768,    14,    58,   182,
-32768,-32768,   187,-32768,-32768,   209,   212,     1,     1,-32768,
-32768,-32768,-32768,-32768,-32768,   816,-32768,   204,   -12,-32768,
   207,   221,   197,-32768,   691,   224,    80,   106,-32768,   169,
   816,-32768,   816,-32768,   816,-32768,    95,   624,   254,    65,
-32768,-32768,-32768,-32768,-32768,   256,   270,   169,    79,-32768,
   275,   272,   274,    79,   816,   214,   259,   264,   816,   290,
-32768,-32768,   288,-32768,   289,   766,-32768,   428,   500,   226,
   226,-32768,   297,   301,   304,-32768,    65,   624,-32768,   303,
-32768,   169,    35,-32768,    79,   265,   133,   169,-32768,    79,
   307,   178,-32768,-32768,   302,   306,-32768,-32768,-32768,   305,
-32768,     1,   310,-32768,-32768,-32768,    15,   118,-32768,-32768,
   271,   271,-32768,   308,    95,   464,-32768,   317,   104,    79,
    79,    57,-32768,-32768,-32768,   816,    79,   816,   593,   816,
-32768,   766,   152,   154,-32768,-32768,   464,-32768,   169,-32768,
-32768,   335,-32768,    89,   325,-32768,   265,   318,   464,    95,
   319,   320,-32768,-32768,   791,-32768,-32768,-32768,   321,   112,
    79,-32768,   841,-32768,   841,   816,   169,    79,   169,    67,
-32768,   302,   306,    24,   331,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,   185,-32768,   655,   841,    79,-32768,-32768,
-32768,   160,    79,   265,   349,   368,-32768
};

static const short yypgoto[] = {-32768,
-32768,-32768,-32768,  -125,-32768,   294,-32768,   166,    83,    87,
   168,-32768,    26,    96,   179,-32768,-32768,   136,-32768,   175,
-32768,   -64,   -31,-32768,    51,-32768,-32768,   230,   186,-32768,
    41,-32768,    62,    48,-32768,    36,-32768,-32768,  -181,   299,
   -86,-32768,-32768,-32768,-32768,   203,-32768,   205,   -11,   -70,
  -229,  -154,   328,   111,  -263,  -252,-32768,    91,   105,     0
};


#define	YYLAST		879


static const short yytable[] = {    30,
    54,   140,   120,   184,   224,   200,   258,    99,   102,    30,
    30,  -145,   168,    63,    65,   138,     1,     2,  -145,   296,
    85,   299,     1,     2,   111,    74,    75,   277,    30,    76,
   297,   105,   300,    13,   306,    26,   169,     1,     2,   284,
    24,    89,   234,   309,   170,    34,    37,    25,     1,     2,
    24,    24,   106,    59,   310,   106,   115,    25,    25,     1,
     2,    94,   117,    66,    87,    86,   204,   123,   165,    24,
   276,   133,   240,   241,   134,   198,    25,   146,   205,   263,
    85,     1,     2,   131,   174,   132,   175,   145,   204,    30,
   281,    30,    86,    30,   240,   241,   251,    59,   164,    59,
   205,   140,   140,   191,    30,    30,    93,    86,   220,   240,
   241,   260,   281,   111,   167,   138,   124,   173,   112,   194,
   113,   195,    86,   102,   185,   147,   260,   148,   192,    37,
    24,   107,    24,    86,    24,     1,     2,    25,   288,    25,
   157,    25,   204,   289,    90,    24,    24,    95,   163,   193,
    69,   108,    25,    25,   242,   109,    70,    30,   110,    91,
    92,    71,    72,   214,    86,   240,   241,   203,   207,   118,
   313,     1,     2,   207,   114,   183,   183,   268,   150,   272,
   151,   138,   247,   240,   241,    23,   154,   145,   145,   308,
   240,   241,   176,   197,   206,    23,    23,    30,    24,     3,
     4,   167,   117,   155,   207,    25,   207,   173,   156,   207,
   152,   153,   295,   179,    23,   272,   215,   216,   294,   125,
   120,   180,   126,   127,   128,   181,   129,   104,   227,   228,
   239,   186,   243,   235,   214,    30,   295,   187,    24,   207,
   207,   207,   294,   111,   188,    25,   207,   190,    30,   275,
     5,     6,     7,     8,     9,    57,    30,   189,   280,     5,
     6,     7,     8,     9,   198,   261,   262,   264,    30,   183,
   240,   241,   267,   253,   254,    23,    24,    23,   201,    23,
   207,   255,   256,    25,   270,   275,   280,   207,   280,    24,
    23,    23,   162,   202,   229,   230,    25,    24,   208,  -123,
   210,   218,   209,   221,    25,    30,   290,   207,   219,    24,
  -123,   222,   207,   304,     1,     2,    25,    74,    75,   223,
   231,    76,  -123,  -123,   232,  -123,   233,  -123,   248,    88,
  -123,  -123,   249,   312,   236,   246,   250,   252,   314,   257,
   240,   157,  -123,    23,  -123,   259,    24,   282,   316,   283,
   285,   286,   287,    25,     5,     6,     7,     8,     9,    57,
  -123,    77,   307,    78,    79,    80,    81,   317,   238,   302,
    82,    58,     1,     2,   135,   305,   291,   245,     3,     4,
   237,   266,   244,    23,   196,     5,     6,     7,     8,     9,
   225,    10,   149,   226,    11,   122,   301,   311,     0,    12,
     0,     0,    13,     0,     0,     0,    14,     0,    15,     0,
    16,    17,     0,    18,     0,     0,    19,     0,     0,     0,
    20,    23,    21,     0,     0,     0,     0,     0,     0,     0,
     1,     2,     0,     0,    23,     0,     3,     4,     0,     0,
     0,     0,    23,     5,     6,     7,     8,     9,     0,    39,
     0,     0,    40,     0,    23,     0,     0,    12,     0,     0,
    13,     0,     0,   139,     0,    42,     1,     2,     0,    17,
     0,    18,     3,     4,     0,     0,     0,     0,     0,     5,
     6,     7,     8,     9,     0,    10,     0,     0,    11,     0,
     0,    23,     0,    12,     0,     0,    13,     0,   158,     0,
     0,   159,     1,     2,     0,    17,     0,    18,     3,     4,
     0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
     0,    39,     0,     0,    40,     0,     0,     0,     0,    12,
     0,     0,    13,    31,    32,    41,     0,    42,     0,     3,
     4,    17,     0,    18,     0,     0,     5,     6,     7,     8,
     9,     0,    10,    33,     0,    11,     0,     0,     0,     0,
    12,     0,     0,    13,     1,     2,     0,     0,     0,     0,
     3,     4,    17,     0,    18,     0,     0,     5,     6,     7,
     8,     9,     0,    10,     0,     0,    11,    36,     0,     0,
     0,    12,     0,     0,    13,     1,     2,     0,     0,     0,
     0,     3,     4,    17,     0,    18,     0,     0,     5,     6,
     7,     8,     9,     0,    10,     0,     0,    11,     0,     0,
     0,     0,    12,     0,   269,    13,     1,     2,     0,     0,
     0,     0,     3,     4,    17,     0,    18,     0,     0,     5,
     6,     7,     8,     9,     0,    10,     0,     0,    11,     0,
     0,     0,     0,    12,     0,     0,    13,   292,   293,     0,
     0,     0,     0,     3,     4,    17,     0,    18,     0,     0,
     5,     6,     7,     8,     9,     0,    10,     0,     0,    11,
     0,     0,     0,     0,    12,     0,     0,    13,     0,     0,
     0,     0,     0,     1,     2,     0,    17,     0,    18,     3,
     4,   -84,     0,     0,     0,     0,     5,     6,     7,     8,
     9,     0,    39,     0,     0,    40,     0,     0,    96,    97,
    88,     0,   -84,    13,     3,     4,    41,     0,    42,     0,
     0,     5,     6,     7,     8,     9,     0,    39,    98,     0,
    40,     0,     0,     1,     2,     0,     0,     0,    13,     3,
     4,    41,     0,    42,     0,     0,     5,     6,     7,     8,
     9,     0,    39,     0,     0,    40,   101,     0,     1,     2,
     0,     0,     0,    13,     3,     4,    41,     0,    42,     0,
     0,     5,     6,     7,     8,     9,     0,    39,     0,     0,
    40,     0,     0,     1,     2,   136,     0,     0,    13,     3,
     4,    41,     0,    42,     0,     0,     5,     6,     7,     8,
     9,     0,    39,     0,     0,    40,     0,   179,     1,     2,
     0,     0,     0,    13,     3,     4,    41,     0,    42,     0,
     0,     5,     6,     7,     8,     9,     0,    39,     0,     0,
    40,     0,     0,   292,   293,     0,     0,     0,    13,     3,
     4,    41,     0,    42,     0,     0,     5,     6,     7,     8,
     9,     0,    39,     0,     0,    40,     0,     0,     0,     0,
     0,     0,     0,    13,     0,     0,    41,     0,    42
};

static const short yycheck[] = {     0,
    12,    88,    67,   129,   186,   160,   236,    39,    40,    10,
    11,    24,     5,    14,    15,    86,     3,     4,    31,   283,
    21,   285,     3,     4,    56,     6,     7,   257,    29,    10,
   283,    11,   285,    33,    11,     0,    29,     3,     4,   269,
     0,    27,   197,   307,    31,    10,    11,     0,     3,     4,
    10,    11,    32,    13,   307,    32,    22,    10,    11,     3,
     4,    24,    63,    16,    29,    28,    10,    68,    23,    29,
   252,    52,     6,     7,    55,    11,    29,    89,    22,    23,
    81,     3,     4,    56,    27,    58,    29,    88,    10,    90,
    24,    92,    28,    94,     6,     7,   222,    57,   110,    59,
    22,   188,   189,    24,   105,   106,    23,    28,   179,     6,
     7,    23,    24,   145,   115,   186,    69,   118,    57,   151,
    59,   153,    28,   155,   136,    90,    23,    92,    23,    94,
    90,    38,    92,    28,    94,     3,     4,    90,    27,    92,
   105,    94,    10,    32,    27,   105,   106,    26,   108,   150,
    41,    38,   105,   106,    22,    31,    47,   158,    32,    23,
    24,    52,    53,   175,    28,     6,     7,   168,   169,    30,
    11,     3,     4,   174,    33,   128,   129,   248,    27,   250,
    27,   252,     5,     6,     7,     0,    23,   188,   189,     5,
     6,     7,    11,   158,   169,    10,    11,   198,   158,     9,
    10,   202,   203,    24,   205,   158,   207,   208,    26,   210,
    23,    24,   283,    27,    29,   286,     3,     4,   283,    53,
   285,    13,    56,    57,    58,    14,    60,    42,     3,     4,
   205,    28,   207,   198,   246,   236,   307,    31,   198,   240,
   241,   242,   307,   275,    24,   198,   247,    24,   249,   250,
    16,    17,    18,    19,    20,    21,   257,    61,   259,    16,
    17,    18,    19,    20,    11,   240,   241,   242,   269,   222,
     6,     7,   247,     3,     4,    90,   236,    92,    23,    94,
   281,   231,   232,   236,   249,   286,   287,   288,   289,   249,
   105,   106,   107,    24,   190,   191,   249,   257,    24,     0,
    27,    43,    31,    14,   257,   306,   281,   308,    45,   269,
    11,    24,   313,   288,     3,     4,   269,     6,     7,    31,
    24,    10,    23,    24,    24,    26,    23,    28,    27,    30,
    31,    32,    27,   308,    32,    29,    32,    28,   313,    32,
     6,   306,    43,   158,    45,    29,   306,    23,     0,    32,
    32,    32,    32,   306,    16,    17,    18,    19,    20,    21,
    61,    50,    32,    52,    53,    54,    55,     0,   203,   287,
    59,    33,     3,     4,    81,   289,   281,   210,     9,    10,
   202,   246,   208,   198,   155,    16,    17,    18,    19,    20,
   188,    22,    94,   189,    25,    68,   286,   307,    -1,    30,
    -1,    -1,    33,    -1,    -1,    -1,    37,    -1,    39,    -1,
    41,    42,    -1,    44,    -1,    -1,    47,    -1,    -1,    -1,
    51,   236,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     3,     4,    -1,    -1,   249,    -1,     9,    10,    -1,    -1,
    -1,    -1,   257,    16,    17,    18,    19,    20,    -1,    22,
    -1,    -1,    25,    -1,   269,    -1,    -1,    30,    -1,    -1,
    33,    -1,    -1,    36,    -1,    38,     3,     4,    -1,    42,
    -1,    44,     9,    10,    -1,    -1,    -1,    -1,    -1,    16,
    17,    18,    19,    20,    -1,    22,    -1,    -1,    25,    -1,
    -1,   306,    -1,    30,    -1,    -1,    33,    -1,    35,    -1,
    -1,    38,     3,     4,    -1,    42,    -1,    44,     9,    10,
    -1,    -1,    -1,    -1,    -1,    16,    17,    18,    19,    20,
    -1,    22,    -1,    -1,    25,    -1,    -1,    -1,    -1,    30,
    -1,    -1,    33,     3,     4,    36,    -1,    38,    -1,     9,
    10,    42,    -1,    44,    -1,    -1,    16,    17,    18,    19,
    20,    -1,    22,    23,    -1,    25,    -1,    -1,    -1,    -1,
    30,    -1,    -1,    33,     3,     4,    -1,    -1,    -1,    -1,
     9,    10,    42,    -1,    44,    -1,    -1,    16,    17,    18,
    19,    20,    -1,    22,    -1,    -1,    25,    26,    -1,    -1,
    -1,    30,    -1,    -1,    33,     3,     4,    -1,    -1,    -1,
    -1,     9,    10,    42,    -1,    44,    -1,    -1,    16,    17,
    18,    19,    20,    -1,    22,    -1,    -1,    25,    -1,    -1,
    -1,    -1,    30,    -1,    32,    33,     3,     4,    -1,    -1,
    -1,    -1,     9,    10,    42,    -1,    44,    -1,    -1,    16,
    17,    18,    19,    20,    -1,    22,    -1,    -1,    25,    -1,
    -1,    -1,    -1,    30,    -1,    -1,    33,     3,     4,    -1,
    -1,    -1,    -1,     9,    10,    42,    -1,    44,    -1,    -1,
    16,    17,    18,    19,    20,    -1,    22,    -1,    -1,    25,
    -1,    -1,    -1,    -1,    30,    -1,    -1,    33,    -1,    -1,
    -1,    -1,    -1,     3,     4,    -1,    42,    -1,    44,     9,
    10,    11,    -1,    -1,    -1,    -1,    16,    17,    18,    19,
    20,    -1,    22,    -1,    -1,    25,    -1,    -1,     3,     4,
    30,    -1,    32,    33,     9,    10,    36,    -1,    38,    -1,
    -1,    16,    17,    18,    19,    20,    -1,    22,    23,    -1,
    25,    -1,    -1,     3,     4,    -1,    -1,    -1,    33,     9,
    10,    36,    -1,    38,    -1,    -1,    16,    17,    18,    19,
    20,    -1,    22,    -1,    -1,    25,    26,    -1,     3,     4,
    -1,    -1,    -1,    33,     9,    10,    36,    -1,    38,    -1,
    -1,    16,    17,    18,    19,    20,    -1,    22,    -1,    -1,
    25,    -1,    -1,     3,     4,    30,    -1,    -1,    33,     9,
    10,    36,    -1,    38,    -1,    -1,    16,    17,    18,    19,
    20,    -1,    22,    -1,    -1,    25,    -1,    27,     3,     4,
    -1,    -1,    -1,    33,     9,    10,    36,    -1,    38,    -1,
    -1,    16,    17,    18,    19,    20,    -1,    22,    -1,    -1,
    25,    -1,    -1,     3,     4,    -1,    -1,    -1,    33,     9,
    10,    36,    -1,    38,    -1,    -1,    16,    17,    18,    19,
    20,    -1,    22,    -1,    -1,    25,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    33,    -1,    -1,    36,    -1,    38
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/local/share/bison.simple"
/* This file comes from bison-1.27.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 216 "/usr/local/share/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 133 "../ParseLex/term.y"
{;
    break;}
case 2:
#line 134 "../ParseLex/term.y"
{ ParseResult.tag              = COMMAND;
		   ParseResult.info.command.tag = QUIT;
		   return 0; ;
    break;}
case 3:
#line 139 "../ParseLex/term.y"
{ ParseResult.tag        = DATA;
			    ParseResult.info.data  = yyvsp[0].datatype;
			  ;
    break;}
case 4:
#line 142 "../ParseLex/term.y"
{ ParseResult.tag        = ALIAS;
			    ParseResult.info.alias = yyvsp[0].alias;
			  ;
    break;}
case 5:
#line 145 "../ParseLex/term.y"
{ ParseResult.tag        = DEF;
			    ParseResult.info.def   = yyvsp[0].function;
			  ;
    break;}
case 6:
#line 148 "../ParseLex/term.y"
{ ParseResult.tag        = EXPR;
			    ParseResult.info.expr  = yyvsp[0].expr;
			  ;
    break;}
case 7:
#line 152 "../ParseLex/term.y"
{ ParseResult.tag = COMMAND;
			    ParseResult.info.command.tag = READFILE; 
			    ParseResult.info.command.info.readfile = yyvsp[0].string;
			  ;
    break;}
case 8:
#line 156 "../ParseLex/term.y"
{ if (pb_SetCommand)
			      ParseResult.tag = SETCOMMAND;
                            else
			      ParseResult.tag = COMMAND;

			    pb_SetCommand = BFALSE;
                          ;
    break;}
case 9:
#line 163 "../ParseLex/term.y"
{ ParseResult.tag = QUERY ;
    break;}
case 10:
#line 169 "../ParseLex/term.y"
{ParseResult.info.command.tag =READFILE;
				 ParseResult.info.command.info.readfile = yyvsp[0].string; ;
    break;}
case 11:
#line 171 "../ParseLex/term.y"
{ParseResult.tag = COMMAND;
				 ParseResult.info.command.tag = QUIT; return 0;
    break;}
case 12:
#line 173 "../ParseLex/term.y"
{ParseResult.info.command.tag = QUERY;
    break;}
case 13:
#line 174 "../ParseLex/term.y"
{pb_SetCommand = BTRUE ;
    break;}
case 14:
#line 181 "../ParseLex/term.y"
{ParseResult.info.setcommand.tag = REPLACE;
	        ParseResult.info.setcommand.info.replace.entryType= yyvsp[-1].string;
		ParseResult.info.setcommand.info.replace.doReplace = yyvsp[0].string;
    break;}
case 15:
#line 185 "../ParseLex/term.y"
{ParseResult.info.setcommand.tag = PRINTCTEXPR;
	        ParseResult.info.setcommand.info.doPrintCT_EXPR = yyvsp[0].string;
    break;}
case 16:
#line 188 "../ParseLex/term.y"
{ParseResult.info.setcommand.tag = INCLUDEDIRS;
		ParseResult.info.setcommand.info.dirList = yyvsp[0].strList;
    break;}
case 17:
#line 191 "../ParseLex/term.y"
{ParseResult.info.setcommand.tag = APPENDDIRS;
		ParseResult.info.setcommand.info.dirList = yyvsp[0].strList;
    break;}
case 18:
#line 194 "../ParseLex/term.y"
{ParseResult.info.setcommand.tag = QUERY;
    break;}
case 19:
#line 196 "../ParseLex/term.y"
{ yyval.strList = pe_StrListCons(yyvsp[0].string, NULL);
    break;}
case 20:
#line 197 "../ParseLex/term.y"
{ yyval.strList = pe_StrListCons(yyvsp[-2].string, yyvsp[0].strList);
    break;}
case 21:
#line 202 "../ParseLex/term.y"
{ParseResult.info.query.tag = STRNG;
			       ParseResult.info.query.info.query = yyvsp[0].string;
    break;}
case 22:
#line 204 "../ParseLex/term.y"
{ParseResult.info.query.tag = ABOUT;
    break;}
case 23:
#line 205 "../ParseLex/term.y"
{ParseResult.info.query.tag = STRNG;
			       ParseResult.info.query.info.query = "set";
    break;}
case 24:
#line 207 "../ParseLex/term.y"
{ParseResult.info.query.tag = REPLACE;
    break;}
case 25:
#line 208 "../ParseLex/term.y"
{ParseResult.info.query.tag = INCLUDEDIRS;
    break;}
case 26:
#line 209 "../ParseLex/term.y"
{ParseResult.info.query.tag = STRNG;
			       ParseResult.info.query.info.query = "comb";
    break;}
case 27:
#line 211 "../ParseLex/term.y"
{ParseResult.info.query.tag = SHOWCOMB;
			       ParseResult.info.query.info.showcomb=yyvsp[0].string;
    break;}
case 28:
#line 213 "../ParseLex/term.y"
{ParseResult.info.query.tag = SHOWCOMB;
			       ParseResult.info.query.info.showcomb="comb";
    break;}
case 29:
#line 215 "../ParseLex/term.y"
{ParseResult.info.query.tag = SHOWCOMB;
			       ParseResult.info.query.info.showcomb="set";
    break;}
case 30:
#line 217 "../ParseLex/term.y"
{ParseResult.info.query.tag = DUMPTABLE;
    break;}
case 31:
#line 218 "../ParseLex/term.y"
{ParseResult.info.query.tag = SHOWMEM;
    break;}
case 32:
#line 219 "../ParseLex/term.y"
{ParseResult.info.query.tag = QUERY;
    break;}
case 33:
#line 221 "../ParseLex/term.y"
{yyval.string = yyvsp[0].string;
    break;}
case 34:
#line 222 "../ParseLex/term.y"
{yyval.string = PROD_TYPE;
    break;}
case 35:
#line 223 "../ParseLex/term.y"
{yyval.string = checkTerminalType(yyvsp[0].string);
    break;}
case 36:
#line 224 "../ParseLex/term.y"
{yyval.string = SUM_TYPE;
    break;}
case 37:
#line 230 "../ParseLex/term.y"
{yyval.datatype= DATADEF(yyvsp[-6].string,yyvsp[-5].strList,yyvsp[-3].string,yyvsp[-2].strList,yyvsp[0].structorList);;
    break;}
case 38:
#line 233 "../ParseLex/term.y"
{ yyval.strList = yyvsp[-1].strList;                                    ;
    break;}
case 39:
#line 234 "../ParseLex/term.y"
{ yyval.strList = NULL;                                  ;
    break;}
case 40:
#line 235 "../ParseLex/term.y"
{ yyval.strList = StrListCons (yyvsp[0].string, NULL, parseHeapDesc); ;
    break;}
case 41:
#line 236 "../ParseLex/term.y"
{ yyval.strList = NULL;                                  ;
    break;}
case 42:
#line 239 "../ParseLex/term.y"
{yyval.structorList = StructorListAppend(yyvsp[-2].structorList, yyvsp[0].structorList);;
    break;}
case 43:
#line 240 "../ParseLex/term.y"
{yyval.structorList = yyvsp[0].structorList;;
    break;}
case 44:
#line 246 "../ParseLex/term.y"
{yyval.structorList =StructorListCons(StructorNew(yyvsp[-2].string,typeSigof(yyvsp[0].structorList)),yyvsp[0].structorList);;
    break;}
case 45:
#line 247 "../ParseLex/term.y"
{ yyval.structorList = StructorListCons (StructorNew (yyvsp[-2].string, yyvsp[0].structor_type_sig), NULL); ;
    break;}
case 46:
#line 250 "../ParseLex/term.y"
{yyval.type_sig = TypeSigNew(yyvsp[-2].type, yyvsp[0].type);;
    break;}
case 47:
#line 255 "../ParseLex/term.y"
{ yyval.structor_type_sig = TypeSigNew2 (yyvsp[-2].type, NULL, yyvsp[0].type); ;
    break;}
case 48:
#line 256 "../ParseLex/term.y"
{ yyval.structor_type_sig = TypeSigNew2 (yyvsp[-4].type, yyvsp[-2].type,   yyvsp[0].type); ;
    break;}
case 49:
#line 260 "../ParseLex/term.y"
{ yyval.type = TypeNew (yyvsp[0].string, NULL); ;
    break;}
case 50:
#line 261 "../ParseLex/term.y"
{ yyval.type = TypeNew (yyvsp[-2].string, NULL); ;
    break;}
case 51:
#line 262 "../ParseLex/term.y"
{ yyval.type = TypeNew (yyvsp[-3].string, yyvsp[-1].typeList);   ;
    break;}
case 52:
#line 263 "../ParseLex/term.y"
{ yyval.type = TypeNew (yyvsp[-1].string, TypeListCons (yyvsp[0].type, NULL)); ;
    break;}
case 53:
#line 264 "../ParseLex/term.y"
{yyval.type = TypeNew(yyvsp[-1].string,
                                  TypeListCons(yyvsp[-2].type, TypeListCons (yyvsp[0].type, NULL))); ;
    break;}
case 54:
#line 266 "../ParseLex/term.y"
{yyval.type = TypeNew(SUM_TYPE, 
                                  TypeListCons(yyvsp[-2].type, TypeListCons (yyvsp[0].type, NULL))); ;
    break;}
case 55:
#line 268 "../ParseLex/term.y"
{ yyval.type = TypeNew (checkTerminalType (yyvsp[0].string), NULL); ;
    break;}
case 56:
#line 269 "../ParseLex/term.y"
{ yyval.type = yyvsp[-1].type; ;
    break;}
case 57:
#line 273 "../ParseLex/term.y"
{ yyval.typeList = TypeListCons (yyvsp[-2].type, yyvsp[0].typeList); ;
    break;}
case 58:
#line 274 "../ParseLex/term.y"
{ yyval.typeList = TypeListCons (yyvsp[-2].type, TypeListCons (yyvsp[0].type, NULL));;
    break;}
case 59:
#line 278 "../ParseLex/term.y"
{yyval.strList = StrListCons(yyvsp[-2].string, yyvsp[0].strList, parseHeapDesc);;
    break;}
case 60:
#line 279 "../ParseLex/term.y"
{yyval.strList = StrListCons(yyvsp[0].string, NULL, parseHeapDesc);;
    break;}
case 61:
#line 287 "../ParseLex/term.y"
{ yyval.alias = BuildAlias (yyvsp[-3].string,
						   StrListReverse (yyvsp[-2].strList),
						   yyvsp[0].type);
				;
    break;}
case 62:
#line 296 "../ParseLex/term.y"
{PopScope(); yyval.function = DEFFUNC(yyvsp[-3].string, yyvsp[-2].macroList, NULL, yyvsp[0].function);;
    break;}
case 63:
#line 297 "../ParseLex/term.y"
{PopScope(); yyval.function = DEFFUNC(yyvsp[-5].string, yyvsp[-4].macroList, yyvsp[-2].type_sig, yyvsp[0].function);;
    break;}
case 64:
#line 300 "../ParseLex/term.y"
{yyval.function =pe_MakeFunBody(yyvsp[0].t_phraseList);;
    break;}
case 65:
#line 302 "../ParseLex/term.y"
{yyval.mcro = MacroNew(yyvsp[-2].string, yyvsp[0].type_sig);;
    break;}
case 66:
#line 303 "../ParseLex/term.y"
{yyval.mcro = MacroNew(yyvsp[0].string, NULL);;
    break;}
case 67:
#line 306 "../ParseLex/term.y"
{yyval.macroList = MacroListCons(yyvsp[-2].mcro, yyvsp[0].macroList);;
    break;}
case 68:
#line 307 "../ParseLex/term.y"
{yyval.macroList = MacroListCons(yyvsp[0].mcro, NULL);;
    break;}
case 69:
#line 310 "../ParseLex/term.y"
{PushScope(); yyval.macroList = pe_Macros(yyvsp[-1].macroList);;
    break;}
case 70:
#line 311 "../ParseLex/term.y"
{PushScope(); yyval.macroList = NULL;;
    break;}
case 71:
#line 312 "../ParseLex/term.y"
{PushScope(); yyval.macroList = NULL;;
    break;}
case 72:
#line 319 "../ParseLex/term.y"
{yyval.patt = pePatt1(yyvsp[0].patt); ;
    break;}
case 73:
#line 323 "../ParseLex/term.y"
{yyval.patt = yyvsp[0].patt;;
    break;}
case 74:
#line 324 "../ParseLex/term.y"
{yyval.patt = pe_MakePattConstr(yyvsp[-1].string, yyvsp[0].patt);;
    break;}
case 75:
#line 325 "../ParseLex/term.y"
{yyval.patt = yyvsp[0].patt;;
    break;}
case 76:
#line 329 "../ParseLex/term.y"
{yyval.patt = Pbang();;
    break;}
case 77:
#line 330 "../ParseLex/term.y"
{yyval.patt = yyvsp[-1].patt;;
    break;}
case 78:
#line 331 "../ParseLex/term.y"
{yyval.patt = Ppair(yyvsp[-3].patt, yyvsp[-1].patt);;
    break;}
case 79:
#line 332 "../ParseLex/term.y"
{yyval.patt = Precord(yyvsp[-1].p_structorArr);;
    break;}
case 80:
#line 337 "../ParseLex/term.y"
{yyval.p_structorArr = pe_MakePattDestr(yyvsp[-2].string, yyvsp[0].patt, NULL);;
    break;}
case 81:
#line 338 "../ParseLex/term.y"
{yyval.p_structorArr = pe_MakePattDestr(yyvsp[-4].string, yyvsp[-2].patt, yyvsp[0].p_structorArr);;
    break;}
case 82:
#line 339 "../ParseLex/term.y"
{yyval.p_structorArr = pe_MakePattDestr(yyvsp[-2].string, peHOvar(yyvsp[-2].string, yyvsp[0].string), NULL);;
    break;}
case 83:
#line 341 "../ParseLex/term.y"
{yyval.p_structorArr = pe_MakePattDestr(yyvsp[-4].string, peHOvar(yyvsp[-4].string, yyvsp[-2].string), yyvsp[0].p_structorArr);;
    break;}
case 84:
#line 345 "../ParseLex/term.y"
{yyval.patt = Pvar(yyvsp[0].string);;
    break;}
case 85:
#line 346 "../ParseLex/term.y"
{yyval.patt = Pdontcare();;
    break;}
case 86:
#line 347 "../ParseLex/term.y"
{yyval.patt = yyvsp[0].patt;;
    break;}
case 87:
#line 348 "../ParseLex/term.y"
{yyval.patt = yyvsp[0].patt;;
    break;}
case 88:
#line 349 "../ParseLex/term.y"
{yyval.patt = yyvsp[0].patt;;
    break;}
case 89:
#line 350 "../ParseLex/term.y"
{yyval.patt = yyvsp[0].patt;;
    break;}
case 90:
#line 359 "../ParseLex/term.y"
{yyval.patt = yyvsp[-1].patt;;
    break;}
case 91:
#line 360 "../ParseLex/term.y"
{yyval.patt = pe_MakePattNilList();;
    break;}
case 92:
#line 364 "../ParseLex/term.y"
{yyval.patt = pe_MakePattList(yyvsp[0].patt, NULL);;
    break;}
case 93:
#line 365 "../ParseLex/term.y"
{yyval.patt = pe_MakePattList(yyvsp[-2].patt, yyvsp[0].patt);;
    break;}
case 94:
#line 375 "../ParseLex/term.y"
{yyval.longInt = peMakeInt(yyvsp[0].string);;
    break;}
case 95:
#line 376 "../ParseLex/term.y"
{yyval.longInt = peMakeInt(yyvsp[0].string);;
    break;}
case 96:
#line 380 "../ParseLex/term.y"
{yyval.patt = pe_MakeIntPatt(INTX, yyvsp[0].longInt, INTX, yyvsp[0].longInt);;
    break;}
case 97:
#line 381 "../ParseLex/term.y"
{yyval.patt = pe_MakeIntPatt(INTX, yyvsp[-2].longInt, INTX, yyvsp[0].longInt);;
    break;}
case 98:
#line 382 "../ParseLex/term.y"
{yyval.patt = pe_MakeIntPatt(INTX, yyvsp[-1].longInt, POSINF, 0);;
    break;}
case 99:
#line 383 "../ParseLex/term.y"
{yyval.patt = pe_MakeIntPatt(NEGINF, 0, INTX, yyvsp[0].longInt);;
    break;}
case 100:
#line 393 "../ParseLex/term.y"
{yyval.string = peMakeChar(yyvsp[0].string,2);;
    break;}
case 101:
#line 394 "../ParseLex/term.y"
{yyval.string = peMakeChar(yyvsp[0].string,8);;
    break;}
case 102:
#line 395 "../ParseLex/term.y"
{yyval.string = peMakeChar(yyvsp[0].string,10);;
    break;}
case 103:
#line 396 "../ParseLex/term.y"
{yyval.string = peMakeChar(yyvsp[0].string,16);;
    break;}
case 104:
#line 397 "../ParseLex/term.y"
{yyval.string = yyvsp[0].string;;
    break;}
case 105:
#line 401 "../ParseLex/term.y"
{yyval.patt = peMakeCharPatt(yyvsp[0].string,yyvsp[0].string);;
    break;}
case 106:
#line 402 "../ParseLex/term.y"
{yyval.patt = peMakeCharPatt(yyvsp[-2].string,yyvsp[0].string);;
    break;}
case 107:
#line 412 "../ParseLex/term.y"
{yyval.strList = pe_StrListCons(yyvsp[0].string, NULL);;
    break;}
case 108:
#line 413 "../ParseLex/term.y"
{yyval.strList = pe_StrListCons(yyvsp[-1].string, yyvsp[0].strList);;
    break;}
case 109:
#line 414 "../ParseLex/term.y"
{yyval.strList = pe_StrListCons(yyvsp[0].string, NULL);;
    break;}
case 110:
#line 415 "../ParseLex/term.y"
{yyval.strList = pe_StrListCons(yyvsp[-1].string,yyvsp[0].strList);;
    break;}
case 111:
#line 419 "../ParseLex/term.y"
{yyval.string = pe_StrListImplode(yyvsp[-1].strList);;
    break;}
case 112:
#line 420 "../ParseLex/term.y"
{yyval.string = pe_StrListImplode(NULL);;
    break;}
case 113:
#line 424 "../ParseLex/term.y"
{yyval.patt = pe_MakeStrPatt(yyvsp[0].string);;
    break;}
case 114:
#line 433 "../ParseLex/term.y"
{yyval.expr = yyvsp[0].expr;;
    break;}
case 115:
#line 435 "../ParseLex/term.y"
{yyval.expr = ExprNew(yyvsp[-1].term, yyvsp[0].expr);;
    break;}
case 116:
#line 436 "../ParseLex/term.y"
{yyval.expr = yyvsp[0].expr;;
    break;}
case 117:
#line 440 "../ParseLex/term.y"
{yyval.expr = ExprPair(yyvsp[-3].expr, yyvsp[-1].expr);;
    break;}
case 118:
#line 441 "../ParseLex/term.y"
{yyval.expr = yyvsp[-1].expr;;
    break;}
case 119:
#line 446 "../ParseLex/term.y"
{yyval.expr=ExprNew(pe_TermRecordNew(yyvsp[-1].recordList),ConstNew());;
    break;}
case 120:
#line 447 "../ParseLex/term.y"
{yyval.expr = ConstNew();;
    break;}
case 121:
#line 451 "../ParseLex/term.y"
{ yyval.expr = yyvsp[-1].expr;;
    break;}
case 122:
#line 452 "../ParseLex/term.y"
{ yyval.expr = ExprNew(TermIdNew("nil",NULL),ConstNew());;
    break;}
case 123:
#line 453 "../ParseLex/term.y"
{ yyval.expr = ExprIdNew(yyvsp[0].string);;
    break;}
case 124:
#line 454 "../ParseLex/term.y"
{ yyval.expr = peMakeStrExpr(yyvsp[0].string);;
    break;}
case 125:
#line 455 "../ParseLex/term.y"
{ yyval.expr = peMakeIntExpr(yyvsp[0].longInt); ;
    break;}
case 126:
#line 456 "../ParseLex/term.y"
{ yyval.expr = peMakeCharExpr(yyvsp[0].string);;
    break;}
case 127:
#line 457 "../ParseLex/term.y"
{  yyval.expr = peMakeGuardExpr(yyvsp[-2].expr,yyvsp[0].t_phraseListList); ;
    break;}
case 128:
#line 461 "../ParseLex/term.y"
{ yyval.t_phraseListList = T_PhraseListListCons(yyvsp[-1].t_phraseList,NULL); ;
    break;}
case 129:
#line 462 "../ParseLex/term.y"
{ yyval.t_phraseListList = T_PhraseListListCons(T_PhraseListCons(yyvsp[0].t_phrase, NULL), NULL); ;
    break;}
case 130:
#line 463 "../ParseLex/term.y"
{ yyval.t_phraseListList=T_PhraseListListCons(yyvsp[-3].t_phraseList,yyvsp[0].t_phraseListList);;
    break;}
case 131:
#line 464 "../ParseLex/term.y"
{ yyval.t_phraseListList = T_PhraseListListCons(T_PhraseListCons(yyvsp[-2].t_phrase,NULL), yyvsp[0].t_phraseListList); ;
    break;}
case 132:
#line 469 "../ParseLex/term.y"
{yyval.expr = ExprNew(TermIdNew("cons", NULL), 
	              ExprPair(yyvsp[0].expr,ExprNew(TermIdNew("nil",NULL),ConstNew())));;
    break;}
case 133:
#line 472 "../ParseLex/term.y"
{yyval.expr = ExprNew(TermIdNew("cons", NULL), ExprPair(yyvsp[-2].expr, yyvsp[0].expr));;
    break;}
case 134:
#line 493 "../ParseLex/term.y"
{ yyval.term = TermProgNew      (yyvsp[-1].t_phraseList); ;
    break;}
case 135:
#line 494 "../ParseLex/term.y"
{ EnterFold   (); ;
    break;}
case 136:
#line 494 "../ParseLex/term.y"
{ ExitFold   (); ;
    break;}
case 137:
#line 494 "../ParseLex/term.y"
{ yyval.term = pe_TermFoldNew   (yyvsp[-2].foldList); ;
    break;}
case 138:
#line 495 "../ParseLex/term.y"
{ EnterUnfold (); ;
    break;}
case 139:
#line 495 "../ParseLex/term.y"
{ ExitUnfold (); ;
    break;}
case 140:
#line 495 "../ParseLex/term.y"
{ yyval.term = pe_TermUnfoldNew (yyvsp[-2].unfoldList); ;
    break;}
case 141:
#line 497 "../ParseLex/term.y"
{yyval.term = TermIdNew(yyvsp[-3].string, yyvsp[-1].macroTerms);;
    break;}
case 142:
#line 498 "../ParseLex/term.y"
{yyval.term = TermIdNew(yyvsp[0].string, NULL);;
    break;}
case 143:
#line 502 "../ParseLex/term.y"
{yyval.macroTerms = FunPhraseListCons(yyvsp[0].funMacroVariant, NULL);;
    break;}
case 144:
#line 503 "../ParseLex/term.y"
{yyval.macroTerms = FunPhraseListCons(yyvsp[-2].funMacroVariant, yyvsp[0].macroTerms);;
    break;}
case 145:
#line 507 "../ParseLex/term.y"
{yyval.funMacroVariant = FunPhraseNew (NULL, NULL);;
    break;}
case 146:
#line 508 "../ParseLex/term.y"
{yyval.funMacroVariant = FunPhraseNew (yyvsp[0].term,   NULL);;
    break;}
case 147:
#line 509 "../ParseLex/term.y"
{yyval.funMacroVariant = FunPhraseNew (yyvsp[-2].term,   yyvsp[0].term);  ;
    break;}
case 148:
#line 513 "../ParseLex/term.y"
{yyval.term = yyvsp[0].term;
    break;}
case 149:
#line 514 "../ParseLex/term.y"
{yyval.term = TermProgNew (yyvsp[0].t_phraseList);;
    break;}
case 150:
#line 523 "../ParseLex/term.y"
{ yyval.t_phraseList = T_PhraseListCons (yyvsp[0].t_phrase, NULL);;
    break;}
case 151:
#line 524 "../ParseLex/term.y"
{ yyval.t_phraseList = T_PhraseListCons (yyvsp[-2].t_phrase, yyvsp[0].t_phraseList);;
    break;}
case 152:
#line 528 "../ParseLex/term.y"
{  PopScope();     yyval.t_phrase = T_PhraseNew(yyvsp[-2].patt, yyvsp[0].expr); ;
    break;}
case 153:
#line 529 "../ParseLex/term.y"
{  PopScope();     yyval.t_phrase = T_PhraseNew(yyvsp[-2].patt, yyvsp[0].expr); ;
    break;}
case 154:
#line 533 "../ParseLex/term.y"
{ yyval.expr = yyvsp[0].expr; ;
    break;}
case 155:
#line 536 "../ParseLex/term.y"
{ yyval.expr = peMakeGuardBool(yyvsp[-2].expr, yyvsp[0].expr, yyvsp[-3].expr); ;
    break;}
case 156:
#line 537 "../ParseLex/term.y"
{ yyval.expr = peMakeGuardBool(yyvsp[0].expr, yyvsp[-2].expr, yyvsp[-3].expr); ;
    break;}
case 157:
#line 541 "../ParseLex/term.y"
{ yyval.expr = yyvsp[0].expr; ;
    break;}
case 158:
#line 554 "../ParseLex/term.y"
{yyval.foldList = FoldListCons(FoldNew(yyvsp[-2].string, yyvsp[0].t_phrase), NULL);;
    break;}
case 159:
#line 555 "../ParseLex/term.y"
{yyval.foldList=FoldListCons(FoldNew(yyvsp[-4].string, yyvsp[-2].t_phrase), 
					 	        FoldListAddId(yyvsp[-4].string,yyvsp[0].foldList));;
    break;}
case 160:
#line 560 "../ParseLex/term.y"
{yyval.foldList = yyvsp[0].foldList;;
    break;}
case 161:
#line 561 "../ParseLex/term.y"
{yyval.foldList = FoldListCons(FoldNew(NULL, yyvsp[0].t_phrase), NULL);;
    break;}
case 162:
#line 562 "../ParseLex/term.y"
{yyval.foldList = FoldListCons(FoldNew(NULL, yyvsp[-2].t_phrase), yyvsp[0].foldList);;
    break;}
case 163:
#line 583 "../ParseLex/term.y"
{  PopScope();
                            yyval.unfoldList = UnfoldListAddPatt (yyvsp[-2].patt, yyvsp[0].unfoldList);;
    break;}
case 164:
#line 589 "../ParseLex/term.y"
{yyval.unfoldList = UnfoldListCons (UnfoldNew (yyvsp[-4].string, T_PhraseNew (NULL, yyvsp[-2].expr)), yyvsp[0].unfoldList);;
    break;}
case 165:
#line 592 "../ParseLex/term.y"
{yyval.unfoldList = UnfoldListCons (UnfoldNew (yyvsp[-4].string, T_PhraseNew (NULL, yyvsp[-2].expr)), yyvsp[0].unfoldList);;
    break;}
case 166:
#line 595 "../ParseLex/term.y"
{yyval.unfoldList = UnfoldListCons (UnfoldNew (yyvsp[-2].string, T_PhraseNew (NULL, yyvsp[0].expr)), NULL);;
    break;}
case 167:
#line 598 "../ParseLex/term.y"
{yyval.unfoldList=UnfoldListCons(
                UnfoldNew(
                    yyvsp[-4].string, HOT_PhraseNew(
                            NULL,TermProgNew(T_PhraseListCons(yyvsp[-2].t_phrase,yyvsp[0].casesAndUnfolds->cases)))),
                yyvsp[0].casesAndUnfolds->unfolds);;
    break;}
case 168:
#line 605 "../ParseLex/term.y"
{yyval.unfoldList = UnfoldListCons(
                  UnfoldNew(
                      yyvsp[-4].string, HOT_PhraseNew(
                              NULL, TermProgNew(T_PhraseListCons (yyvsp[-2].t_phrase, NULL)))),
                  yyvsp[0].unfoldList);;
    break;}
case 169:
#line 612 "../ParseLex/term.y"
{yyval.unfoldList = UnfoldListCons (UnfoldNew (yyvsp[-4].string, HOT_PhraseNew (NULL, TermProgNew (T_PhraseListCons (yyvsp[-2].t_phrase, NULL)))), yyvsp[0].unfoldList);;
    break;}
case 170:
#line 615 "../ParseLex/term.y"
{yyval.unfoldList = UnfoldListCons (UnfoldNew (yyvsp[-2].string, HOT_PhraseNew (NULL, TermProgNew (T_PhraseListCons (yyvsp[0].t_phrase, NULL)))), NULL);;
    break;}
case 171:
#line 619 "../ParseLex/term.y"
{ yyval.expr = yyvsp[0].expr; ;
    break;}
case 172:
#line 620 "../ParseLex/term.y"
{ yyval.expr = yyvsp[0].expr; ;
    break;}
case 173:
#line 623 "../ParseLex/term.y"
{yyval.casesAndUnfolds = AddMoreCases (yyvsp[-2].t_phrase, yyvsp[0].casesAndUnfolds);  ;
    break;}
case 174:
#line 624 "../ParseLex/term.y"
{yyval.casesAndUnfolds = MoreCasesNew (yyvsp[-2].t_phrase, yyvsp[0].unfoldList);  ;
    break;}
case 175:
#line 625 "../ParseLex/term.y"
{yyval.casesAndUnfolds = MoreCasesNew (yyvsp[-2].t_phrase, yyvsp[0].unfoldList);  ;
    break;}
case 176:
#line 626 "../ParseLex/term.y"
{yyval.casesAndUnfolds = MoreCasesNew (yyvsp[0].t_phrase, NULL);;
    break;}
case 177:
#line 647 "../ParseLex/term.y"
{yyval.recordList = RecordListCons (RecordNew (yyvsp[-2].string, yyvsp[0].expr), NULL);;
    break;}
case 178:
#line 650 "../ParseLex/term.y"
{yyval.recordList = RecordListCons (HORecordNew (yyvsp[-2].string, TermProgNew (yyvsp[0].t_phraseList)), NULL);;
    break;}
case 179:
#line 653 "../ParseLex/term.y"
{yyval.recordList = RecordListCons (RecordNew (yyvsp[-4].string, yyvsp[-2].expr), yyvsp[0].recordList);;
    break;}
case 180:
#line 656 "../ParseLex/term.y"
{yyval.recordList = RecordListCons (HORecordNew (yyvsp[-4].string, TermProgNew (yyvsp[-2].t_phraseList)), yyvsp[0].recordList);;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 542 "/usr/local/share/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

 yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

 yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}
#line 664 "../ParseLex/term.y"
