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
#define	HO_ID	258
#define	FO_ID	259
#define	MAPP	260
#define	PROD	261
#define	SUM	262
#define	STRING	263
#define	INTNEG	264
#define	INTPOS	265
#define	IMP	266
#define	STRNG	267
#define	SYMTABENTRY	268
#define	BOOL	269
#define	DIR	270
#define	BINCHARREP	271
#define	OCTCHARREP	272
#define	DECCHARREP	273
#define	HEXCHARREP	274
#define	IDCHARREP	275
#define	CHAR	276
#define	LPAR	277
#define	RPAR	278
#define	COMMA	279
#define	LBRA	280
#define	RBRA	281
#define	COLON	282
#define	SEMI	283
#define	EQUALS	284
#define	LBRACE	285
#define	RBRACE	286
#define	OR	287
#define	QUOTE	288
#define	OROR	289
#define	NOT	290
#define	UNDERSCORE	291
#define	DATA	292
#define	RANGE	293
#define	DEF	294
#define	SET	295
#define	READFILE	296
#define	LUNFOLD	297
#define	RUNFOLD	298
#define	LFOLD	299
#define	RFOLD	300
#define	ILLEGALCHAR	301
#define	QUIT	302
#define	EXPR	303
#define	EMPTY_INPUT	304
#define	SHOWMEM	305
#define	COMMAND	306
#define	SETCOMMAND	307
#define	QUERY	308
#define	ABOUT	309
#define	SHOWCOMB	310
#define	REPLACE	311
#define	PRINTCTEXPR	312
#define	INCLUDEDIRS	313
#define	DUMPTABLE	314
#define	APPENDDIRS	315
#define	AMPER	316
#define	ALIAS	317
#define	CONSTR	318


extern YYSTYPE yylval;
