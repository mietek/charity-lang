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


extern YYSTYPE yylval;
