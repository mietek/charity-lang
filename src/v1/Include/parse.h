/******************************************************************************
 *                                                                            *
 *   parse.h                                                                  *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
/******************************************************************************
**
** FILE: parse.h
**
** Data structures that are loaded from input.
** An internal representation of the term logic.
**
******************************************************************************/

#ifndef __PARSE_H__
#define __PARSE_H__

#define CHARMIN 0
#define CHARMAX 127

#include "list.h"
#include "types.h"

extern int newVarCntr;
extern BBOOL period;
extern BBOOL gbChangeScope;

/********************/
/* Data definitions */
/********************/

/*****************************/
/* Types and type signatures */
/*****************************/

typedef enum {
    INTX,       /* integer x, st.   -infinity < x < +infinity  */
    NEGINF,     /* +infinity */
    POSINF      /* -infinity */
} PE_INT_TAG;

typedef struct _PE_TYPE {       /* eg: 1, *, list(A) */
     char         *ident;
     PE_LIST_TYPE *parms;
} PE_TYPE;

typedef struct _PE_TYPE_SIG {   /* eg: 1 -> C */
     PE_TYPE *domain;
     PE_TYPE *codomain;
} PE_TYPE_SIG;

/* [H-O] EXTENDED TYPE SIGNATURES FOR STRUCTORS: */

typedef struct     /* EG: C -> A => B */
{
  PE_TYPE *domain;
  PE_TYPE *param;        /* NULL IF THERE IS NO PARAMETER, EG: C -> B */
  PE_TYPE *codomain;
}
PE_STRUCTOR_TYPE_SIG;

/*********************************/
/* Type signatures for structors */
/* eg: nil: 1 -> C               */ 
/*********************************/

typedef struct
{
  char                 *ident;
  PE_STRUCTOR_TYPE_SIG *type_sig;     /* [H-O] ALTERED THE TYPE (SEE ABOVE) */
}
PE_STRUCTOR;


/* DATA defintions */
/* DATATYPE domainId domainVars -> codomainId codomainVars =
              structors
	    | structors.                                          */
typedef struct _PE_DATA {
     char             *domainId;
     STR_LIST         *domainVars;
     char             *codomainId;
     STR_LIST         *codomainVars;
     PE_LIST_STRUCTOR *structors;
} PE_DATA;


/******************/
/*  Type Aliases  */
/******************/

typedef struct
{
  char     *name;
  STR_LIST *variables;
  PE_TYPE  *type;
}
PE_ALIAS;


/****************/
/* patterns     */
/****************/

/* structor patterns or records */
/* eg:    cons(0, L)            */
/*      | cons(1, L)            */
typedef struct _P_STRUCTOR {  
     char            *id;
     struct _PE_PATT *arg;
} P_STRUCTOR;

typedef struct {
  P_STRUCTOR **array;
  ST_KEY parentKey;
  int numDestructors;
} P_STRUCTOR_ARRAY;

typedef enum {
     P_VAR,
     P_HOVAR,
     P_RECORD,
     P_PAIR,
     P_CONSTR,
     P_BANG,
     /* builtins */
     P_INT,
     P_CHAR,
     P_STR         /* Yet to be implemented (string = list char) */
} PE_PATT_TAG;

typedef struct _PE_PATT {
     PE_PATT_TAG tag;
     union {
	  char *var;

          struct {
              char *hovar;
              ST_KEY destr;
          } hovar;

	  struct {
	       struct _PE_PATT *l;
	       struct _PE_PATT *r;
	  } ppair;

	  P_STRUCTOR  *constr;

	  P_STRUCTOR **record;      /* NULL terminated */

          struct {
              PE_INT_TAG lTag;
              long       l;
              PE_INT_TAG uTag;
              long       u;
          } intcharBI;

          char *strBI;     /* FUTURE (string = list char) */

     } info;
} PE_PATT;


/*****************/
/* [BI] Builtins */
/*****************/

typedef enum {
    BI_STRING,          /* FUTURE (string = list char) */
    BI_INT,
    BI_CHAR
}   PE_BUILTIN_TAG;

typedef struct {
    PE_BUILTIN_TAG  tag;

    union {
        char           *strBI;        /* UNUSED */
        long            intBI;
        char            charBI;
    } info;

}  PE_BUILTIN;


/*********/
/* Terms */
/*********/

typedef enum {
     T_STRUCTOR,
     T_FUNCTION,
     T_MACRO,
     T_MAP,
     T_FOLD,
     T_CASE,       /* doesn't have to be complete */
     T_COMPLETE_CASE,    /* used in pattern match translation algorithm */
                         /* just like a T_CASE, but it must be complete */
     T_UNFOLD,
     T_RECORD,
     T_BUILTIN
} PE_TERM_TAG;

typedef struct {
     char              *fun_name;
     PE_LIST_T_PHRASE **macros;
} PE_FUNCTION;

typedef struct {
     char              *macro_name;
     PE_LIST_T_PHRASE **macros;
} PE_MACROS;

/*
 * [H-O] ADDED THIS TYPE, AS PHRASES (IE. FOR THE MAP) MAY NOW CONSIST OF BOTH
 *       A POSITIVE AND A NEGATIVE COMPONENT (SEE codetab.h FOR MORE
 *       INFORMATION). IF A COMPONENT IS NOT USED THEN IT IS NULL.
 *
 */

typedef struct _PE_MAP_PHRASE
{
  PE_LIST_T_PHRASE *positive;
  PE_LIST_T_PHRASE *negative;
}
PE_MAP_PHRASE;

typedef struct _PE_MAP
{
  char          *type_name;
  PE_MAP_PHRASE *phrases;       /* [H-O] ALTERED THE TYPE (SEE ABOVE) */
}
PE_MAP;

typedef struct _PE_T_PHRASE
{
  PE_PATT         *patt;
  struct _PE_EXPR *expr;
  struct _PE_TERM *cases;     /* [H-O] EXTENDED TO INCLUDE H-O PHRASES */
                              /*       expr  NULL IF H-O               */
                              /*       cases NULL IF F-O               */
                              /*       (SEE term.y)                    */
}
PE_T_PHRASE;

/*
 * [H-O] ADDED THIS TYPE, AS PHRASES (IE. FOR THE MAP) MAY NOW CONSIST OF BOTH
 *       A POSITIVE AND A NEGATIVE COMPONENT (SEE codetab.h FOR MORE
 *       INFORMATION). IF A COMPONENT IS NOT USED THEN IT IS NULL.
 *
 */

typedef struct _PE_FUN_PHRASE
{
  struct _PE_TERM *positive;
  struct _PE_TERM *negative;
}
PE_FUN_PHRASE;

typedef struct _PE_FOLD {
     char             *constr;
     PE_LIST_T_PHRASE *phrases;	       
} PE_FOLD;

typedef struct _PE_UNFOLD {
     char             *destr;
     PE_LIST_T_PHRASE *phrases;
} PE_UNFOLD;

/* [H-O] ADDED THIS TYPE (SEE term.y): */

typedef struct
{
  PE_LIST_T_PHRASE *cases;
  PE_LIST_UNFOLD   *unfolds;
}
PE_CASES_AND_UNFOLDS;

typedef struct _PE_RECORD
{
  char            *destr;
  struct _PE_EXPR *expr;
  struct _PE_TERM *cases;     /* [H-O] EXTENDED TO INCLUDE H-O PHRASES */
                              /*       expr  NULL IF H-O               */
                              /*       cases NULL IF F-O               */
                              /*       (SEE term.y)                    */
}
PE_RECORD;

typedef struct _PE_TERM {
     PE_TERM_TAG tag;
     union {
	  char               *struct_name;
	  PE_FUNCTION        *function;
	  PE_MACROS          *macro;
	  PE_FOLD           **folds;
	  PE_UNFOLD         **unfolds;
	  PE_RECORD         **records;
	  PE_LIST_T_PHRASE   *cases;
	  PE_MAP             *maps;
          PE_BUILTIN         *builtin;
     } info;

} PE_TERM;


/*****************/
/* expressions   */
/*****************/

typedef enum
{
  E_VAR,
  E_PAIR,
  E_APP,
  E_BANG
}
PE_EXPR_TAG;

typedef struct _PE_EXPR
{
  PE_EXPR_TAG tag;

  union
    {
      char *var;

      struct
	{
	  PE_TERM         *term;
	  struct _PE_EXPR *expr;
	}
      app;

      struct
	{
	  struct _PE_EXPR *l;
	  struct _PE_EXPR *r;
	}
      epair;
    }
  info;

}
PE_EXPR;


/*************************/
/* Functions definitions */
/*************************/

/*******************************/
/* Macros                      */
/* eg:     eq : nat * nat -> c */
/*******************************/
typedef struct _PE_MACRO {     
     char        *ident;
     PE_TYPE_SIG *type_sig;
} PE_MACRO;

/*******************************/
/* Variable bases of functions */
/* eg:     def foo (vb)        */
/*******************************/
typedef enum {
     VB_BANG,
     VB_VAR,
     VB_PAIR
} PE_VAR_BASE_TAG;

typedef struct _PE_VAR_BASE {
     PE_VAR_BASE_TAG tag;
     
     union {
	  char *var;
	  
	  struct {
	       struct _PE_VAR_BASE *l;
	       struct _PE_VAR_BASE *r;
	  } vbpair;
     } info;

} PE_VAR_BASE;

/* Def(ining) functions */
typedef struct _PE_DEF {
     char           *id;
     PE_LIST_MACRO  *macros;
     PE_TYPE_SIG    *type_sig;
     PE_VAR_BASE    *var_base;
     PE_EXPR        *expr;
} PE_DEF;

typedef struct {
  int tag;

  union {
    char *readfile;
  } info;
} PE_COMM;

typedef struct {
  int tag;

  union {

    struct {
      char *entryType;
      char *doReplace;
    } replace;

    STR_LIST *dirList;

    char  *doPrintCT_EXPR;

  } info;
} PE_SETCOMM;

typedef struct {
  int tag;

  union {
    char *query;
    char *showcomb;
  } info;
} PE_QUERY;

/***********************************************************/
typedef struct _PARSE_RESULT {
     int tag;      /* look in y.tab.h for the lookup table */
     
     union {
	  PE_DATA    *data;
	  PE_ALIAS   *alias;
	  PE_DEF     *def;
	  PE_EXPR    *expr;
	  PE_COMM     command;
          PE_SETCOMM  setcommand;
	  PE_QUERY    query;
     } info;

} PARSE_RESULT;


#include "listParse.h"


extern PARSE_RESULT ParseResult;
extern MEMORY       parseHeapDesc;
extern MEMORY       parseHD;
 
/*******************************************************/
/* Function Definitions                                */
/*******************************************************/
extern void          ParserConstruct(void);
extern void          ParserDestruct(void);
extern void          ParserReset(void);
extern void          ParseStream(void);

extern void         *DATADEF(char *domainId, STR_LIST *domainVars, 
			     char *codomainId, STR_LIST *codomainVars, 
			     PE_LIST_STRUCTOR *structor);

extern char         *checkTerminalType(char *termType);
extern PE_TYPE      *TypeNew(char *ident, PE_LIST_TYPE *parms);

extern PE_TYPE_SIG  *TypeSigNew(PE_TYPE *domain, PE_TYPE *codomain);
extern PE_MACRO     *MacroNew(char *ident, PE_TYPE_SIG *type_sig);

/* [H-O] ADDED/ALTERED TO HANDLE TYPE PE_STRUCTOR_TYPE_SIG (SEE ABOVE): */

extern PE_STRUCTOR_TYPE_SIG *TypeSigNew2 (PE_TYPE *domain,
					  PE_TYPE *param,
					  PE_TYPE *codomain);
extern PE_STRUCTOR          *StructorNew (char                 *ident,
					  PE_STRUCTOR_TYPE_SIG *sig);
extern PE_STRUCTOR_TYPE_SIG *typeSigof   (PE_LIST_STRUCTOR *structorList);


/**********************************/
/*     Type Aliases               */
/**********************************/

extern PE_ALIAS *BuildAlias (char     *name,
			     STR_LIST *variables,
			     PE_TYPE  *type);


/**********************************/
/*     Variable Base functions    */
/**********************************/

extern PE_VAR_BASE  *VBvar(char *id);
extern PE_VAR_BASE  *VBbang(void);

/**********************************/
/*     Pattern functions          */
/**********************************/

extern PE_PATT      *pePatt1(PE_PATT *patty);
extern PE_PATT      *Ppair(PE_PATT *l, PE_PATT *r);
extern PE_PATT      *Pconstr(char *id, PE_PATT *structor);
extern PE_PATT      *Pvar(char *id);
extern PE_PATT      *peHOvar(char *destr, char *var);
extern PE_PATT      *Pdontcare(void);
extern PE_PATT      *Precord(P_STRUCTOR_ARRAY *sArray);
extern PE_PATT      *Pbang(void);

extern PE_PATT      *pe_MakePattNilList(void);
extern PE_PATT      *pe_MakePattList(PE_PATT *head, PE_PATT *tail);
extern PE_PATT      *pe_MakeIntPatt(PE_INT_TAG type1, long i1,
                                    PE_INT_TAG type2, long i2);
extern PE_PATT      *pe_MakeStrPatt(char *string);
extern PE_PATT      *peMakeCharPatt(char *cl, char *cu);

extern PE_PATT      *pe_MakePattConstr(char *id, PE_PATT *patt);

extern P_STRUCTOR_ARRAY *pe_MakePattDestr(char *id, PE_PATT *patt, P_STRUCTOR_ARRAY *sArray);

extern PE_PATT      *peMakeVarPatt(MEMORY heap, char *var, BBOOL copyVar);


extern P_STRUCTOR   *P_StructorNew(char *id, PE_PATT *arg);
extern P_STRUCTOR_ARRAY *P_RecordAdd(P_STRUCTOR *structor, P_STRUCTOR_ARRAY *sArray);

extern char         *peMakeChar(char *str, int radix);
extern long          peMakeInt(char *intRep);

extern PE_PATT      *peCopyPatt(MEMORY heap, PE_PATT *orig);
extern P_STRUCTOR   *peCopyPStructor(MEMORY heap, P_STRUCTOR *orig);

/**********************************/
/*   Function definitions         */
/**********************************/
extern PE_DEF       *DEFFUNC(char *name, PE_LIST_MACRO *macros, 
			     PE_TYPE_SIG *type_sig, PE_DEF *defPart);
extern PE_DEF       *pe_MakeFunBody(PE_LIST_T_PHRASE *t_case);


/**********************************
 *                                *
 *      Macro functions           *
 *                                *
 **********************************/
extern PE_LIST_MACRO *pe_Macros(PE_LIST_MACRO *mlist);

/* [H-O] ALTERED THE TYPE OF THE SECOND PARAMETER (SEE parse.c): */
/* [H-O] ADDED THIS PROTOTYPE (SEE parse.c): */
extern PE_FUN_PHRASE *FunPhraseNew (PE_TERM *positive, PE_TERM *negative);


/**********************************/
/*   Folds                        */
/**********************************/

extern PE_TERM      *pe_TermFoldNew(PE_LIST_FOLD *folds);
extern PE_FOLD      *FoldNew(char *constr, PE_T_PHRASE *phrase);
extern PE_LIST_FOLD *FoldListAddId(char *id, PE_LIST_FOLD *folds);

extern void EnterFold   (void);     /* [#@] */
extern void ExitFold    (void);
extern void EnterUnfold (void);
extern void ExitUnfold  (void);

/**********************************/
/*   UnFolds                      */
/**********************************/

extern PE_TERM        *pe_TermUnfoldNew  (PE_LIST_UNFOLD *unfolds);
extern PE_UNFOLD      *UnfoldNew         (char *destr, PE_T_PHRASE *phrase);
extern PE_LIST_UNFOLD *Fold2Unfold       (PE_LIST_FOLD *folds);
extern PE_LIST_UNFOLD *UnfoldListAddPatt (PE_PATT        *patt,
					  PE_LIST_UNFOLD *unfolds);

/* [H-O] ADDED (SEE term.y): */

extern PE_CASES_AND_UNFOLDS *MoreCasesNew (PE_T_PHRASE    *newCase,
					   PE_LIST_UNFOLD *unfolds);
extern PE_CASES_AND_UNFOLDS *AddMoreCases (PE_T_PHRASE          *newCase,
					   PE_CASES_AND_UNFOLDS *original);

/**********************************/
/*   Case                         */
/**********************************/

extern PE_TERM     *TermCaseNew(PE_LIST_T_PHRASE *cases);
extern PE_TERM     *TermProgNew(PE_LIST_T_PHRASE *t_case);

extern PE_T_PHRASE *T_PhraseNew (PE_PATT *patt, PE_EXPR *expr);

/* [H-O] ADDED (SEE term.y): */

extern PE_T_PHRASE *HOT_PhraseNew (PE_PATT *patt, PE_TERM *cases);

/**********************************/
/*   Record                       */
/**********************************/

extern PE_TERM   *pe_TermRecordNew (PE_LIST_RECORD *records);
extern PE_RECORD *RecordNew        (char *destr, PE_EXPR *expr);

/* [H-O] ADDED (SEE term.y): */

extern PE_RECORD *HORecordNew (char *destr, PE_TERM *cases);

/**********************************/
/*   ID                           */
/**********************************/

extern PE_TERM      *TermIdNew(char *id, PE_LIST_FUN_PHRASE *phrases);

extern PE_EXPR      *peMakeIntExpr(long integer);
extern PE_EXPR      *peMakeStrExpr(char *str);
extern PE_EXPR      *peMakeCharExpr(char *charRep);

/**********************************/
/*   Expressions                  */
/**********************************/
extern PE_EXPR      *ExprIdNew(char *id);

extern PE_EXPR      *ExprNew(PE_TERM *term, PE_EXPR *expr);
extern PE_EXPR      *ConstNew(void);
extern PE_EXPR      *ExprVarNew(char *var);

extern PE_EXPR      *ExprPair(PE_EXPR *l, PE_EXPR *r);

/**********************************/
/*   Guards                       */
/**********************************/
extern PE_EXPR    *peMakeGuardBool(PE_EXPR *trueCase, 
                                   PE_EXPR *falseCase,
                                   PE_EXPR *cond);
extern PE_EXPR    *peMakeGuardExpr(PE_EXPR *term, 
                                   PE_LIST_LIST_T_PHRASE *casesList);

/**********************************/
/*   display routines             */
/**********************************/

extern void          displayDatadef(PE_DATA *datadef);

extern void          displayDef(PE_DEF *def);

extern void          printExpr(PE_EXPR *expr);
extern void          printStructor(PE_STRUCTOR *structor);
extern void          printString(char *str);
extern void          showString(char *str);

extern void          printMacro(PE_MACRO *macro);
extern void          printT_Phrase(PE_T_PHRASE *phrase);
extern void          printType(PE_TYPE *type);
extern void          printRecord(PE_RECORD **record);
extern void          printFold(PE_FOLD **fold);
extern void          printMap(PE_MAP *map);
extern void          printUnfold(PE_UNFOLD **unfold);

/* Show routines */
extern void peShowMacro(PE_MACRO *macro);


#endif
