/******************************************************************************
 *                                                                            *
 *   symtab.h                                                                 *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __SYMTAB_H__
#define __SYMTAB_H__

#include <stdio.h>
#include <assert.h>

#include "parse.h"
#include "pmem.h"
#include "types.h"
#include "variance.h"     /* [H-O] ADDED THIS INCLUDE */
#include "machine.h"      /* [BI]  ADDED THIS INCLUDE */


/* [BI] BUILT IN TYPES: */

#define BOOL_TYPENAME   "bool"
#define INT_TYPENAME    "int"
#define CHAR_TYPENAME   "char"
#define LIST_TYPENAME   "list"

/* #define STRING_TYPENAME "string" */

#define FALSE_CONSTRUCTORNAME "false"
#define TRUE_CONSTRUCTORNAME  "true"

#define NIL_CONSTRUCTORNAME  "nil"
#define CONS_CONSTRUCTORNAME "cons"

#define INT_CONSTRUCTOR  RES_PREFIX "INT"
#define CHAR_CONSTRUCTOR RES_PREFIX "CHAR"

/* #define STRING_CONSTRUCTOR RES_PREFIX "STRING" */

/* [BI] BUILT IN FUNCTIONS: */

#define ADD_INT   "add_int"
#define SUB_INT   "sub_int"
#define MUL_INT   "mul_int"
#define DIV_INT   "div_int"
#define MOD_INT   "mod_int"

#define LT_INT    "lt_int"
#define LE_INT    "le_int"
#define GT_INT    "gt_int"
#define GE_INT    "ge_int"
#define EQ_INT    "eq_int"

#define LT_CHAR   "lt_char"
#define LE_CHAR   "le_char"
#define GT_CHAR   "gt_char"
#define GE_CHAR   "ge_char"
#define EQ_CHAR   "eq_char"

#define CODE      "code"
#define DECODE    "decode"

/* #define EQ_STRING "eq_string" */


/**********************
 *                    *
 * SYMBOL TABLE TYPES *
 *                    *
 **********************/

/*
 *  THE FOLLOWING ARE FOR CHARITY TYPES:
 *
 */

typedef int ST_PVAR;     /* PARAMETRIC TYPE VARIABLE IDENTIFIERS */

typedef enum               /* THE KINDS OF CHARITY TYPES: */
{
  TYPE_1,                  /* NULLARY PRODUCTS (THE UNIT) */
  TYPE_PROD,               /* BINARY  PRODUCTS            */
  TYPE_PARAMETRIC_VAR,     /* PARAMETRIC TYPE VARIABLES   */
  TYPE_STATE_VAR,          /* STATE      TYPE VARIABLES   */
  TYPE_USER_DATA,          /* USER-DEFINED DATATYPES      */
  TYPE_BUILTIN_INT,        /* [BI] BUILTIN INTEGERS       */
  TYPE_BUILTIN_CHAR        /* [BI] BUILTIN CHARACTERS     */
}
ST_TYPE_TAG;

typedef struct _ST_TYPE     /* THE CHARITY TYPES THEMSELVES */
{
  ST_TYPE_TAG tag;

  union
    {
      struct
	{
	  ST_KEY           key;
	  struct _ST_TYPE *l;
	  struct _ST_TYPE *r;
	}
      prod;

      int     parametric_var;     /* OBSOLETE...         */
      ST_PVAR param_var;          /* ...USE THIS INSTEAD */

      struct
	{
	  char             *name;     /* OBSOLETE...         */
	  ST_KEY            key;      /* ...USE THIS INSTEAD */
	  struct _ST_TYPE **args;
	}
      user_data;
    }
  info;
}
ST_TYPE;

/*
 * [H-O] EXTENDED THIS TYPE, AS TYPE SIGNATURES (IE. FOR THE MAP) MAY NOW
 *       CONSIST OF BOTH A POSITIVE AND A NEGATIVE COMPONENT IN THE params.
 *       THERE ARE FOUR POSSIBILITES FOR THE map^R: R(A) -> R(B) COMBINATOR:
 *
 *       map^R{_}         IF VARIANCE OF A IS ?
 *                        (domain/codomain/negDomain/negCodomain FIELDS ARE
 *                        NULL)
 *
 *       map^R{p}         WHERE p: A -> B, IF VARIANCE OF A IS +
 *                        (negDomain/negCodomain FIELDS ARE NULL)
 *
 *       map^R{n}         WHERE n: B -> A, IF VARIANCE OF A IS -
 *                        (domain/codomain FIELDS ARE NULL)
 *
 *       map^R{p & n}     WHERE p: A -> B & n: B -> A, IF VARIANCE OF A IS *
 *                        (NO FIELDS ARE NULL)
 *
 *       FOR OTHER SIGNATURES, THE domain/codomain FIELDS ARE USED EXCLUSIVELY
 *       (AND THE negDomain/negCodomain FIELDS SHOULD BE IGNORED).
 *
 */

typedef struct _ST_TYPE_SIG     /* THE CHARITY TYPE SIGNATURES */
{
  ST_TYPE              *domain;
  ST_TYPE              *codomain;

  ST_TYPE              *negDomain;         /* [H-O] (ADDED THESE FIELDS)   */
  ST_TYPE              *negCodomain;

  struct _ST_TYPE_SIG **params;            /* NULL TERMINATED              */

  BBOOL			userspecified;     /* FOR MACROS AND FUNCTIONS     */
                                           /* TYPECHECKER NEEDS TO KNOW IF */
                                           /* TYPE WAS SPECIFIED BY USER   */
}
ST_TYPE_SIG;


/**************************
 *                        *
 * SYMBOL TABLE FUNCTIONS *
 *                        *
 **************************/

/* SYMBOL TABLE CONSTRUCTION AND DESTRUCTION: */

extern void initSymTable        (BBOOL loadBase);
extern void st_DestructSymTable (void);

/* SCOPE PUSHING AND POPPING: */

extern void PushScope (void);
extern void PopScope  (void);
extern void stPopToTop(void);

/* LOOKUP: */

extern ST_KEY  st_NameToKey(char *name);    /* returns NULL on fail */
extern char   *st_KeyToName(ST_KEY key);    /* returns NULL on fail */

extern char          *getStructorParent(char *structorName);
extern ST_KEY         st_GetStructorParent(ST_KEY structKey);
extern char         **getStructorNames(char *typeName);
extern char         **st_GetStructorNames(ST_KEY typeKey);
extern ST_KEY        *st_GetStructorKeys(ST_KEY typeKey);
extern int            getNumStructors(char *typeName);
extern int            st_GetNumStructors(ST_KEY typeKey);
extern int            getStructorPosn(char *structorName);
extern int            st_GetStructorPosn(ST_KEY structKey);

extern int            getTypeParams(char *typeName);
extern int            getNumParams(char *typeName);
extern int            st_GetNumParams(ST_KEY typeKey);
extern BBOOL          isInductiveType(char *typeName);
extern BBOOL          st_IsInductiveType(ST_KEY typeKey);
extern BBOOL          isCoinductiveType(char *typeName);
extern BBOOL          st_IsCoinductiveType(ST_KEY typeKey);

extern int         getAliasNumParams (char *alias);
extern V_VARIANCE *getAliasVarity    (char *alias);
extern ST_TYPE    *getAliasExpansion (char *alias);

extern M_INSTR_TAG st_GetInstruction (ST_KEY functionKey);

/* [H-O] ADDED THESE FUNCTIONS (SEE lookupSymtab.c): */

extern V_VARIANCE *st_GetVarity (ST_KEY typeKey);
extern BBOOL       st_IsHO      (ST_KEY destKey);

extern BBOOL       st_IsVar(ST_KEY key, int *level);
extern BBOOL       st_IsHOVar(ST_KEY key, int *level);
extern char       *st_GetUniqueVar(ST_KEY varKey);

extern int     st_GetParamPosn(ST_PVAR param);
extern int     st_GetNumParams(ST_KEY typeKey);

extern BBOOL          isConstructor(char *structorName);
extern BBOOL          st_IsConstructor(ST_KEY structKey);
extern BBOOL          isDestructor(char *structorName);
extern BBOOL          st_IsDestructor(ST_KEY structKey);

extern BBOOL          st_IsMacro(ST_KEY key);
extern BBOOL          st_IsMacroByName(char *macroName);
extern char         **getMacroNames(char *funName);  /* in positional order */
extern ST_KEY        *st_GetMacroKeys(ST_KEY funKey); /* in positnal order */
extern int            getNumMacros(char *funName);
extern int            st_GetNumMacros(ST_KEY funKey);
extern int            st_GetMacroPosn(ST_KEY macroKey);

extern ST_TYPE_SIG   *st_GetTypeSig(ST_KEY key);
extern ST_TYPE_SIG  **st_GetMacroTypeSigs(ST_KEY funKey);
extern ST_TYPE_SIG   *getStructorTypeSig(char *structorName);
extern ST_TYPE_SIG   *getFunTypeSig(char *funName);
extern ST_TYPE       *getStructorType(char *structorName);

extern BBOOL          st_IsStructor(ST_KEY key);
extern ST_TYPE       *st_GetGenericStateType(ST_KEY key);  /* for a structor */
extern ST_TYPE_TAG    st_GetDatatypeTag(ST_KEY typeKey);
extern char         *st_GetOpCombParentByName(char *combName);

extern BBOOL          isMacro(char *funName, char *name);
extern BBOOL          isFunction(char *name);
extern BBOOL          isDatatype(char *name);
extern BBOOL          isAlias(char *name);
extern BBOOL          isStructor(char *name);

/* INSERTION AND DELETION: */

extern ST_KEY stAddVar(char *var, BBOOL isHO);
extern ST_KEY stAddMacro(char *name);
extern void   addDatatype    (PE_DATA  *dataDefn);
extern void   addAlias       (PE_ALIAS *alias);
extern ST_KEY st_AddFunction (PE_DEF   *def);
extern void   st_RemoveEntry (ST_KEY    key);

/* DISPLAY: */

extern void st_PrintEntryInfo (ST_KEY key);

extern void st_DumpTable (void);

extern void st_ShowType               (ST_TYPE     *type);

extern void st_ShowTypeSig            (ST_TYPE_SIG *typeSig);
extern void st_ShowSig                (ST_TYPE_SIG *typeSig);  /* NO PARAMS  */
extern void st_ShowTypeSigLessContext (ST_TYPE_SIG *typeSig);  /* NO CONTEXT */

extern void st_ShowDatatypeCombinators (ST_KEY dataKey);

/* MISC: */

extern void st_UpdateTypeSig               (ST_KEY       key,
					    ST_TYPE_SIG *newsig);
extern void st_LinkMacroTypeSigsToFunction (ST_KEY       fnkey);

extern ST_TYPE *st_SubstType (MEMORY    heap,       /* MAP OVER THE TYPE */
			      ST_TYPE  *type,
			      ST_TYPE  *state,
			      ST_TYPE **parms);

extern BBOOL st_KeysEqual   (ST_KEY  k1,
			     ST_KEY  k2);
extern BBOOL st_ParamsEqual (ST_PVAR p1,
			     ST_PVAR p2);


/*********************************
 *                               *
 * SYMBOL TABLE GLOBAL VARIABLES *
 *                               *
 *********************************/

extern BBOOL gb_ReplaceFunctions;     /* [FIX] REMOVE GLOBAL VARIABLES */

#endif
