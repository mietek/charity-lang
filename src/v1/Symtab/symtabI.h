/******************************************************************************
 *                                                                            *
 *   symtabI.h                                                                *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __SYMTABI_H__
#define __SYMTABI_H__

#include "parse.h"
#include "symtab.h"
#include "variance.h"     /* [H-O] ADDED THIS INCLUDE */
#include "machine.h"      /* [BI]  ADDED THIS INCLUDE */

#define SYMTAB_SIZE 191            /* prime not too close to a power of 2 */
#define SYMTAB_HEAP_SIZE 25000     /* size of symbol table heap in bytes */

/***************************
 *                         *
 * Symbol Table Structures *
 *                         *
 ***************************/

typedef struct _ST_LOOKUP_ENTRY
{
  char                    *name;
  ST_KEY                   key;
  struct _ST_LOOKUP_ENTRY *next;
}
ST_LOOKUP_ENTRY;

typedef struct _ST_SCOPE
{
  MEMORY            scopeHD;     /* HEAP FOR EACH SCOPE'S DATA */
  struct _ST_SCOPE *below;

  struct _ST_ENTRY *table     [SYMTAB_SIZE];
  ST_LOOKUP_ENTRY  *lookupTab [SYMTAB_SIZE];
}
ST_SCOPE;

extern ST_SCOPE *symTab;     /* THE SYMBOL TABLE---A STACK OF SCOPES */

typedef enum
{
  ST_FUNCTION,     /* 'def' Functions                        */
  ST_MACRO,        /* macro in a function                    */
  ST_STRUCTOR,     /* constructors or destructors            */
  ST_DATATYPE,     /* global user defined data definitions   */
  ST_ALIAS,        /* type alias                             */
  ST_OPCOMB,       /* operation combinators (eg. case, fold) */
  ST_VAR           /* vars encountered during parsing        */
}
ST_TAG;

typedef enum
{
  DT_INDUCTIVE,
  DT_COINDUCTIVE
}
ST_DT_KIND;

typedef struct _ST_ENTRY
{
    ST_TAG               tag;
    char                *name;
    ST_KEY               key;
    struct _ST_ENTRY    *nextEntry;

    union {
	struct {
	     char            **macroNames;     /*to be eliminated; use macroKeys */
	     ST_KEY           *macroKeys;      /* macro keys IN ORDER! */
	     int               numMacros;      /* so we can traverse array */
	     ST_TYPE_SIG      *type_sig;
	     M_INSTR_TAG       instr;          /* [BI] ADDED (SEE miscSymtab.c) */
	} function;

	/* macro name is $$functionName.macroName$$ */
	struct {
	     int posn;
	     struct _ST_ENTRY    *parent_type;
	     ST_TYPE_SIG         *type_sig;
	} macros;

	struct
	  {
	    /* [H-O] ADDED A FIELD TO INDICATE IF H-O DESTRUCTOR: */

	    int               posn;
	    struct _ST_ENTRY *parent;
	    ST_TYPE_SIG      *type_sig;
	    BBOOL             isHO;
	  }
	structor;

	/* [H-O] ADDED A FIELD FOR VARIANCE ARITY (varity) OF DATATYPES: */

	struct
	  {
	    ST_DT_KIND   class;

	    int          numParams;            /*  ARITY                     */
	    V_VARIANCE  *varity;               /* VARITY (OF LEN. numParams) */

	    int          numStructors;         /* STRUCTORS */
	    ST_KEY      *structorKeys;
	    char       **structorNames;        /*
					        * TO BE ELIMINATED;
						* USE structorKeys
						*
						*/

	    ST_KEY      *opCombKeys;           /* OPERATORS */

	    ST_TYPE     *genericStateType;     /* TYPE FOR THE STATE VAR */
	  }
	datatype;

	struct
	  {
	    int         numParams;
	    V_VARIANCE *varity;
	    ST_TYPE    *expansion;
	  }
	alias;

	struct {
	     int                  numParams;
	     struct _ST_ENTRY    *parent;
	     ST_TYPE_SIG         *type_sig;
	} opcomb;

        struct {
            char          *uniqueVar;
	    BBOOL          isHO;
        } var;

    } info;
}
ST_ENTRY;


/***************************
 *                         *
 * Symbol Table Functions  *
 *                         *
 ***************************/

extern ST_KEY    addEntryToSymTab (ST_ENTRY *entry);
/* changed so that it returns the level the name was found at. The most   *
 * recent level is 0 and it increments from there. If name not found then *
 * level = -1. level can be NULL if no info required.                     */
extern ST_ENTRY *getEntry         (char     *name);
extern ST_ENTRY *getNextEntry     (ST_ENTRY *entry);
extern ST_ENTRY *getFirstEntry    (void);

extern void loadBaseTypes (void);

extern ST_ENTRY *st_GetEntry       (ST_KEY key, int *level);
extern void      st_RemoveAnyEntry (ST_KEY key);

extern BBOOL st_IsDatatype   (ST_KEY key);
extern BBOOL st_IsFunction   (ST_KEY key);
extern BBOOL st_IsCombinator (ST_KEY key);

ST_TYPE  *st_MakeGenericStateType (ST_ENTRY *dTypeEntry);
void      st_AddOpCombs           (ST_KEY    typeKey);

extern int st_GetNumDistinctParams (ST_TYPE_SIG *sig);

extern ST_KEY st_MakeKey (void);


extern ST_KEY PROD_KEY;
extern ST_KEY TERM_KEY;

extern ST_KEY CHAR_KEY;     /* [BI] ADDED */
extern ST_KEY INT_KEY;

#endif
