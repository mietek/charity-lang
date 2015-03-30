/******************************************************************************
 *                                                                            *
 *   codetab.h                                                                *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __CODETAB_H__
#define __CODETAB_H__

#include "types.h"
#include "compile.h"
#include "machine.h"
#include "symtab.h"

#define CODETAB_HEAP_SIZE 375   /* # of records in code table heap */

#define PRIM_BANG  "!"
#define PRIM_PAIR  "pair"

extern int     kludge;

typedef enum
{
  CTT_COMPOSITION,
  CTT_COMBINATOR,
  CTT_CLOSURE
}
COMB_EXPR_TAG;

typedef enum
{
  CC_PRIMITIVE,
  CC_CONSTRUCTOR,
  CC_DESTRUCTOR,
  CC_FOLD,
  CC_CASE,
  CC_MAP_I,
  CC_UNFOLD,
  CC_RECORD,
  CC_MAP_C,
  CC_FUNCTION,
  CC_MACRO,
  CC_BUILTIN_INT,      /* [BI] BUILTINS */
  CC_BUILTIN_CHAR,
  CC_CATA,             /* [#@] */
  CC_ANA,              /* [#@] */
  CC_SELF,             /* [#@] */
  CC_AT                /* [#@] */
}
COMB_CLASS;

/*
 * [H-O] ADDED THIS TYPE, AS COMBINATOR PHRASES (IE. FOR THE MAP) MAY NOW
 *       CONSIST OF BOTH A POSITIVE AND A NEGATIVE COMPONENT. THERE ARE FOUR
 *       POSSIBILITES FOR THE map^R: R(A) -> R(B) COMBINATOR:
 *
 *       map^R{_}         IF VARIANCE OF A IS ?
 *                        (BOTH FIELDS ARE NULL)
 *
 *       map^R{p}         WHERE p: A -> B, IF VARIANCE OF A IS +
 *                        (negative FIELD IS NULL)
 *
 *       map^R{n}         WHERE n: B -> A, IF VARIANCE OF A IS -
 *                        (positive FIELD IS NULL)
 *
 *       map^R{p & n}     WHERE p: A -> B & n: B -> A, IF VARIANCE OF A IS *
 *                        (NEITHER FIELD IS NULL)
 *
 *       FOR OTHER COMBINATORS, THE positive FIELD IS USED EXCLUSIVELY
 *       (AND negative FIELD IS NULL).
 *
 */

typedef struct
{
  struct _COMB_EXPR *positive;
  struct _COMB_EXPR *negative;
}
COMB_PHR;

typedef struct _COMB_EXPR
{
  COMB_EXPR_TAG tag;

  union
    {
      struct
    {
      struct _COMB_EXPR *l;
      struct _COMB_EXPR *r;
    }
      composition;

      struct
    {
          COMB_CLASS   class;
      char        *name;
      char        *parentName;
      int          parameter;      /* only used for function and macro */
      int          numParams;
      COMB_PHR   **param;          /* [H-O] ALTERED TYPE (SEE ABOVE)   */

      int  i;                      /* [BI] FOR BUILTIN INTEGERS   ONLY */
      char c;                      /*      FOR BUILTIN CHARACTERS ONLY */

      int self;                    /* [#@]                             */
    }
      combinator;

      CLO_TAB_ENTRY closure;
    }
  info;
}
COMB_EXPR;

typedef struct
{
  /* number of reductions */
  long   machine_ops;

  /* memory used */
  long   heap_size;
  long   cells_used;

} MACHINE_STATS;

/**************************/
/*                        */
/* Code Table Entry Type  */
/*                        */
/**************************/

typedef struct _CT_ENTRY {
     char             *name;
     COMB_EXPR        *comb_exp;
     M_INSTR          *machine_code;

     struct _CT_ENTRY *next;
} CT_ENTRY;

/**************************/
/*                        */
/* Code Table Functions   */
/*                        */
/**************************/

extern void                CodeTableConstruct(int size);
extern void                CodeTableDestruct(void);
extern void                CodeTableReset(void);

extern void                CodeTableAdd(char *name, COMB_EXPR *expr, M_INSTR *mc);
extern COMB_EXPR          *CodeTableGetComb(char *name);
extern M_INSTR            *CodeTableGetMC(char *name);

extern void                CodeTableShowComb(char *funName);
extern void                CodeTableShowMC(char *funName);


/***********************************
 *                                 *
 *   Combinator Printing Functions *
 *                                 *
 ***********************************/
extern void         combExprPrint(COMB_EXPR *combExpr,
                  int        maxShowDepth,
                  int        maxRecordDepth,
                  ST_TYPE   *type);


#endif
