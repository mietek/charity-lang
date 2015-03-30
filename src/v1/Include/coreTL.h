/******************************************************************************
 *                                                                            *
 *   coreTL.h                                                                 *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#ifndef __CORE_TL_H__
#define __CORE_TL_H__

#include "list.h"

#define CTHEAP_SIZE 50000  /* size of heap for coreterm logic expression */


/********************/
/*                  */
/* Core Term Logic  */
/*                  */
/********************/

/*******************************/
/* Variable bases of functions */
/* eg:     def foo (vb)        */
/*******************************/
typedef enum {
     CT_VB_BANG,
     CT_VB_VAR,
     CT_VB_PAIR
} CT_VAR_BASE_TAG;

typedef struct _CT_VAR_BASE {
     CT_VAR_BASE_TAG tag;

     union {
      char *var;

      struct {
           struct _CT_VAR_BASE *l;
           struct _CT_VAR_BASE *r;
      } pair;
     } info;

} CT_VAR_BASE;


/*****************/
/* expressions   */
/*****************/

typedef enum
{
  CT_VAR,
  CT_PAIR,
  CT_APP,
  CT_BANG,
  CT_INCOMPLETE    /* used only by pattern match translation */
}
CT_EXPR_TAG;

typedef struct _CT_EXPR
{
  CT_EXPR_TAG tag;

  union
    {
      char *var;

      struct
    {
      struct _CT_TERM *term;
      struct _CT_EXPR *expr;
    }
      app;

      struct
    {
      struct _CT_EXPR *l;
      struct _CT_EXPR *r;
    }
      pair;
    }
  info;
}
CT_EXPR;


/*********/
/* Terms */
/*********/

typedef enum
{
  CT_T_STRUCTOR,
  CT_T_FUNCTION,
  CT_T_MACRO,
  CT_T_MAP,
  CT_T_FOLD,
  CT_T_UNFOLD,
  CT_T_CASE,
  CT_T_RECORD,
  CT_T_ABS,
  CT_T_BUILTIN,      /* [BI] BUILTINS (SEE BELOW) */
  CT_T_CATA,         /* [#@]                      */
  CT_T_ANA,          /* [#@]                      */
  CT_T_SELF          /* [#@]                      */
}
CT_TERM_TAG;

typedef struct {
     CT_VAR_BASE *var_base;
     CT_EXPR     *expr;
   } CT_PHRASE;

/*
 * [H-O] ADDED THIS TYPE, AS PHRASES (IE. FOR THE MAP) MAY NOW CONSIST OF BOTH
 *       A POSITIVE AND A NEGATIVE COMPONENT (SEE codetab.h FOR MORE
 *       INFORMATION). IF A COMPONENT IS NOT USED THEN ITS CONSTITUENT FIELDS
 *       ARE NULL.
 *
 */

typedef struct
{
  CT_VAR_BASE *var_base;
  CT_EXPR     *expr;

  CT_VAR_BASE *neg_var_base;
  CT_EXPR     *neg_expr;
}
CT_MAP_PHRASE;

typedef struct {
   char         *macro_name;
   CT_PHRASE   **macros;
} CT_MACROS;

typedef struct {
   char         *fun_name;
   CT_PHRASE   **macros;
} CT_FUNCTION;

typedef struct
{
  char           *type_name;
  CT_MAP_PHRASE **phrases;       /* [H-O] ALTERED THE TYPE (SEE ABOVE) */
}
CT_MAP;

typedef struct {
   char        *constr;
   CT_VAR_BASE *var_base;
   CT_EXPR     *expr;
} CT_FOLD;

typedef struct
{
  char        *destr;
  CT_VAR_BASE *var_base;
  CT_VAR_BASE *var_base2;     /* [H-O] ADDED THIS FIELD FOR A H-O PARAMETER */
                              /*       (NULL IF UNUSED)                     */
  CT_EXPR     *expr;
}
CT_UNFOLD;

typedef struct {
  char        *constr;
  CT_VAR_BASE *var_base;
  CT_EXPR     *expr;
} CT_CASE;

typedef struct
{
  char        *destr;
  CT_VAR_BASE *var_base;     /* [H-O] ADDED THIS FIELD FOR A H-O PARAMETER */
                             /*       (NULL IF UNUSED)                     */
  CT_EXPR     *expr;
}
CT_RECORD;

typedef struct {
   CT_VAR_BASE *var_base;
   CT_EXPR     *expr;
} CT_ABS;

/* [BI] BUILTINS (int, char---string = list char): */

typedef enum
{
  CT_INT,
  CT_CHAR
}
CT_BUILTIN_TAG;

typedef struct
{
  CT_BUILTIN_TAG tag;

  union
    {
      int  i;
      char c;
    }
  info;
}
CT_BUILTIN;

typedef struct     /* [#@] */
{
  int       self;
  CT_EXPR  *context;
  CT_FOLD **folds;
}
CT_CATA;

typedef struct     /* [#@] */
{
  int         self;
  CT_EXPR    *context;
  CT_UNFOLD **unfolds;
}
CT_ANA;

typedef struct _CT_TERM
{
  CT_TERM_TAG tag;

  union
    {
      char         *struct_name;
      CT_MACROS    *macro;
      CT_FUNCTION  *function;
      CT_MAP       *maps;
      CT_FOLD     **folds;
      CT_UNFOLD   **unfolds;
      CT_CASE     **cases;
      CT_RECORD   **records;
      CT_ABS       *abs;
      CT_BUILTIN   *builtin;         /* [BI] BUILTINS (SEE ABOVE) */

      CT_CATA      *cata;            /* [#@] */
      CT_ANA       *ana;
      int           self;
    }
  info;
}
CT_TERM;

#endif
