/******************************************************************************
 *                                                                            *
 *   typecheckI.h                                                             *
 *                                                                            *
 *   COPYRIGHT (c) 1997 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#ifndef __TYPECHECKI_H__
#define __TYPECHECKI_H__


#include "typecheck.h"


/***************************************/
/*  data structures for typechecking:  */
/***************************************/

typedef ST_KEY TC_TYPE_CONSTRUCTOR;
typedef ST_PVAR TC_TYPE_VARIABLE;
#define tc_type_con_match(x,y) st_KeysEqual(x,y)
#define tc_type_var_match(x,y) st_ParamsEqual(x,y)

typedef enum {
  USER_VAR,             /* <-- for type vars specified by the user in defs */
  TYPE_VAR,
  TYPE_CON
} TYPE_TAG;

typedef struct _TYPE_EXPR {
  TYPE_TAG tag;
  union {
    TC_TYPE_VARIABLE var;
    TC_TYPE_CONSTRUCTOR con;
  } id;
  struct _TYPE_EXPR **params;         /* <-- used only if tag is TYPE_CON */
} TYPE_EXPR;

typedef struct {
  TC_TYPE_VARIABLE var;
  union {
    TYPE_EXPR *expr;
    TC_TYPE_VARIABLE var;             /* <-- only for renaming variables */
  } rhs;
} TYPE_ASMT;

typedef struct {
  PE_PATT *patt;
  TYPE_EXPR **type;		/* FOpatt : type[0] , HOpatt : type[0]->type[1] */
} TYPED_PATT;

typedef LIST TYPE_ASMT_LIST;
typedef LIST TYPED_PATT_LIST;



/************************************/
/*  global variables:  (tc_main.c)  */
/************************************/
extern MEMORY tc_memory;			/* local memory */

extern TC_TYPE_VARIABLE next_new_var;		/* for renaming type vars */
#define new_type_var() next_new_var++

extern TYPE_ASMT_LIST *SubstList;


/**********************************/
/*  list functions:  (tc_list.c)  */
/**********************************/
extern int TEL_length(TYPE_EXPR **list);

extern TYPE_ASMT *TAL_head(TYPE_ASMT_LIST *list);
extern TYPE_ASMT_LIST *TAL_tail(TYPE_ASMT_LIST *list);
extern TYPE_ASMT_LIST *TAL_append(TYPE_ASMT_LIST *list1, TYPE_ASMT_LIST *list2);
extern TYPE_ASMT_LIST *TAL_cons(TYPE_ASMT *asmt, TYPE_ASMT_LIST *list);

extern TYPED_PATT *TPL_head(TYPED_PATT_LIST *list);
extern TYPED_PATT_LIST *TPL_tail(TYPED_PATT_LIST *list);
extern TYPED_PATT_LIST *TPL_cons(TYPED_PATT *tp, TYPED_PATT_LIST *list);

/*******************************************/
/*  substitution functions:  (tc_subst.c)  */
/*******************************************/
extern TYPE_EXPR *type_vars_to_user_vars(TYPE_EXPR *expr);

extern TYPE_ASMT_LIST *collect_vars_in_TE(TYPE_EXPR *expr);
extern TYPE_ASMT_LIST *collect_vars_in_TEL(TYPE_EXPR **list);
extern TYPE_ASMT_LIST *TAL_var_union(TYPE_ASMT_LIST *list1, TYPE_ASMT_LIST *list2);
extern TYPE_ASMT_LIST *assign_new_vars(TYPE_ASMT_LIST *list);

extern TYPE_EXPR *rename_vars_in_TE(TYPE_ASMT_LIST *alist, TYPE_EXPR *expr);
extern TYPE_EXPR **rename_vars_in_TEL(TYPE_ASMT_LIST *alist, TYPE_EXPR **elist);

extern TYPE_EXPR *copy_TE(TYPE_EXPR *expr);

extern TYPE_ASMT *type_asmt(TC_TYPE_VARIABLE var, TYPE_EXPR *expr);

extern TYPE_EXPR *subst_in_TE(TYPE_ASMT *asmt, TYPE_EXPR *expr);
extern TYPE_EXPR **subst_in_TEL(TYPE_ASMT *asmt, TYPE_EXPR **list);
extern TYPE_EXPR *subst_all_in_TE(TYPE_ASMT_LIST *list, TYPE_EXPR *expr);
extern TYPE_EXPR **subst_all_in_TEL(TYPE_ASMT_LIST *alist, TYPE_EXPR **elist);

extern TYPE_EXPR *eliminate_AT_in_TE(TC_TYPE_CONSTRUCTOR con, TYPE_EXPR *expr);
extern TYPE_EXPR **eliminate_AT_in_TEL(TC_TYPE_CONSTRUCTOR con, TYPE_EXPR **elist);
extern TYPE_ASMT_LIST *eliminate_AT_in_TAL(TC_TYPE_CONSTRUCTOR con, TYPE_ASMT_LIST *alist);

/******************************************/
/*  unification functions:  (tc_unify.c)  */
/******************************************/
extern int type_con_occurs_in_TE(TC_TYPE_CONSTRUCTOR con, TYPE_EXPR *expr);
extern void add_equation_to_SubstList(TYPE_EXPR *e1, TYPE_EXPR *e2);

/*******************************************/
/*  conversion functions:  (tc_convert.c)  */
/*******************************************/
extern TYPE_EXPR  *convert_ST_TYPE_to_TE(ST_TYPE *st_type);
extern ST_TYPE    *convert_TE_to_ST_TYPE(TYPE_EXPR *expr);


#endif
