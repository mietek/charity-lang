/******************************************************************************
 *                                                                            *
 *   tc_subst.c                                                               *
 *                                                                            *
 *   COPYRIGHT (c) 1997 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#include "typecheckI.h"

/*
 *  functions devoted to substitution and renaming of type vars
 */

/****************************/
/*                          */
/*  type_vars_to_user_vars  */
/*                          */
/****************************/
TYPE_EXPR *type_vars_to_user_vars(TYPE_EXPR *expr)
{
  TYPE_EXPR **elist;
  int i;  
  if (!expr) return 0;
  switch (expr->tag) {
  case USER_VAR:
    printMsg(FATAL_MSG,"(typecheck) type_vars_to_user_vars: unexpected user var");
  case TYPE_VAR:
    expr->tag = USER_VAR;       /* just replace TYPE_VAR tags by USER_VAR tags */
    return expr;
  case TYPE_CON:
    elist = expr->params;   
    if (!elist) return expr;
    i=0;
    while (elist[i]) {
      elist[i] = type_vars_to_user_vars(elist[i]);
      i++;
    }
    return expr;
  default:
    printMsg(FATAL_MSG,"(typecheck) type_vars_to_user_vars: unknown tag");
  }
}

/************************/
/*                      */
/*  collect_vars_in_TE  */
/*                      */
/************************/
TYPE_ASMT_LIST *collect_vars_in_TE(TYPE_EXPR *expr)
/*
 *  determine the set of all type vars in expr
 *  return a list with items of the form V:=(null), one for each type var V
 */
{
  if (!expr) return 0;
  switch(expr->tag) {
  case USER_VAR:
  case TYPE_VAR:
    return TAL_cons(type_asmt(expr->id.var,0),0);
  case TYPE_CON:
    return collect_vars_in_TEL(expr->params);
  default:
    printMsg(FATAL_MSG,"(typecheck) collect_vars_in_TE: unknown tag");
  }
}

/*************************/
/*                       */
/*  collect_vars_in_TEL  */
/*                       */
/*************************/
TYPE_ASMT_LIST *collect_vars_in_TEL(TYPE_EXPR **list)
{
  TYPE_ASMT_LIST *alist;
  int i;

  if (!list) return 0;
  alist = 0;
  i=0;
  while (list[i]) {
    alist = TAL_var_union(alist,collect_vars_in_TE(list[i]));
    i++;
  }
  return alist;
}

/*******************/
/*                 */
/*  TAL_var_union  */
/*                 */
/*******************/
TYPE_ASMT_LIST *TAL_var_union(TYPE_ASMT_LIST *list1, TYPE_ASMT_LIST *list2)
/*
 *  filter out those entries in list2 not occurring in list1 and append these to list1
 */
{
  TYPE_ASMT_LIST *ptr1, *ptr2, *filtered2;
  TYPE_ASMT *asmt1, *asmt2;

  if (!list1) return list2;
  filtered2 = 0;
  ptr2 = list2;
  while (ptr2) {
    asmt2 = TAL_head(ptr2);
    ptr1 = list1;
    while (ptr1) {
      asmt1 = TAL_head(ptr1);
      if (tc_type_var_match(asmt1->var,asmt2->var)) break;
      else ptr1 = TAL_tail(ptr1);
    }
    if (!ptr1) filtered2 = TAL_append(filtered2,TAL_cons(asmt2,0));
    ptr2 = TAL_tail(ptr2);
  }
  return TAL_append(list1,filtered2);
}

/*********************/
/*                   */
/*  assign_new_vars  */
/*                   */
/*********************/
TYPE_ASMT_LIST *assign_new_vars(TYPE_ASMT_LIST *list)
/*
 *  assuming list entries are of the form V:=(null) (as returned by collect_vars above)
 *  replace each (null) with a brand new type variable
 */
{
  TYPE_ASMT_LIST *ptr;
  TYPE_ASMT *asmt;

  ptr = list;
  while (ptr) {
    asmt = TAL_head(ptr);
    asmt->rhs.var = new_type_var();
    ptr = TAL_tail(ptr);
  }
  return list;
}

/***********************/
/*                     */
/*  rename_vars_in_TE  */
/*                     */
/***********************/
TYPE_EXPR *rename_vars_in_TE(TYPE_ASMT_LIST *alist, TYPE_EXPR *expr)
/*
 *  assuming alist entries are of form V1:=V2, rename each V1 in expr to V2
 */
{
  TYPE_ASMT *asmt;

  if (!expr) return 0;
  switch (expr->tag) {
  case USER_VAR:
  case TYPE_VAR:
    while (alist) {
      asmt = TAL_head(alist);
      if (tc_type_var_match(asmt->var,expr->id.var)) {
	expr->id.var = asmt->rhs.var;
        return expr;
      }
      else alist = TAL_tail(alist);
    }
    return expr;
  case TYPE_CON:
    expr->params = rename_vars_in_TEL(alist,expr->params);
    return expr;
  default:
    printMsg(FATAL_MSG,"(typecheck) rename_vars_in_TE: unknown tag");
  }
}

/************************/
/*                      */
/*  rename_vars_in_TEL  */
/*                      */
/************************/
TYPE_EXPR **rename_vars_in_TEL(TYPE_ASMT_LIST *alist, TYPE_EXPR **elist)
{
  int i;
  if (!elist) return 0;
  i=0;
  while (elist[i]) {
    elist[i] = rename_vars_in_TE(alist,elist[i]);
    i++;
  }
  return elist;
}

/*************/
/*           */
/*  copy_TE  */
/*           */
/*************/
TYPE_EXPR *copy_TE(TYPE_EXPR *expr)
{
  TYPE_EXPR *new_expr;
  int numparams,i;
  if (!expr) return 0;
  else {
    new_expr = (TYPE_EXPR *) MemHeapAlloc(tc_memory,1,sizeof(TYPE_EXPR));
    new_expr->tag = expr->tag;
    switch (expr->tag) {
    case USER_VAR:
    case TYPE_VAR:
      new_expr->id.var = expr->id.var;
      new_expr->params = 0;
      return new_expr;
    case TYPE_CON:
      new_expr->id.con = expr->id.con;
      numparams = TEL_length(expr->params);
      if (numparams == 0) new_expr->params = 0;
      else {
	new_expr->params = (TYPE_EXPR **) MemHeapAlloc(tc_memory,numparams+1,sizeof(TYPE_EXPR *));
	for (i=0; i<numparams; i++) {
	  new_expr->params[i] = copy_TE(expr->params[i]);
	}
	new_expr->params[i] = 0;
      }
      return new_expr;
    default:
      printMsg(FATAL_MSG,"(typechecker) copy_TE: unknown tag");
    }
  }
}

/***************/
/*             */
/*  type_asmt  */
/*             */
/***************/
TYPE_ASMT *type_asmt(TC_TYPE_VARIABLE var, TYPE_EXPR *expr)
{
  TYPE_ASMT *asmt;
  asmt = (TYPE_ASMT *) MemHeapAlloc(tc_memory,1,sizeof(TYPE_ASMT));
  asmt->var = var;
  asmt->rhs.expr = copy_TE(expr);
  return asmt;
}

/*****************/
/*               */
/*  subst_in_TE  */
/*               */
/*****************/
TYPE_EXPR *subst_in_TE(TYPE_ASMT *asmt, TYPE_EXPR *expr)
/*
 *  replace all occurrences of asmt->var in expr by asmt->expr
 */
{
  if (!expr) return 0;
  switch (expr->tag) {
  case USER_VAR:
  case TYPE_VAR:
    if (tc_type_var_match(asmt->var,expr->id.var)) return copy_TE(asmt->rhs.expr);
    else return expr;
  case TYPE_CON:
    expr->params = subst_in_TEL(asmt,expr->params);
    return expr;
  default:
    printMsg(FATAL_MSG,"(typecheck) subst_in_TE: unknown tag");
  }
}

/******************/
/*                */
/*  subst_in_TEL  */
/*                */
/******************/
TYPE_EXPR **subst_in_TEL(TYPE_ASMT *asmt, TYPE_EXPR **list)
/*
 *  apply the substitution to each expr in the list
 */
{
  int i;
  if (!list) return 0;
  i=0;
  while (list[i]) {
    list[i] = subst_in_TE(asmt,list[i]);
    i++;
  }
  return list;
}

/*********************/
/*                   */
/*  subst_all_in_TE  */
/*                   */
/*********************/
TYPE_EXPR *subst_all_in_TE(TYPE_ASMT_LIST *list, TYPE_EXPR *expr)
/*
 *  apply all substitutions in list to expr
 */
{
  while (list) {
    expr = subst_in_TE(TAL_head(list),expr);
    list = TAL_tail(list);
  }
  return expr;
}

/**********************/
/*                    */
/*  subst_all_in_TEL  */
/*                    */
/**********************/
TYPE_EXPR **subst_all_in_TEL(TYPE_ASMT_LIST *alist, TYPE_EXPR **elist)
/*
 *  apply all substitutions in alist to each item in elist
 */
{
  int i;
  if (!elist) return 0;
  i=0;
  while (elist[i]) {
    elist[i] = subst_all_in_TE(alist,elist[i]);
    i++;
  }
  return elist;
}



/* functions for stripping @ type constructors... */

/************************/
/*                      */
/*  eliminate_AT_in_TE  */
/*                      */
/************************/
TYPE_EXPR *eliminate_AT_in_TE(TC_TYPE_CONSTRUCTOR at_con, TYPE_EXPR *expr)
{
  if (!expr) return 0;
  switch (expr->tag) {
  case USER_VAR:
  case TYPE_VAR:
    return expr;
  case TYPE_CON:
    if (tc_type_con_match(at_con, expr->id.con)) {
      return eliminate_AT_in_TE(at_con, expr->params[0]);
    }
    else {
      expr->params = eliminate_AT_in_TEL(at_con, expr->params);
      return expr;
    }
  default:
    printMsg(FATAL_MSG,"(typecheck) eliminate_AT_in_TE: unknown tag");
  }
}

/*************************/
/*                       */
/*  eliminate_AT_in_TEL  */
/*                       */
/*************************/
TYPE_EXPR **eliminate_AT_in_TEL(TC_TYPE_CONSTRUCTOR at_con, TYPE_EXPR **elist)
{
  int i;
  if (!elist) return 0;
  i=0;
  while (elist[i]) {
    elist[i] = eliminate_AT_in_TE(at_con, elist[i]);
    i++;
  }
  return elist;
}

/*************************/
/*                       */
/*  eliminate_AT_in_TAL  */
/*                       */
/*************************/
TYPE_ASMT_LIST *eliminate_AT_in_TAL(TC_TYPE_CONSTRUCTOR at_con, TYPE_ASMT_LIST *alist)
{
  TYPE_ASMT_LIST *ptr;
  TYPE_ASMT *asmt;
  ptr = alist;
  while (ptr) {
    asmt = TAL_head(ptr);
    asmt->rhs.expr = eliminate_AT_in_TE(at_con, asmt->rhs.expr);
    ptr = TAL_tail(ptr);
  }
  return alist;
}
