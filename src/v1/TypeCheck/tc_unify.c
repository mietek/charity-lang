/******************************************************************************
 *                                                                            *
 *   tc_unify.c                                                               *
 *                                                                            *
 *   COPYRIGHT (c) 1997 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#include "typecheckI.h"

static TYPE_ASMT_LIST *match(TYPE_EXPR *e1, TYPE_EXPR *e2);
static int type_var_occurs_in_TE(TC_TYPE_VARIABLE var, TYPE_EXPR *expr);
static int type_var_occurs_in_TEL(TC_TYPE_VARIABLE var, TYPE_EXPR **list);
static TYPE_ASMT_LIST *coalesce(TYPE_ASMT *asmt, TYPE_ASMT_LIST *list);
static TYPE_ASMT_LIST *linearize(TYPE_ASMT_LIST *list);
void add_equation_to_SubstList(TYPE_EXPR *e1, TYPE_EXPR *e2);

/***********/
/*         */
/*  match  */
/*         */
/***********/
static TYPE_ASMT_LIST *match(TYPE_EXPR *e1, TYPE_EXPR *e2)
/*
 *  produce a list of asmts [V1:=E1,...,Vn:=En] (such that no Vi occurs in Ei)
 *   which must simultaneously hold for e1 and e2 to match
 */
{
  TYPE_EXPR **list1, **list2;
  TYPE_ASMT_LIST *match_list;
  char *str1, *str2;
  int i;
  switch (e1->tag) {
  case USER_VAR:
    switch (e2->tag) {
    case TYPE_VAR:
      return TAL_cons(type_asmt(e2->id.var,e1),0);
    case USER_VAR:
      if (tc_type_var_match(e1->id.var,e2->id.var)) return 0; /* discard U=U */
    default:
      tc_close_typechecker(0);
      printMsg(ERROR_MSG,"some user specified type is too general");
    }
  case TYPE_VAR:
    switch (e2->tag) {
    case TYPE_VAR:
      if (tc_type_var_match(e1->id.var,e2->id.var)) return 0; /* discard V=V */
    case USER_VAR:
      return TAL_cons(type_asmt(e1->id.var,e2),0);
    case TYPE_CON:
      if (type_var_occurs_in_TEL(e1->id.var,e2->params)) {
	tc_close_typechecker(0);
	printMsg(ERROR_MSG,"unable to unify types (failed occurs check)");
      }
      return TAL_cons(type_asmt(e1->id.var,e2),0);
    default:
      printMsg(FATAL_MSG,"(typecheck) match: unknown e2 tag");
    }
  case TYPE_CON:
    switch (e2->tag) {
    case TYPE_VAR:
      if (type_var_occurs_in_TEL(e2->id.var,e1->params)) {
	tc_close_typechecker(0);
	printMsg(ERROR_MSG,"unable to unify types (failed occurs check)");
      }
      return TAL_cons(type_asmt(e2->id.var,e1),0);
    case TYPE_CON:
      list1 = e1->params;
      list2 = e2->params;
      if (tc_type_con_match(e1->id.con,e2->id.con)) {
	/* length of list1 and list2 are assumed to be equal */
	if (!list1) return 0;
	match_list = 0;
	i=0;
	while (list1[i]) {
	  match_list = TAL_append(match_list,match(list1[i],list2[i]));
	  i++;
	}
	return match_list;
      }
      else {
        /* special handling for @ type constructors (assumes all other tycons >= 0) */
	if (e2->id.con < 0) return match(e1,list2[0]);
	if (e1->id.con < 0) {
	  str2 = st_KeyToName(e2->id.con);
	  tc_close_typechecker(0);
	  printMsg(ERROR_MSG,"cannot propagate @ exception through %s type",str2);
	}
	/* unification error */
	str1 = st_KeyToName(e1->id.con);
	str2 = st_KeyToName(e2->id.con);
	tc_close_typechecker(0);
	printMsg(ERROR_MSG,"unable to unify %s with %s",str1,str2);
      }
    case USER_VAR:
      tc_close_typechecker(0);
      printMsg(ERROR_MSG,"some user specified type is too general");
    default:
      printMsg(FATAL_MSG,"(typecheck) match: unknown e2 tag'");
    }
  default:
    printMsg(FATAL_MSG,"(typecheck) match: unknown e1 tag");
  }
}

/***************************/
/*                         */
/*  type_var_occurs_in_TE  */
/*                         */
/***************************/
static int type_var_occurs_in_TE(TC_TYPE_VARIABLE var, TYPE_EXPR *expr)
{
  switch (expr->tag) {
  case USER_VAR:
  case TYPE_VAR:
    return tc_type_var_match(var,expr->id.var);
  case TYPE_CON:
    return type_var_occurs_in_TEL(var,expr->params);
  default:
    printMsg(FATAL_MSG,"(typecheck) type_var_occurs_in_TE: unknown tag");
  }
}

/****************************/
/*                          */
/*  type_var_occurs_in_TEL  */
/*                          */
/****************************/
static int type_var_occurs_in_TEL(TC_TYPE_VARIABLE var, TYPE_EXPR **list)
{
  int i;
  if (!list) return 0;
  i=0;
  while (list[i]) {
    if (type_var_occurs_in_TE(var,list[i])) return 1;
    i++;
  }
  return 0;
}

/***************************/
/*                         */
/*  type_con_occurs_in_TE  */
/*                         */
/***************************/
int type_con_occurs_in_TE(TC_TYPE_CONSTRUCTOR con, TYPE_EXPR *expr)
{
  TYPE_EXPR **list;
  int i;
  switch (expr->tag) {
  case USER_VAR:
  case TYPE_VAR:
    return 0;
  case TYPE_CON:
    if (tc_type_con_match(con,expr->id.con)) return 1;
    list = expr->params;
    if (!list) return 0;
    i=0;
    while (list[i]) {
      if (type_con_occurs_in_TE(con,list[i])) return 1;
      i++;
    }
    return 0;
  default:
    printMsg(FATAL_MSG,"(typecheck) type_con_occurs_in_TE: unknown tag");
  }
}

/**************/
/*            */
/*  coalesce  */
/*            */
/**************/
static TYPE_ASMT_LIST *coalesce(TYPE_ASMT *asmt, TYPE_ASMT_LIST *list)
/*
 *  coalesce(S/A,[]) = []
 *  coalesce(S/A,T/A::Rest) = append(match(S,T),coalesce(S/A,Rest))
 *  coalesce(S/A,T/B::Rest) = T[S/A]/B :: coalesce(S/A,Rest)
 */
{
  TYPE_ASMT *asmt1;
  TYPE_EXPR *expr1;
  if (!list) return 0;
  asmt1 = TAL_head(list);
  if (tc_type_var_match(asmt->var,asmt1->var)) {
    return TAL_append(match(asmt->rhs.expr,asmt1->rhs.expr),coalesce(asmt,TAL_tail(list)));
  }
  expr1 = subst_in_TE(asmt,asmt1->rhs.expr);
  if (expr1  &&  expr1->tag==TYPE_VAR  &&  tc_type_var_match(asmt1->var,expr1->id.var)) {
    return coalesce(asmt,TAL_tail(list));	/* discard V:=V */
  }
  if (type_var_occurs_in_TE(asmt1->var,expr1)) {
    tc_close_typechecker(0);
    printMsg(ERROR_MSG,"unable to unify types (failed occurs check)");
  }
  asmt1->rhs.expr = expr1;
  return TAL_cons(asmt1,coalesce(asmt,TAL_tail(list)));
}

/***************/
/*             */
/*  linearize  */
/*             */
/***************/
static TYPE_ASMT_LIST *linearize(TYPE_ASMT_LIST *list)
/*
 *  linearize [] = []
 *  linearize S/A::Rest = S/A::linearize(coalesce(S/A,Rest))
 */
{
  TYPE_ASMT *asmt;
  if (!list) return 0;
  asmt = TAL_head(list);
  return TAL_cons(asmt,linearize(coalesce(asmt,TAL_tail(list))));
}

/*******************************/
/*                             */
/*  add_equation_to_SubstList  */
/*                             */
/*******************************/
void add_equation_to_SubstList(TYPE_EXPR *e1, TYPE_EXPR *e2)
/*
 *  add the constraint e1=e2 to the global substitution list
 */
{
/* disp_TE(e1);printf("=");disp_TE(e2);printf("\n"); */

  e1 = subst_all_in_TE(SubstList,e1);
  e2 = subst_all_in_TE(SubstList,e2);
  SubstList = TAL_append(SubstList,linearize(match(e1,e2)));
}
