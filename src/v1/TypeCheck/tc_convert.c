/******************************************************************************
 *                                                                            *
 *   tc_convert.c                                                             *
 *                                                                            *
 *   COPYRIGHT (c) 1997 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

/*
 *  code for converting between ST_TYPE and TYPE_EXPR structures
 */

#include "typecheckI.h"


/***************************/
/*                         */
/*  convert_ST_TYPE_to_TE  */
/*                         */
/***************************/
TYPE_EXPR *convert_ST_TYPE_to_TE(ST_TYPE *st_type)
/*
 *  convert an ST_TYPE structure to a TYPE_EXPR structure
 */
{
  TYPE_EXPR *expr;
  int numparams,i;

  if (!st_type) return 0;
  expr = (TYPE_EXPR *) MemHeapAlloc(tc_memory,1,sizeof(TYPE_EXPR));
  switch (st_type->tag) {
  case TYPE_1:
    expr->tag = TYPE_CON;
    expr->id.con = st_NameToKey(TERMINAL_TYPE);
    expr->params = 0;
    return expr;
  case TYPE_PROD:
    expr->tag = TYPE_CON;
    expr->id.con = st_type->info.prod.key;
    expr->params = (TYPE_EXPR **) MemHeapAlloc(tc_memory,3,sizeof(TYPE_EXPR *));
    expr->params[0] = convert_ST_TYPE_to_TE(st_type->info.prod.l);
    expr->params[1] = convert_ST_TYPE_to_TE(st_type->info.prod.r);
    expr->params[2] = 0;
    return expr;
  case TYPE_PARAMETRIC_VAR:
    expr->tag = TYPE_VAR;
    expr->id.var = st_type->info.param_var;
    return expr;
  case TYPE_STATE_VAR:
    expr->tag = TYPE_VAR;
    expr->id.var = -1;		/* since param_var undefined in this case */
    return expr;
  case TYPE_USER_DATA:
    expr->tag = TYPE_CON;
    expr->id.con = st_type->info.user_data.key;
    numparams = st_GetNumParams(st_type->info.user_data.key);
    if (numparams == 0) expr->params = 0;
    else {
      expr->params = (TYPE_EXPR **) MemHeapAlloc(tc_memory,numparams+1,sizeof(TYPE_EXPR *));
      for (i=0; i<numparams; i++) {
	expr->params[i] = convert_ST_TYPE_to_TE(st_type->info.user_data.args[i]);
      }
      expr->params[i] = 0;
    }
    return expr;

  case TYPE_BUILTIN_INT:       /* [BI] BUILTIN INTEGERS   */
    expr->tag = TYPE_CON;
    expr->id.con = st_NameToKey(INT_TYPENAME);
    expr->params = 0;
    return expr;

  case TYPE_BUILTIN_CHAR:      /* [BI] BUILTIN CHARACTERS */
    expr->tag = TYPE_CON;
    expr->id.con = st_NameToKey(CHAR_TYPENAME);
    expr->params = 0;
    return expr;

  default:
    printMsg(FATAL_MSG,"(typecheck) convert_ST_TYPE_to_TE: unknown st tag");
  }
}

/***************************/
/*                         */
/*  convert_TE_to_ST_TYPE  */
/*                         */
/***************************/
ST_TYPE *convert_TE_to_ST_TYPE(TYPE_EXPR *expr)
/*
 *  convert a TYPE_EXPR to a ST_TYPE (the ST_TYPE is built in tc_memory)
 */
{
  ST_TYPE *st_type;
  int numparams, i;

  if (!expr) return 0;
  st_type = (ST_TYPE *) MemHeapAlloc(tc_memory,1,sizeof(ST_TYPE));
  switch (expr->tag) {
  case TYPE_VAR:
  case USER_VAR:
    st_type->tag = TYPE_PARAMETRIC_VAR;
    st_type->info.param_var = expr->id.var;
    return st_type;

  case TYPE_CON:
    if (tc_type_con_match(expr->id.con, st_NameToKey(INT_TYPENAME))) {
      st_type->tag = TYPE_BUILTIN_INT;
      return st_type;
    }
    if (tc_type_con_match(expr->id.con, st_NameToKey(CHAR_TYPENAME))) {
      st_type->tag = TYPE_BUILTIN_CHAR;
      return st_type;
    }
    st_type->tag = st_GetDatatypeTag(expr->id.con);
    switch (st_type->tag) {
    case TYPE_1:
      return st_type;
    case TYPE_PROD:
      st_type->info.prod.key = expr->id.con;
      st_type->info.prod.l = convert_TE_to_ST_TYPE(expr->params[0]);
      st_type->info.prod.r = convert_TE_to_ST_TYPE(expr->params[1]);
      return st_type;
    case TYPE_USER_DATA:
      st_type->info.user_data.name = st_KeyToName(expr->id.con);
      st_type->info.user_data.key = expr->id.con;
      numparams = TEL_length(expr->params);
      if (numparams == 0) st_type->info.user_data.args = 0;
      else {
	st_type->info.user_data.args = (ST_TYPE **) MemHeapAlloc(tc_memory,numparams+1,sizeof(ST_TYPE *));
	i=0;
	while (expr->params[i]) {
	  st_type->info.user_data.args[i] = convert_TE_to_ST_TYPE(expr->params[i]);
	  i++;
	}
	st_type->info.user_data.args[i] = 0;
      }
      return st_type;
    default:
      printMsg(FATAL_MSG,"(typecheck) convert_TE_to_ST_TYPE: unknown ST tag");
    }
  default:
    printMsg(FATAL_MSG,"(typecheck) convert_TE_to_ST_TYPE: unknown TE tag");
  }
}
