/******************************************************************************
 *                                                                            *
 *   tc_list.c                                                                *
 *                                                                            *
 *   COPYRIGHT (c) 1997 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#include "typecheckI.h"

/*
 *  list functions used only by the type checker
 */

/****************
 *              *
 *  TEL_length  *
 *              *
 ****************/
int TEL_length(TYPE_EXPR **list)
{
  int i=0;
  if (!list) return 0;
  while (list[i]) i++;
  return i;
}

/**************
 *            *
 *  TAL_head  *
 *            *
 **************/
TYPE_ASMT *TAL_head(TYPE_ASMT_LIST *list)
{
  return (TYPE_ASMT *) ListHead((LIST *) list);
}

/**************
 *            *
 *  TPL_head  *
 *            *
 **************/
TYPED_PATT *TPL_head(TYPED_PATT_LIST *list)
{
  return (TYPED_PATT *) ListHead((LIST *) list);
}

/**************
 *            *
 *  TAL_tail  *
 *            *
 **************/
TYPE_ASMT_LIST *TAL_tail(TYPE_ASMT_LIST *list)
{
  return (TYPE_ASMT_LIST *) ListTail((LIST *) list);
}

/**************
 *            *
 *  TPL_tail  *
 *            *
 **************/
TYPED_PATT_LIST *TPL_tail(TYPED_PATT_LIST *list)
{
  return (TYPED_PATT_LIST *) ListTail((LIST *) list);
}

/**************
 *            *
 *  TAL_cons  *
 *            *
 **************/
TYPE_ASMT_LIST *TAL_cons(TYPE_ASMT *asmt, TYPE_ASMT_LIST *list)
{
  return (TYPE_ASMT_LIST *) _cons((char *) asmt, (LIST *) list, L_TYPE_ASMT, tc_memory);
}

/**************
 *            *
 *  TPL_cons  *
 *            *
 **************/
TYPED_PATT_LIST *TPL_cons(TYPED_PATT *tp, TYPED_PATT_LIST *list)
{
  return (TYPED_PATT_LIST *) _cons((char *) tp, (LIST *) list, L_TYPED_PATT, tc_memory);
}

/****************
 *              *
 *  TAL_append  *
 *              *
 ****************/
TYPE_ASMT_LIST *TAL_append(TYPE_ASMT_LIST *list1, TYPE_ASMT_LIST *list2)
{
    return (TYPE_ASMT_LIST *) _append((LIST *) list1, (LIST *) list2);
}
