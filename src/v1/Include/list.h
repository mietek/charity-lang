/******************************************************************************
 *                                                                            *
 *   list.h                                                                   *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __LIST_H__
#define __LIST_H__

#include "pmem.h"   /* memory manager */
#include "types.h"

/***************************************
 *                                     *
 *    All of the known list elements   *
 *                                     *
 ***************************************/

typedef enum
{
  L_TYPE,
  L_ST_TYPE,
  L_STRUCTOR,
  L_STRING,
  L_MACRO,
  L_P_STRUCTOR,
  L_T_PHRASE,
  L_LIST_T_PHRASE,
  L_TERM,
  L_FUN_PHRASE,        /* [H-O] ADDED (SEE BELOW) */
  L_RECORD,
  L_FOLD,
  L_UNFOLD,
  L_PECT_T_PHRASE,
  L_PM_LIST_CASE_STATE,
  L_PE_PATT,
  L_ALIAS,
  L_CT_TERM,
  L_CT_EXPR,
  L_CT_EXPRADDR,
  L_INT,
  L_VOIDPTR,           /* for pointers, see pmLib.c */
  L_MEM_PAGE,
  L_PM_PHRASE,
  L_TYPE_ASMT,	       /* typechecker */
  L_TYPED_PATT	       /* typechecker */
}
LIST_TYPE;

typedef struct _LIST
{
    char         *item;
    struct _LIST *next;
    LIST_TYPE    lt;
} LIST;


/***************************************
 *                                     *
 *        type list aliases            *
 *                                     *
 ***************************************/

typedef LIST PE_LIST_TYPE;
typedef LIST PE_LIST_STRUCTOR;
typedef LIST PE_LIST_MACRO;
typedef LIST PE_LIST_P_STRUCTOR;
typedef LIST PE_LIST_T_PHRASE;
typedef LIST PECT_LIST_T_PHRASE;
typedef LIST PM_LIST_CASE_STATE;
typedef LIST PE_LIST_TERM;
typedef LIST PE_LIST_RECORD;
typedef LIST PE_LIST_FOLD;
typedef LIST PE_LIST_UNFOLD;
typedef LIST PE_LIST_LIST_T_PHRASE;
typedef LIST PE_LIST_PATT;
typedef LIST CT_LIST_TERM;
typedef LIST INT_LIST;
typedef LIST VOIDPTR_LIST;           /* for pointers; see pmLib.c */
typedef LIST STR_LIST;
typedef LIST CP_LIST_ALIAS;          /* CP for compiler */
typedef LIST MEM_LIST_PAGE;          /* Memory manager  */
typedef LIST PE_LIST_FUN_PHRASE;     /* [H-O] ADDED THIS TYPE (SEE term.y) */
typedef LIST CT_LIST_EXPR;           /* Used in pattern matcher */
typedef LIST CT_LIST_EXPRADDR;       /* Used in pattern matcher */
typedef LIST PM_LIST_PHRASE;         /* Used in pattern matcher */

typedef LIST ST_LIST_TYPE;

/***************************************
 *                                     *
 *         list functions              *
 *                                     *
 ***************************************/

extern LIST *_cons(char *x, LIST *l, LIST_TYPE lt, MEMORY mdesc);
extern LIST *_append(LIST *L1, LIST *L2);
extern char *_head(LIST *l);

extern char *ListHead(LIST *list);
extern LIST *ListTail(LIST *list);

extern void      printList(LIST *list);
extern void      showList(LIST *list, BBOOL showBrackets);
extern int       ListLen(LIST *list);
extern char     *ListGetNthElement(int n, LIST *list);
extern LIST     *ListRemNthElement(int n, LIST *list);

extern STR_LIST    *StrListCons(char *x, STR_LIST *l, MEMORY hd);
extern int          StrListLen(STR_LIST  *list);
extern BBOOL        StrListMember(STR_LIST *list, char *item);
extern int          StrListPosn(char *item, STR_LIST *list);
extern char        *StrListHead(STR_LIST *list);
extern STR_LIST    *StrListTail(STR_LIST *list);
extern STR_LIST    *StrListAppend(STR_LIST *l1, STR_LIST *l2);
extern STR_LIST    *StrListReverse(STR_LIST *L);
extern char        *StrListImplode(STR_LIST *list, MEMORY heap);

#endif
