/******************************************************************************
 *                                                                            *
 *   list.c                                                                   *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "types.h"
#include "list.h"
#include "pmem.h"

/*
 * A list package with run-time checking of data elements.  This will
 * cost extra overhead in examining the type of each item in a list, but
 * can be offset by moving towards C++ with a static type checker for
 * all list items.  
 */

static LIST *_reverse(LIST *L, LIST *prevPtr);
static LIST *ListReverse(LIST *list);

/*********************************
 *                               *
 *    _cons                      *
 *                               *
 *********************************/
/* cons() item to the front of a list
 * INPUT:  x    - item to add to list
 *         l    - the list
 *         lt   - the type of list we have
 *         mdesc - memory descriptor
 */

LIST
*_cons(char *x, LIST *l, LIST_TYPE lt, MEMORY mdesc)
{
     LIST *list = NULL;

     /*  assert(x);  integer lists  */

     if (l) { /* run time tag check */
	 assert(l->lt == lt);
     }

     list = (LIST *) MemHeapAlloc(mdesc, 1, sizeof(LIST));

     list->item = x;
     list->next = l;
     list->lt   = lt;
     return(list);
}

/*********************************
 *                               *
 *    _append                    *
 *                               *
 *********************************/
LIST
*_append(LIST *L1, LIST *L2)
{
     LIST *tmp = L1;
     
     if (!L1) return L2;
     while (tmp->next) {
	  tmp = tmp->next;
     }
     tmp->next = L2;

     return(L1);
}

/*********************************
 *                               *
 *    _head                      *
 *                               *
 *********************************/
char
*_head(LIST *L)
{
     if (L) {
	  return(L->item);
     }
     return(0);
}

/*********************************
 *                               *
 *    _reverse                   *
 *                               *
 *********************************/
static LIST *
_reverse(LIST *L, LIST *prevPtr) {

    LIST   *tmp;

    if ( L == NULL ) 
        return prevPtr;
    else {
        tmp = _reverse(ListTail(L), L);
        L->next = prevPtr;
        return tmp;
    }   /*  esle  */

}

/*********************************
 *                               *
 *    ListTail                   *
 *                               *
 *********************************/
LIST *
ListTail(LIST *list) {

if (list)   return( list->next );
else        return( NULL );

}

/*********************************
 *                               *
 *    ListHead                   *
 *                               *
 *********************************/
char
*ListHead(LIST *L)
{
     if ( L != NULL ) {
       return(L->item);
     } else {
       return(0);
     }
}

/*********************************
 *                               *
 *    ListLen                    *
 *                               *
 *********************************/
int
ListLen(LIST *list)
{
    LIST *tmp = list;
    int   count = 0;

    while (tmp) {
	    count++;
	    tmp = tmp->next;
    }	
    return(count);
}


/*********************************
 *                               *
 *    ListRemNthElement          *
 *                               *
 *********************************/
LIST *
ListRemNthElement(int n, LIST *list)
{
    LIST *currMember = list,
         *prevMember;
    int   count = 0;

    if (n==0) 
      return(list->next);
    else {
      while (count != n) {
        prevMember = currMember;
        currMember = currMember->next;
        count++;
      }
      prevMember->next = currMember->next;
    }   /*  esle  */

    return(list);
}


/*********************************
 *                               *
 *    ListGetNthElement          *
 *                               *
 *********************************/
char *
ListGetNthElement(int n, LIST *list) {

  int   count = 0;
  char *element;

  do {
    element = _head(list);
    list    = ListTail(list);
  } while (count++ < n);

  return(element);
}


/*********************************
 *                               *
 *    ListReverse                *
 *                               *
 *********************************/
static LIST *
ListReverse(LIST *list) {

if ( list )
    return _reverse(list, NULL);
else   
    return NULL;

}

/****************************************************************************/


/*********************************
 *                               *
 *    StrListCons                *
 *                               *
 *********************************/
STR_LIST *
StrListCons(char *x, STR_LIST *l, MEMORY hd)
{     
  return((STR_LIST *) _cons((char *) x, (LIST *) l, L_STRING, hd));
}

/*********************************
 *                               *
 *    StrListLen                 *
 *                               *
 *********************************/
int
StrListLen(STR_LIST  *list) {

  return( ListLen( (LIST *)list ) );

}


/*********************************
 *                               *
 *    StrListMember              *
 *                               *
 *********************************/
BBOOL
StrListMember(STR_LIST *pelist, char *item) {

  if (pelist) 
    if (strcmp(pelist->item, item)) 
      return(StrListMember(pelist->next, item));
    else
      return(BTRUE);
  else
    return(BFALSE);
}


/*********************************
 *                               *
 *    StrListPosn                *
 *                               *
 *********************************/
int
StrListPosn(char *item, STR_LIST *list)
{
    STR_LIST    *tmp = list;
    int          i   = 0;

    while (tmp) {
	  if (strcmp(StrListHead(tmp), item) != 0) {
       tmp = StrListTail(tmp);
       i++;
	  }
	  else
       return(i);
    }
    return(-1);
}


/*********************************
 *                               *
 *    StrListAppend              *
 *                               *
 *********************************/
STR_LIST
*StrListAppend(STR_LIST *l1, STR_LIST *l2)
{
     return((STR_LIST *) _append((LIST *) l1, (LIST *) l2));
}


/*********************************
 *                               *
 *    StrListHead                *
 *                               *
 *********************************/
char
*StrListHead(STR_LIST *list)
{
  return( (char *)ListHead( (LIST *)list ) );

}


/*********************************
 *                               *
 *    StrListTail                *
 *                               *
 *********************************/
STR_LIST
*StrListTail(STR_LIST *list)
{
  return( (STR_LIST *)ListTail( (LIST *)list) );

}


/*********************************
 *                               *
 *    StrListReverse             *
 *                               *
 *********************************/
STR_LIST
*StrListReverse(STR_LIST *list)
{
  return( (STR_LIST *)ListReverse( (LIST *)list) );

}


/*********************************
 *                               *
 *    StrListImplode             *
 *                               *
 *********************************/
char *
StrListImplode(STR_LIST *list, MEMORY heap) {

    int         len = 0;
    char       *str = NULL;
    char       *result = NULL;
    char       *s = NULL;
    STR_LIST   *listx = list;

    while ( (s = StrListHead(listx)) ) {
        listx = StrListTail(listx);
        len += strlen(s);
    }   /*  elihw  */

    result = str = (char *)MemHeapAlloc(heap, len+1, sizeof(char));

    while ( (s = StrListHead(list)) ) {
        list = StrListTail(list);
        while ( (*str++ = *s++) )
            ;
        *str--;
    }   /*  elihw  */
    result[len] = NULL;

    return result;

}   /*  end StrListImplode()  */

