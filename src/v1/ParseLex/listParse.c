/******************************************************************************
 *                                                                            *
 *   listParse.c                                                              *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#include <stdio.h>
#include <string.h>
#include <assert.h>     /* [H-O] ADDED THIS INCLUDE */
#include "types.h"
#include "parse.h"
#include "list.h"
#include "pmem.h"
#include "pm.h"

/*********************************
 *                               *
 *    TypeListCons               *
 *                               *
 *********************************/
PE_LIST_TYPE
*TypeListCons(PE_TYPE *x, PE_LIST_TYPE *l)
{     
     return((PE_LIST_TYPE *) _cons((char *) x, (LIST *) l, L_TYPE, parseHeapDesc));
}

/*********************************
 *                               *
 *    StructorListCons           *
 *                               *
 *********************************/
PE_LIST_STRUCTOR
*StructorListCons(PE_STRUCTOR *x, PE_LIST_STRUCTOR *l)
{     
     return((PE_LIST_STRUCTOR *) _cons((char *) x, (LIST *) l, L_STRUCTOR, parseHeapDesc));
}

/*********************************
 *                               *
 *    StructorListAppend         *
 *                               *
 *********************************/
PE_LIST_STRUCTOR
*StructorListAppend(PE_LIST_STRUCTOR *L1, PE_LIST_STRUCTOR *L2)
{
     return((PE_LIST_STRUCTOR *) _append((LIST *) L1, (LIST *) L2));
}

/*********************************
 *                               *
 *    MacroListCons              *
 *                               *
 *********************************/
PE_LIST_MACRO
*MacroListCons(PE_MACRO *x, PE_LIST_MACRO *l)
{
     return((PE_LIST_MACRO *) _cons((char *) x, (LIST *) l, L_MACRO, parseHeapDesc));
}

/*********************************
 *                               *
 *    MacroListHead              *
 *                               *
 *********************************/
PE_MACRO
*MacroListHead(PE_LIST_MACRO *list)
{
     return((PE_MACRO *) ListHead((LIST *) list));
}

/*********************************
 *                               *
 *    MacroListTail           *
 *                               *
 *********************************/
PE_LIST_MACRO
*MacroListTail(PE_LIST_MACRO *list)
{
     return((PE_LIST_MACRO *) ListTail((LIST *) list));
}

/*********************************
 *                               *
 *    MacroListMember            *
 *                               *
 *********************************/
BBOOL
MacroListMember(PE_LIST_MACRO *macrolist, char *item) {

   LIST      *list  = (LIST *) macrolist;
   PE_MACRO  *macro = NULL;

  if (list) {
    macro = MacroListHead(list);
    if (strcmp(macro->ident, item)) 
      return(MacroListMember(MacroListTail(list), item));
    else
      return(BTRUE);
  }
  else
    return(BFALSE);
}


/*********************************
 *                               *
 *    MacroListPosn              *
 *                               *
 *********************************/
int
MacroListPosn(char *item, PE_LIST_MACRO *list)
{
     STR_LIST    *tmp   = list;
     PE_MACRO    *macro = NULL;
     int          i     = 0;

     while (tmp) {
	  macro = MacroListHead(tmp);
	  if (strcmp(macro->ident, item) != 0) {
	       tmp = MacroListTail(tmp);
	       i++;
	  }
	  else
	       return(i);
     }
     return(-1);
}


/*********************************
 *                               *
 *    MacroListLen               *
 *                               *
 *********************************/
int
MacroListLen(PE_LIST_MACRO *list) {

  return( ListLen( (LIST *)list ) );

}


/*********************************
 *                               *
 *    P_StructorListCons         *
 *                               *
 *********************************/
PE_LIST_P_STRUCTOR
*P_StructorListCons(P_STRUCTOR *x, PE_LIST_P_STRUCTOR *l)
{
     return((PE_LIST_P_STRUCTOR *) _cons((char *) x, (LIST *) l, L_P_STRUCTOR, parseHeapDesc));
}

/*********************************
 *                               *
 *    T_PhraseListCons           *
 *                               *
 *********************************/
PE_LIST_T_PHRASE
*T_PhraseListCons(PE_T_PHRASE *x, PE_LIST_T_PHRASE *l)
{
     return((PE_LIST_T_PHRASE *) _cons((char *) x, (LIST *) l, L_T_PHRASE, parseHeapDesc));
}

/*********************************
 *                               *
 *    T_PhraseListAppend         *
 *                               *
 *********************************/
PE_LIST_T_PHRASE
*T_PhraseListAppend(PE_LIST_T_PHRASE *l1, PE_LIST_T_PHRASE *l2)
{
     return((PE_LIST_T_PHRASE *) _append((LIST *) l1, (LIST *) l2));
}

/*********************************
 *                               *
 *    T_PhraseListHead           *
 *                               *
 *********************************/
PE_T_PHRASE
*T_PhraseListHead(PE_LIST_T_PHRASE *list)
{
     return((PE_T_PHRASE *) ListHead((LIST *) list));
}

/*********************************
 *                               *
 *    T_PhraseListTail           *
 *                               *
 *********************************/
PE_LIST_T_PHRASE
*T_PhraseListTail(PE_LIST_T_PHRASE *list)
{
     return((PE_LIST_T_PHRASE *) ListTail((LIST *) list));
}


/*********************************
 *                               *
 *    T_PhraseListLen            *
 *                               *
 *********************************/
int 
T_PhraseListLen(PE_LIST_T_PHRASE *list) {

  return( ListLen( (LIST *)list ) );

}


/*********************************
 *                               *
 *    T_PhraseListListCons       *
 *                               *
 *********************************/
PE_LIST_LIST_T_PHRASE
*T_PhraseListListCons(PE_LIST_T_PHRASE *x, PE_LIST_LIST_T_PHRASE *l)
{
     return((PE_LIST_LIST_T_PHRASE *) _cons((char *) x, (LIST *) l, L_LIST_T_PHRASE, parseHeapDesc));
}

/*********************************
 *                               *
 *    T_PhraseListListHead       *
 *                               *
 *********************************/
PE_LIST_T_PHRASE
*T_PhraseListListHead(PE_LIST_LIST_T_PHRASE *list)
{
     return((PE_LIST_T_PHRASE *) ListHead((LIST *) list));
}

/*********************************
 *                               *
 *    T_PhraseListListTail       *
 *                               *
 *********************************/
PE_LIST_LIST_T_PHRASE
*T_PhraseListListTail(PE_LIST_LIST_T_PHRASE *list)
{
     return((PE_LIST_LIST_T_PHRASE *) ListTail((LIST *) list));
}


/*********************************
 *                               *
 *    T_PhraseListListLen        *
 *                               *
 *********************************/
int
T_PhraseListListLen(PE_LIST_LIST_T_PHRASE *list) {

  return( ListLen( (LIST *)list ) );

}


/*********************************
 *                               *
 *    TermListCons               *
 *                               *
 *********************************/
PE_LIST_TERM
*TermListCons(PE_TERM *x, PE_LIST_TERM *list)
{
     return((PE_LIST_TERM *) _cons((char *) x, (LIST *) list, L_TERM, parseHeapDesc));
}

/*********************************
 *                               *
 *    TermListHead               *
 *                               *
 *********************************/
PE_TERM
*TermListHead(PE_LIST_TERM *list)
{
     return((PE_TERM *) ListHead((LIST *) list));
}

/*********************************
 *                               *
 *    TermListTail               *
 *                               *
 *********************************/
PE_LIST_TERM
*TermListTail(PE_LIST_TERM *list)
{
     return((PE_LIST_TERM *) ListTail((LIST *) list));
}

/*********************************
 *                               *
 *    TermListLen                *
 *                               *
 *********************************/
int
TermListLen(PE_LIST_TERM *list) {

  return( ListLen( (LIST *)list ) );
}


/*********************************
 *                               *
 *    FunPhraseListIndex         *
 *                               *
 *********************************/

PE_FUN_PHRASE *
FunPhraseListIndex (PE_LIST_FUN_PHRASE *list,
		    int                 i)
{
  assert (list);
  assert (i >= 0);

  while (i > 0)
    {
      i--;
      list = FunPhraseListTail (list);

      assert (list);
    }

  return FunPhraseListHead (list);
}


/*********************************
 *                               *
 *    FunPhraseListCons          *
 *                               *
 *********************************/

/* [H-O] ADDED THIS FUNCTION: */

PE_LIST_FUN_PHRASE *
FunPhraseListCons (PE_FUN_PHRASE      *x,
		   PE_LIST_FUN_PHRASE *list)     /* MAY BE NULL */
{
  assert (x);

  return ((PE_LIST_FUN_PHRASE *)_cons ((char *)x,
				       (LIST *)list,
				       L_FUN_PHRASE,
				       parseHeapDesc));
}


/*********************************
 *                               *
 *    FunPhraseListHead          *
 *                               *
 *********************************/

/* [H-O] ADDED THIS FUNCTION: */

PE_FUN_PHRASE *
FunPhraseListHead (PE_LIST_FUN_PHRASE *list)
{
  return ((PE_FUN_PHRASE *)ListHead ((LIST *)list));
}


/*********************************
 *                               *
 *    FunPhraseListTail          *
 *                               *
 *********************************/

/* [H-O] ADDED THIS FUNCTION: */

PE_LIST_FUN_PHRASE *
FunPhraseListTail (PE_LIST_FUN_PHRASE *list)
{
  return ((PE_LIST_FUN_PHRASE *)ListTail ((LIST *)list));
}


/*********************************
 *                               *
 *    FunPhraseListLen           *
 *                               *
 *********************************/

/* [H-O] ADDED THIS FUNCTION: */

int
FunPhraseListLen (PE_LIST_FUN_PHRASE *list)
{
  return (ListLen ((LIST *)list));
}


/*********************************
 *                               *
 *    pe_StrListCons             *
 *                               *
 *********************************/
STR_LIST
*pe_StrListCons(char *x, STR_LIST *l)
{     
     return(StrListCons(x, l, parseHeapDesc));
}


/*********************************
 *                               *
 *    pe_StrListImplode          *
 *                               *
 *********************************/
char *
pe_StrListImplode(STR_LIST *l)
{     
     return(StrListImplode(l, parseHeapDesc));
}


/*********************************
 *                               *
 *    StructorListLen            *
 *                               *
 *********************************/
int
StructorListLen(PE_LIST_STRUCTOR  *list) {

  return( ListLen( (LIST *)list ) );

}


/*********************************
 *                               *
 *    StructorListHead           *
 *                               *
 *********************************/
PE_STRUCTOR *
StructorListHead(PE_LIST_STRUCTOR *list) {

  return( (PE_STRUCTOR *)ListHead( (LIST *)list) );

}


/*********************************
 *                               *
 *    StructorListTail           *
 *                               *
 *********************************/
PE_LIST_STRUCTOR       *
StructorListTail(PE_LIST_STRUCTOR *list) {

  return( (PE_LIST_STRUCTOR *)ListTail( (LIST *)list) );

}


/*********************************
 *                               *
 *    TypeListHead               *
 *                               *
 *********************************/
PE_TYPE *
TypeListHead(PE_LIST_TYPE *list) {

  return( (PE_TYPE *)ListHead( (LIST *)list ) );

}


/*********************************
 *                               *
 *    TypeListIndex              *
 *                               *
 *********************************/

PE_TYPE *
TypeListIndex (PE_LIST_TYPE *list,
	       int           index)
{
  assert (list);
  assert (index >= 0);

  while (index > 0)
    {
      index--;
      list = TypeListTail (list);

      assert (list);
    }

  return TypeListHead (list);
}


/*********************************
 *                               *
 *    TypeListTail               *
 *                               *
 *********************************/
PE_LIST_TYPE *
TypeListTail(PE_LIST_TYPE *list) {

  return( (PE_LIST_TYPE *)ListTail( (LIST *)list) );

}

/*********************************
 *                               *
 *    TypeListLen                *
 *                               *
 *********************************/
int
TypeListLen(PE_LIST_TYPE *list) {

  return( ListLen( (LIST *)list) );

}

/*********************************
 *                               *
 *    RecordListCons             *
 *                               *
 *********************************/
PE_LIST_RECORD
*RecordListCons(PE_RECORD *x, PE_LIST_RECORD *list)
{
     return((PE_LIST_RECORD *) _cons((char *) x, (LIST *) list, L_RECORD, parseHeapDesc));
}

/*********************************
 *                               *
 *    RecordListHead             *
 *                               *
 *********************************/
PE_RECORD
*RecordListHead(PE_LIST_RECORD *list)
{
     return((PE_RECORD *) ListHead((LIST *) list));
}

/*********************************
 *                               *
 *    RecordListTail             *
 *                               *
 *********************************/
PE_LIST_RECORD
*RecordListTail(PE_LIST_RECORD *list)
{
     return((PE_LIST_RECORD *) ListTail((LIST *) list));
}

/*********************************
 *                               *
 *    FoldListCons             *
 *                               *
 *********************************/
PE_LIST_FOLD
*FoldListCons(PE_FOLD *x, PE_LIST_FOLD *list)
{
     return((PE_LIST_FOLD *) _cons((char *) x, (LIST *) list, L_FOLD, parseHeapDesc));
}

/*********************************
 *                               *
 *    FoldListHead             *
 *                               *
 *********************************/
PE_FOLD
*FoldListHead(PE_LIST_FOLD *list)
{
     return((PE_FOLD *) ListHead((LIST *) list));
}

/*********************************
 *                               *
 *    FoldListTail             *
 *                               *
 *********************************/
PE_LIST_FOLD
*FoldListTail(PE_LIST_FOLD *list)
{
     return((PE_LIST_FOLD *) ListTail((LIST *) list));
}

/*********************************
 *                               *
 *    FoldListAppend             *
 *                               *
 *********************************/
PE_LIST_FOLD
*FoldListAppend(PE_LIST_FOLD *l1, PE_LIST_FOLD *l2)
{
     return((PE_LIST_FOLD *) _append((LIST *) l1, (LIST *) l2));
}

/*********************************
 *                               *
 *    UnfoldListCons             *
 *                               *
 *********************************/
PE_LIST_UNFOLD
*UnfoldListCons(PE_UNFOLD *x, PE_LIST_UNFOLD *list)
{
     return((PE_LIST_UNFOLD *) _cons((char *) x, (LIST *) list, L_UNFOLD, parseHeapDesc));
}

/*********************************
 *                               *
 *    UnfoldListHead             *
 *                               *
 *********************************/
PE_UNFOLD
*UnfoldListHead(PE_LIST_UNFOLD *list)
{
     return((PE_UNFOLD *) ListHead((LIST *) list));
}

/*********************************
 *                               *
 *    UnfoldListTail             *
 *                               *
 *********************************/
PE_LIST_UNFOLD
*UnfoldListTail(PE_LIST_UNFOLD *list)
{
     return((PE_LIST_UNFOLD *) ListTail((LIST *) list));
}

/*********************************
 *                               *
 *    UnfoldListAppend           *
 *                               *
 *********************************/
PE_LIST_UNFOLD
*UnfoldListAppend(PE_LIST_UNFOLD *l1, PE_LIST_UNFOLD *l2)
{
     return((PE_LIST_UNFOLD *) _append((LIST *) l1, (LIST *) l2));
}

/*********************************
 *                               *
 *    CTTermListCons             *
 *                               *
 *********************************/
CT_LIST_TERM
*CTTermListCons(CT_TERM *x, CT_LIST_TERM *list)
{
     return((CT_LIST_TERM *) _cons((char *) x, (LIST *) list, L_CT_TERM, ctHD));
}

/*********************************
 *                               *
 *    CTTermListHead             *
 *                               *
 *********************************/
CT_TERM
*CTTermListHead(CT_LIST_TERM *list)
{
     return((CT_TERM *) ListHead((LIST *) list));
}

/*********************************
 *                               *
 *    CTTermListTail             *
 *                               *
 *********************************/
CT_LIST_TERM
*CTTermListTail(CT_LIST_TERM *list)
{
     return((CT_LIST_TERM *) ListTail((LIST *) list));
}


/*******************************
 *                             *
 *    printList                *
 *                             *
 *******************************/
void
printList(LIST *list)
{
    LIST *ptr  = list;
    BBOOL flag = BTRUE;   /* first item special case...don't print a comma */

    printf("[");
    while (ptr) {
	if (!flag) {
	    printf(", ");
	}
	flag = BFALSE;

	switch (ptr->lt) {
	   case L_TYPE:
	     printType((PE_TYPE *) ptr->item);
	     break;
	   case L_STRUCTOR:
	     printStructor((PE_STRUCTOR *) ptr->item);
	     break;
	   case L_STRING:
	     printString((char *) ptr->item);
	     break;
	   case L_MACRO:
 	     printMacro((PE_MACRO *) ptr->item);
	     break;
	   case L_T_PHRASE:
 	     printT_Phrase((PE_T_PHRASE *) ptr->item);
	     break;
	   case L_RECORD:
 	     printRecord((PE_RECORD **) ptr->item);
	     break;
	   case L_FOLD:
 	     printFold((PE_FOLD **) ptr->item);
	     break;
	   default:
	     perror("printList(): unknown list type\n");
	     exit(-1);
	    break;
	}
	ptr = ptr->next;
    }
    printf("]");
}


/*******************************
 *                             *
 *    showList                 *
 *                             *
 *******************************/
void
showList(LIST *list, BBOOL showBrackets)
{
    LIST *ptr  = list;
    BBOOL flag = BTRUE;   /* first item special case...don't print a comma */

    if ( showBrackets ) appendBuff("[");
    while (ptr) {
	if (!flag) {
	    appendBuff(", ");
	}
	flag = BFALSE;

	switch (ptr->lt) {
/*	   case L_TYPE:
	     printType((PE_TYPE *) ptr->item);
	     break;
	   case L_STRUCTOR:
	     printStructor((PE_STRUCTOR *) ptr->item);
	     break;
*/
	   case L_STRING:
	     showString((char *) ptr->item);
	     break;
	   case L_MACRO:
 	     peShowMacro((PE_MACRO *) ptr->item);
	     break;
/*	   case L_T_PHRASE:
 	     printT_Phrase((PE_T_PHRASE *) ptr->item);
	     break;
	   case L_RECORD:
 	     printRecord((PE_RECORD **) ptr->item);
	     break;
	   case L_FOLD:
 	     printFold((PE_FOLD **) ptr->item);
	     break;
*/
	   default:
	     perror("printList(): unknown list type\n");
	     exit(-1);
	    break;
	}
	ptr = ptr->next;
    }
    if ( showBrackets ) appendBuff("]");
}
