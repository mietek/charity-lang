/******************************************************************************
 *                                                                            *
 *   listParse.h                                                              *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
#ifndef __LISTPARSE_H__
#define __LISTPARSE_H__

#include "coreTL.h"
#include "list.h"
#include "types.h"

/*********************************************
 *                                           *
 * Routines for typecasting generic routines *
 *                                           *
 *********************************************/
extern PE_LIST_P_STRUCTOR *P_StructorListCons(P_STRUCTOR *x, PE_LIST_P_STRUCTOR *l);

/*******************************
 *                             *
 *    PE_LIST_MACRO            *
 *                             *
 *******************************/
extern PE_LIST_MACRO      *MacroListCons(PE_MACRO *x, PE_LIST_MACRO *l);
extern PE_MACRO           *MacroListHead(PE_LIST_MACRO *list);
extern PE_LIST_MACRO      *MacroListTail(PE_LIST_MACRO *list);
extern BBOOL               MacroListMember(PE_LIST_MACRO *macrolist, char *item);
extern int                 MacroListPosn(char *item, PE_LIST_MACRO *list);
extern int                 MacroListLen(PE_LIST_MACRO *list);

/*******************************
 *                             *
 *    PE_LIST_T_PHRASE         *
 *                             *
 *******************************/
extern PE_LIST_T_PHRASE   *T_PhraseListCons(PE_T_PHRASE *x, PE_LIST_T_PHRASE *l);
extern PE_LIST_T_PHRASE   *T_PhraseListAppend(PE_LIST_T_PHRASE *l1, PE_LIST_T_PHRASE *l2);
extern PE_T_PHRASE        *T_PhraseListHead(PE_LIST_T_PHRASE *list);
extern PE_LIST_T_PHRASE   *T_PhraseListTail(PE_LIST_T_PHRASE *list);
extern int T_PhraseListLen(PE_LIST_T_PHRASE *list);

/*******************************
 *                             *
 *    PE_LIST_LIST_T_PHRASE    *
 *                             *
 *******************************/
extern PE_LIST_LIST_T_PHRASE   *T_PhraseListListCons(PE_LIST_T_PHRASE *x, PE_LIST_LIST_T_PHRASE *l);
extern PE_LIST_T_PHRASE        *T_PhraseListListHead(PE_LIST_LIST_T_PHRASE *list);
extern PE_LIST_LIST_T_PHRASE   *T_PhraseListListTail(PE_LIST_LIST_T_PHRASE *list);
extern int                      T_PhraseListListLen(PE_LIST_LIST_T_PHRASE *list);

/*******************************
 *                             *
 *    PE_LIST_TERM             *
 *                             *
 *******************************/
extern PE_LIST_TERM       *TermListCons(PE_TERM *x, PE_LIST_TERM *list);
extern PE_TERM            *TermListHead(PE_LIST_TERM *list);
extern PE_LIST_TERM       *TermListTail(PE_LIST_TERM *list);
extern int                 TermListLen(PE_LIST_TERM *list);


/*******************************
 *                             *
 *    PE_LIST_FUN_PHRASE       *
 *                             *
 *******************************/

/* [H-O] ADDED THESE PROTOTYPES (SEE listParse.c): */

extern PE_LIST_FUN_PHRASE *FunPhraseListCons  (PE_FUN_PHRASE      *x,
					       PE_LIST_FUN_PHRASE *list);
extern PE_FUN_PHRASE      *FunPhraseListHead  (PE_LIST_FUN_PHRASE *list);
extern PE_LIST_FUN_PHRASE *FunPhraseListTail  (PE_LIST_FUN_PHRASE *list);
extern int                 FunPhraseListLen   (PE_LIST_FUN_PHRASE *list);
extern PE_FUN_PHRASE      *FunPhraseListIndex (PE_LIST_FUN_PHRASE *list,
					       int                 i);


/*******************************
 *                             *
 *    CT_LIST_TERM             *
 *                             *
 *******************************/
extern CT_LIST_TERM       *CTTermListCons(CT_TERM *x, CT_LIST_TERM *list);
extern CT_TERM            *CTTermListHead(CT_LIST_TERM *list);
extern CT_LIST_TERM       *CTTermListTail(CT_LIST_TERM *list);

/*******************************
 *                             *
 *    STR_LIST                 *
 *                             *
 *******************************/
extern STR_LIST           *pe_StrListCons(char *x, STR_LIST *l);
extern char               *pe_StrListImplode(STR_LIST *l);

/*******************************
 *                             *
 *    PE_LIST_STRUCTOR         *
 *                             *
 *******************************/
extern int                 StructorListLen(PE_LIST_STRUCTOR  *list);
extern PE_STRUCTOR        *StructorListHead(PE_LIST_STRUCTOR *list);
extern PE_LIST_STRUCTOR   *StructorListTail(PE_LIST_STRUCTOR *list);
extern PE_LIST_STRUCTOR   *StructorListCons(PE_STRUCTOR *x, 
					    PE_LIST_STRUCTOR *l);
extern PE_LIST_STRUCTOR   *StructorListAppend(PE_LIST_STRUCTOR *L1, 
					      PE_LIST_STRUCTOR *L2);

/*******************************
 *                             *
 *    PE_LIST_TYPE             *
 *                             *
 *******************************/
extern PE_LIST_TYPE       *TypeListCons(PE_TYPE *x, PE_LIST_TYPE *l);
extern PE_TYPE            *TypeListHead(PE_LIST_TYPE *list);
extern PE_LIST_TYPE       *TypeListTail(PE_LIST_TYPE *list);
extern int                 TypeListLen(PE_LIST_TYPE *list);
extern PE_TYPE            *TypeListIndex(PE_LIST_TYPE *list,
					 int index);

extern PE_LIST_RECORD     *RecordListCons(PE_RECORD *x, PE_LIST_RECORD *list);
extern PE_RECORD          *RecordListHead(PE_LIST_RECORD *list);
extern PE_LIST_RECORD     *RecordListTail(PE_LIST_RECORD *list);

extern PE_LIST_FOLD       *FoldListCons(PE_FOLD *x, PE_LIST_FOLD *list);
extern PE_FOLD            *FoldListHead(PE_LIST_FOLD *list);
extern PE_LIST_FOLD       *FoldListTail(PE_LIST_FOLD *list);
extern PE_LIST_FOLD       *FoldListAppend(PE_LIST_FOLD *L1, PE_LIST_FOLD *L2);

extern PE_LIST_UNFOLD     *UnfoldListCons(PE_UNFOLD *x, PE_LIST_UNFOLD *list);
extern PE_UNFOLD          *UnfoldListHead(PE_LIST_UNFOLD *list);
extern PE_LIST_UNFOLD     *UnfoldListTail(PE_LIST_UNFOLD *list);
extern PE_LIST_UNFOLD     *UnfoldListAppend(PE_LIST_UNFOLD *L1, PE_LIST_UNFOLD *L2);

#endif


