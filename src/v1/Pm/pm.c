/******************************************************************************
 *                                                                            *
 *   pm.c                                                                     *
 *                                                                            *
 *   COPYRIGHT (c) 1995, 1996 by Charity Development Group.                   *
 *   All rights reserved.                                                     *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

/**************************************************************************
 *                                                                        *
 *           MAIN MODULE TO DO PATTERN MATCHING TRANSLATION               *
 *                                                                        *
 **************************************************************************/

#include <stddef.h>
#include "pm.h"
#include "pmPrivate.h"


/**************************************************************************
 *                                                                        *
 *           Global variables                                             *
 *                                                                        *
 **************************************************************************/
MEMORY              ctHD;  /* heap descriptor for core expr. */
MEMORY              scratchHD;

CT_EXPR     *PmResult;

CT_EXPR     *bangExpr;
CT_VAR_BASE *bangVarBase;

/**************************************************************************
 *                                                                        *
 *           Prototypes (internal only)                                   *
 *                                                                        *
 **************************************************************************/



/**************************************************************************
 *                                                                        *
 *           Function Definitions                                         *
 *                                                                        *
 **************************************************************************/

/*********************************
 *                               *
 *    pmInit                     *
 *                               *
 *********************************/
void
pmInit(void) {

  ctHD = MemAlloc("Core Term Logic Heap (from pm)", CTHEAP_SIZE, sizeof(char));

}


/*********************************
 *                               *
 *    pm_DestructCTHeap          *
 *                               *
 *********************************/
void
pmDestructCTHeap(void) {

  MemDealloc(ctHD);

}


/*********************************
 *                               *
 *    pmTranslate                *
 *                               *
 *********************************/
CT_EXPR  *
pmTranslate(PE_EXPR  *peExpr, int kill_a_def) {

  pmkill_a_def = kill_a_def;

  MemReset(ctHD);
  scratchHD = MemAlloc("pm scratch", SCRATCH_HEAP_SIZE, sizeof(char));

  /* () expressions & variable bases can be reused */
  bangExpr = ctMakeBangExpr(ctHD);
  bangVarBase = ctMakeVarBase(ctHD, NULL, BFALSE);

  PmResult = transPeExpr(peExpr);
  cleanup();
  return(PmResult);

}
