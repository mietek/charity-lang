/******************************************************************************
 *                                                                            *
 *   gc.c                                                                     *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include <stdio.h>

#include "types.h"
#include "ioChar.h"
#include "pmem.h"
#include "decompile.h"
#include "machine.h"
#include "machine_private.h"
#include "gc.h"

#define gcDebug 0

/*****************************************************************************
 *                                                                           *
 *                           internal prototypes                             *
 *                                                                           *
 *****************************************************************************/

static V_INSTR *gc_move(V_INSTR *src);
static void     gc_collect(void);
static V_INSTR *gc_collect1(V_INSTR *start);
static V_INSTR *gc_collect2(V_INSTR *start);

static void     gc_closures(void);

static void     gc_dump2(D_INSTR *Dstack2, D_INSTR *D2ptr);
static void     gc_dump3(D_INSTR *Dstack3, D_INSTR *D3ptr);
static void     gc_dump4(D_INSTR *Dstack4, D_INSTR *D4ptr);

/*****************************************************************************
 *                                                                           *
 *                           internal variables                              *
 *                                                                           *
 *****************************************************************************/
static V_INSTR *oldHeap2Top = NULL;
static V_INSTR *oldHeap2Bot = NULL;

static BBOOL    extraHeapOn = BFALSE;
static MEMORY   extraHeapHD = -1;
static V_INSTR *extraHeap   = NULL;

static V_INSTR *extHeapBot  = NULL;
static V_INSTR *extHeapTop  = NULL;

/***************************************
 *                                     *
 *            gc_closures              *
 *                                     *
 ***************************************/
static
void
gc_closures(void)
{
     /* used in collecting closures in right display mode */
     int      num   = 0;
     int      count = 0;
     CLOSURE *clo   = NULL;

     num = getNumClosures();
     for (count = 0; count < num; count++) {
	  clo = getClosure((CLO_TAB_ENTRY) count);
	  clo->rec = gc_move(clo->rec);
	  clo->clo = gc_move(clo->clo);
     }
}

/***************************************
 *                                     *
 *            gc_move                  *
 *                                     *
 ***************************************/
static
V_INSTR
*gc_move(V_INSTR *src)
{
     V_INSTR *new_locn  = NULL;
     V_INSTR *new_locn1 = NULL;
     int     count     = 0;
     int     num       = 0;

     if (src != NULL) {
	  if (src == _BANG) {                       /* ! */
	       V_assert(src->instr == MCbang);
	       new_locn = src;
	  } else if ( ((src > oldHeap2Top) && (src <= oldHeap2Bot)) ||
		      ((extraHeapOn == BTRUE) && (src > extHeapTop) && (src <= extHeapBot)) ) { /* pairs */
	       if (src->info.gc.gcId == GC_MOVED) {      /* item already collected */
		    V_assert(src->instr == MCgc_ptr);
		    new_locn = src->info.gc.heapItm;
	       } else {
		    V_assert(src->instr == MCpair);
		    V_set(H2->instr = MCpair);
		    H2->info = src->info;

		    V_set(src->instr = MCgc_ptr);
		    src->info.gc.gcId    = GC_MOVED;
		    src->info.gc.heapItm = H2;
		    new_locn             = H2;
		    H2--;

		    assert(H1 < H2);
	       }
	  } else if (src->info.gc.gcId == GC_MOVED) {      /* item already collected */
	       V_assert(src->instr == MCgc_ptr);
	       new_locn = src->info.gc.heapItm;
	  } else if (src->info.gc.gcId > GC_MOVED) { /* it must be a constructor or macro frame */
	       V_assert(src->instr == MCcons ||   /* constructor */
			src->instr == MCldparm);  /* or a macro frame for functions */
	       V_set(H1->instr = src->instr);
	       H1->info        = src->info; 
	       new_locn        = H1;
	       H1++;

	       assert(H1 < H2);

	       V_set(src->instr = MCgc_ptr);

	       src->info.gc.gcId    = GC_MOVED;
	       src->info.gc.heapItm = new_locn;

	  } else if (src->info.gc.gcId == GC_BUILTIN) {     /* [BI] GARBAGE COLLECT BUILTINS */

	    V_set (H1->instr = src->instr);

	    H1->info = src->info;

	    new_locn = H1;
	    H1++;

	    assert (H1 < H2);

	    V_set (src->instr = MCgc_ptr);

	    src->info.gc.gcId    = GC_MOVED;
	    src->info.gc.heapItm = new_locn;

	  } else if (src->info.gc.gcId < GC_BUILTIN) {  /* records */     /* [BI] ALTERED (SEE machine_private.h) */

	       num = -(src->info.gc.gcId + 1);             /* [BI] ALTERED (SEE machine_private.h) */
	       V_assert(src->instr == MCldmacroframe);

	       new_locn = H1;
	       for (count = 0; count < num; count++) {
		    V_assert((src[count].instr == MCldmacroframe) ||
			     (count > 0 && 
			      ((src[count].instr == MCbclosure) ||
			      (src[count].instr == MCclosure))));
		    V_set(H1->instr = src[count].instr);
		    H1->info        = src[count].info;
		    new_locn1       = H1;
		    H1++;

		    assert(H1 < H2);

		    V_set(src[count].instr = MCgc_ptr);

		    src[count].info.gc.gcId    = GC_MOVED;
		    src[count].info.gc.heapItm = new_locn1;
  	       }
	  } else
	       printMsg(FATAL_MSG, "error");
	}
     return(new_locn);
}

/***************************************
 *                                     *
 *            gc_dump2                 *
 *                                     *
 ***************************************/
static
void
gc_dump2(D_INSTR *Dstack2, D_INSTR *D2ptr)
{
     D_INSTR *tmp = D2ptr;

     while (tmp < Dstack2) {
	  D_assert(tmp->instr == DPupdate);

	  if (tmp->info.gc) {
	       if (tmp->info.gc->info.gc.gcId == GC_MOVED)
		    tmp->info.gc = tmp->info.gc->info.gc.heapItm;
	       else
		    tmp->info.gc = NULL;
	  }
	  tmp++;
     }
     assert(tmp == Dstack2);
}

/***************************************
 *                                     *
 *            gc_dump3                 *
 *                                     *
 ***************************************/
static
void
gc_dump3(D_INSTR *Dstack3, D_INSTR *D3ptr)
{
     D_INSTR *tmp = D3ptr;

     while (tmp > Dstack3) {
	  D_assert(tmp->instr == DPreload);

	  tmp->info.reload = gc_move(tmp->info.reload);
	  tmp--;
     }
     assert(tmp == Dstack3);
}

/***************************************
 *                                     *
 *            gc_dump4                 *
 *                                     *
 ***************************************/
static
void
gc_dump4(D_INSTR *Dstack4, D_INSTR *D4ptr)
{
     D_INSTR *tmp = D4ptr;

     while (tmp < Dstack4) {
	  D_assert(tmp->instr == DPpr0 || tmp->instr == DPpr1);

	  tmp->info.gc = gc_move(tmp->info.gc);
	  tmp++;
     }
     assert(tmp == Dstack4);
}

/***************************************
 *                                     *
 *            gc_collect1              *
 *                                     *
 ***************************************/
static
V_INSTR 
*gc_collect1(V_INSTR *start)
{
     V_INSTR *ptr   = start;
     int      num   = 0;
     int      count = 0;

     while (ptr < H1) {
	  if (ptr->info.gc.gcId < GC_BUILTIN) {  /* records */     /* [BI] ALTERED (SEE machine_private.h) */
	       V_assert(ptr->instr == MCldmacroframe);

	       ptr->info.recMacroFrame.frame = gc_move(ptr->info.recMacroFrame.frame);

	       num = GC_BUILTIN - ptr->info.gc.gcId;  /* less macro frame */     /* [BI] ALTERED (SEE machine_private.h) */
	       for (count = 0; count < num; count++) {
		    ptr++;
		    V_assert((ptr->instr == MCbclosure) ||
			     (ptr->instr == MCclosure));
		    ptr->info.closure.v = gc_move(ptr->info.closure.v);
  	       }
	  } else if (ptr->info.gc.gcId == GC_BUILTIN)     /* [BI] GARBAGE COLLECT BUILTINS (DO NOTHING) */
	    ;
	  else { /* it must be a constructor or macro frame */
	    V_assert(ptr->instr == MCcons ||
		     ptr->instr == MCldparm);
	    ptr->info.gc.heapItm = gc_move(ptr->info.gc.heapItm);
	  }
	  ptr++;
     }
     assert(ptr == H1);
     return(ptr);
}

/***************************************
 *                                     *
 *            gc_collect2              *
 *                                     *
 ***************************************/
static
V_INSTR 
*gc_collect2(V_INSTR *start)
{
     V_INSTR *ptr   = start;
     int      num   = 0;
     int      count = 0;

     while (ptr > H2) {
	  V_assert(ptr->instr == MCpair);

	  ptr->info.pair.v0 = gc_move(ptr->info.pair.v0);
	  ptr->info.pair.v1 = gc_move(ptr->info.pair.v1);
	  ptr--;
     }
     assert(ptr == H2);
     return(ptr);
}

/***************************************
 *                                     *
 *            gc_collect               *
 *                                     *
 ***************************************/
static
void
gc_collect(void)
{
     V_INSTR *current1 = HeapStore;
     V_INSTR *current2 = HeapStore + Hp_size - 1; 

     while ( (current1 < H1) || (current2 > H2) ) {
	  current1 = gc_collect1(current1);
	  current2 = gc_collect2(current2);
     }
     assert(current1 == H1);
     assert(current2 == H2);
}

/***************************************
 *                                     *
 *            gc                       *
 *                                     *
 ***************************************/
void
gc(int heapRequested)
{
#if gcDebug
     static int    cellsused    = 0;
     static int    memused      = 0;
#endif

     oldHeap2Top = H2;
     oldHeap2Bot = HeapStore + Hp_size - 1;

     if (extraHeapOn == BTRUE) {

	  Hp_size = Hp_size * 2;
	  extraHeapHD = MemAlloc("extra heap", Hp_size, sizeof(V_INSTR));
	  extraHeap = (V_INSTR *) MemHeapAlloc(extraHeapHD, Hp_size, sizeof(V_INSTR));
	  HeapStore = extraHeap;
#if gcDebug
	  printMsg(MSG, "Increasing heap to %dk\n",(Hp_size*sizeof(V_INSTR))/1024); 
#endif
     } else {
	  if (HeapStore == Hp1)
	       HeapStore = Hp2;
	  else {
	       assert(HeapStore == Hp2);
	       HeapStore = Hp1;
	  }
     }
     H1 = HeapStore;  
     H2 = HeapStore + Hp_size - 1;

#if gcDebug
     printf("Garbage Collection...");
#endif

     A = gc_move(A);
     V = gc_move(V);

     gc_closures();

     gc_dump3(Dstack3_4, D3);               /* macro frames */
     gc_dump4(Dstack3_4 + Dstack_size, D4); /* records/env  */

     gc_collect();

     gc_dump2(Dstack1_2 + Dstack_size, D2); /* updates      */

     /* not quite right */

#if gcDebug
     cellsused = (Hp_size - (int) (H2 - H1)) / sizeof(V_INSTR);
     memused = (int) (((double) cellsused / (double) Hp_size) * 100.0);
     printf("%d/%d cells, %d%% used, Done\n", cellsused, Hp_size, memused);
#endif

     if (extraHeapOn == BTRUE) {
	  MemDealloc(Hp1HeapDesc);
	  MemDealloc(Hp2HeapDesc);
	  Hp1HeapDesc = extraHeapHD;
	  Hp1         = HeapStore;
	  Hp2HeapDesc = MemAlloc("gc Hp2", Hp_size, sizeof(V_INSTR));
	  Hp2         = (V_INSTR *) MemHeapAlloc(Hp2HeapDesc, Hp_size, sizeof(V_INSTR));
	  extraHeapOn = BFALSE;
	  extHeapTop  = NULL;
	  extHeapBot  = NULL;
	  extraHeapHD = -1;
	  extraHeap   = NULL;
     }

     if ((H1 + heapRequested) >= H2) {
	  extraHeapOn = BTRUE;
	  extHeapBot = HeapStore + Hp_size - 1;
	  extHeapTop = H2;

	  if (HeapStore == Hp1)
	       HeapStore = Hp2;
	  else {
	       assert(HeapStore == Hp2);
	       HeapStore = Hp1;
	  }

	  H1 = HeapStore;
	  H2 = HeapStore + Hp_size - 1;
	  assert(H1 + heapRequested < H2);
     }

     assert(H1 + heapRequested < H2);
     assert(H1);
     assert(H2);
}
