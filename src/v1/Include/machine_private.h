/******************************************************************************
 *                                                                            *
 *   machine_private.h                                                        *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __MACHINE_PRIVATE__
#define __MACHINE_PRIVATE__

#define GC_MOVED   -1     /*      IDENTIFIES MOVED VALUES   FOR THE GARABGE COLLECTOR */
#define GC_BUILTIN -2     /* [BI] IDENTIFIES BUILTIN VALUES FOR THE GARBAGE COLLECTOR */

#define BANG -1

#define VDEBUG 0     /* FOR DEBUGGING THE HEAPS       */
#define DDEBUG 0     /* FOR DEBUGGING THE DUMP STACKS */

#if VDEBUG
#define V_assert(x) assert(x)
#define V_set(x)    x;
#else
#define V_assert(ignore) ;
#define V_set(ignore)    ;
#endif

#if DDEBUG
#define D_assert(x) assert(x)
#define D_set(x)    x;
#else
#define D_assert(ignore) ;
#define D_set(ignore)    ;
#endif


/**************************
 *                        *
 *     TYPES              *
 *                        *
 **************************/

/* HEAP CELLS (VALUES): */

typedef struct _V_INSTR
{

#if VDEBUG
  M_INSTR_TAG instr;
#endif

  union
    {
      struct                         /* [#@]        */
	{
	  struct _V_INSTR *next;
	  int              posn;     /* HAS VALUE 0 */
	}
      at;

      struct            /* [BI] BUILTIN INTEGERS               */
	{
	  int i;        /*      THE INTEGER                    */
	  int gcId;     /*      HAS VALUE OF GC_BUILTIN        */
	}
      integer;

      struct            /* [BI] BUILTIN CHARACTERS             */
	{
	  int c;        /*      THE CHARACTER (CAST AS AN int) */
	  int gcId;     /*      HAS VALUE OF GC_BUILTIN        */
	}
      character;

      struct     /* CONSTRUCTORS */
	{
	  struct _V_INSTR *next;
	  int              posn;     /* [#@] THE POSITION OF THE CONSTRUCTOR + 1 */
	}
      structor;

      struct     /* RECORD CLOSURES */
	{
	  struct _V_INSTR *v;
	  M_INSTR         *c;
	}
      closure;

      struct                          /* RECORDS */
	{
	  struct _V_INSTR *frame;     /* CURRENT MACRO FRAME                    */
	  int              gcId;      /* NUMBER OF CLOSURES AS NEGATIVE INT + 2 */
	}
      recMacroFrame;

      struct     /* MACRO FRAMES */
	{
	  struct _V_INSTR  *prev;
	  struct _M_INSTR **arg;
	}
      macroFrame;

      struct     /* 0-TUPLES */
	{
	  int bangId1;     /* UNUSED */
	  int bangId2;     /* UNUSED */
	}
      bang;

      struct     /* 2-TUPLES */
	{
	  struct _V_INSTR *v0;
	  struct _V_INSTR *v1;
	}
      pair;

      struct     /* USED BY GARGAGE COLLECTOR ONLY */
	{
	  struct _V_INSTR *heapItm;
	  int              gcId;
	}
      gc;
    }
  info;
}
V_INSTR;


/* DUMP STACK CELLS: */

#if DDEBUG
typedef enum
{
  DPboundary,
  DPenv     ,
  DPcont    ,
  DPreload  ,
  DPupdate  ,
  DPpr0     ,
  DPpr1
}
D_INSTR_TAG;
#endif

typedef struct _D_INSTR
{

#if DDEBUG
  D_INSTR_TAG instr;
#endif

  union
    {
      M_INSTR *code;       /* returns from jump, etc         */
      V_INSTR *env;        /* propagating the environment    */
      V_INSTR *reload;     /* reloading a macro frame        */
      V_INSTR *update;     /* updating a closure in a record */
      V_INSTR *pr0;        /* creating a pair                */
      V_INSTR *pr1;        /* creating a pair                */
      V_INSTR *gc;         /* garbage collecting the dump    */
    }
  info;
}
D_INSTR;


/**************************
 *                        *
 *     VARIABLES          *
 *                        *
 **************************/

extern MEMORY Hp1HeapDesc;
extern MEMORY Hp2HeapDesc;

extern V_INSTR *Hp1;
extern V_INSTR *Hp2;

extern V_INSTR *HeapStore;

extern V_INSTR *V;
extern V_INSTR *A;

extern V_INSTR *H1;
extern V_INSTR *H2;

extern D_INSTR *Dstack1_2;
extern D_INSTR *Dstack3_4;

extern D_INSTR *D1;
extern D_INSTR *D2;
extern D_INSTR *D3;
extern D_INSTR *D4;

extern V_INSTR *_BANG;

extern int Hp_size;
extern int Astack_size;
extern int Dstack_size;

#endif
