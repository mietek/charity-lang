/******************************************************************************
 *                                                                            *
 *   machine.c                                                                *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include <stdio.h>
#include <assert.h>
#include "pmem.h"
#include "machine.h"
#include "machine_private.h"
#include "decompile.h"
#include "gc.h"
#include "emit.h"
#include "ioChar.h"

#define mcDebug 0
#define sDebug  0

#if sDebug
#define disp(s) printf(s);
#define disp1(s, a) printf(s, a);
#else
#define disp(ignore) ((void) 0);
#define disp1(ignore,ignore1) ((void) 0);
#endif

/*****************************************************************************/
/* Define the constants -- initial heap and stack sizes */

#define INIT_HP_SIZE         10000
#define INIT_D_STACK_SIZE    25000

/*****************************************************************************/
/* pushing and popping items onto and off of one of the 4 dump stacks        */

#define DSTACK_OVERFLOW_CHK1 if (D2 <= D1) {printMsg(ERROR_MSG, "D(1/2) Stack Overflow.");}
#define DSTACK_OVERFLOW_CHK2 if (D4 <= D3) {printMsg(ERROR_MSG, "D(3/4) Stack Overflow.");}

#if DDEBUG
#define pushD1(x) D1++; DSTACK_OVERFLOW_CHK1 ; D1->instr = x;
#define pushD2(x) D2--; DSTACK_OVERFLOW_CHK1 ; D2->instr = x;
#define pushD3(x) D3++; DSTACK_OVERFLOW_CHK2 ; D3->instr = x;
#define pushD4(x) D4--; DSTACK_OVERFLOW_CHK2 ; D4->instr = x;

#else
#define pushD1(x) D1++; DSTACK_OVERFLOW_CHK1
#define pushD2(x) D2--; DSTACK_OVERFLOW_CHK1
#define pushD3(x) D3++; DSTACK_OVERFLOW_CHK2
#define pushD4(x) D4--; DSTACK_OVERFLOW_CHK2

#endif

#define popD1()   D1--;
#define popD2()   D2++;
#define popD3()   D3--;
#define popD4()   D4++;

/* allocating heap items */
#define allocH1(n,R) if ((H1+n) >= H2) { gc(n); } R = H1; H1 = H1 + n; assert(R);
#define allocH2(n,R) if ((H2-n) <= H1) { gc(n); } R = H2; H2 = H2 - n; assert(R);

/*****************************************************************************
 *                                                                           *
 *                           internal prototypes                             *
 *                                                                           *
 *****************************************************************************/
static int   _Machine(V_INSTR **value);

/*****************************************************************************
 *                                                                           *
 *                           external accesible variables                    *
 *                                                                           *
 *****************************************************************************/
V_INSTR *_BANG    = NULL;

V_INSTR *HeapStore;    /* Heap */
V_INSTR *H1;           /* constructors, records, macroframes, builtins */
V_INSTR *H2;           /* pairs                                        */
int      Hp_size;

D_INSTR *Dstack1_2;    /* Acutal Dump split into 2 dump stacks */
D_INSTR *D1;           /* code continuations                   */
D_INSTR *D2;           /* updates of records                   */

D_INSTR *Dstack3_4;    /* Acutal Dump split into 2 dump stacks */
D_INSTR *D3;           /* macro frames                         */
D_INSTR *D4;           /* pr0, pr1 of products                 */
int      Dstack_size;

V_INSTR *V;
V_INSTR *A;

V_INSTR *Hp1;
V_INSTR *Hp2;

MEMORY Hp1HeapDesc  = -1;
MEMORY Hp2HeapDesc  = -1;

static M_INSTR _RET     = {MCret};
static M_INSTR _HALT    = {MChalt};

static MEMORY bangHD    = -1;

static MEMORY Dstack1_2HD;
static MEMORY Dstack3_4HD;

static M_INSTR *PC;
static V_INSTR *R0;
static V_INSTR *R1;

/***************************************
 *                                     *
 *            MachineReset             *
 *                                     *
 ***************************************/
void
MachineReset(void)
{
     V       = NULL;
     A       = NULL;
     R0      = NULL;
     R1      = NULL;

     D1  = Dstack1_2;
     D2  = Dstack1_2 + Dstack_size;

     D3  = Dstack3_4;
     D4  = Dstack3_4 + Dstack_size;

     H1 = HeapStore;
     H2 = HeapStore + Hp_size - 1;
}

/***************************************
 *                                     *
 *            _Machine                 *
 *                                     *
 ***************************************/
static
int
_Machine(V_INSTR **value)
{
     int    machineOps = 0;
     int    num        = 0;

     while (PC->instr != MChalt) {

	  switch (PC->instr) {
	     case MChalt:
	       disp("MChalt\n"); 
	       break;
	     case MCret:
	       disp("MCret\n");
	       D_assert(D1->instr == DPcont); 

	       PC = D1->info.code;
	       popD1();
	       break;
	     case MCgoto:
	       disp("MCgoto\n");
	       PC = PC->info.code;
	       break;
             case MCsave:
               disp("MCsave\n");

               pushD4(DPpr0);
               D4->info.pr0 = V;
               PC++;
               break;
             case MCswap:
               disp("MCswap\n")
               D_assert(D4->instr == DPpr0);
               R0           = D4->info.pr0;
               D_set(D4->instr = DPpr1);

               D4->info.pr1 = V;
               V            = R0;
               PC++;
               break;
             case MCpair:
               disp("MCpair\n");
	       D_assert(D4->instr == DPpr0 || D4->instr == DPpr1); 

               allocH2(1, R0);
               V_set(R0->instr = MCpair);
               R0->info.pair.v0 = V;
               R0->info.pair.v1 = D4->info.pr1;
               V                = R0;
               popD4();
               PC++;
               break;
            case MCp0:
               disp("P0\n");
               V_assert(V->instr == MCpair);

               V = V->info.pair.v0;
               PC++;
               break;
             case MCp1:
               disp("P1\n");
               V_assert(V->instr == MCpair);

               V = V->info.pair.v1;
               PC++;
               break;
             case MCmap_prod:
               disp("map_prod\n");
               V_assert(V->instr == MCpair);
               allocH2(1, R0);
	       V_set(R0->instr = MCpair);

               R0->info.pair.v1 = V->info.pair.v1;   /* <_, v2>                         */
               R1 = V->info.pair.v0;                 /* <v0, v1>                        */

               V_assert(R1->instr == MCpair);
               R0->info.pair.v0 = R1->info.pair.v0;  /* build <v0, v2>                  */
               
               pushD4(DPpr0);
               D4->info.pr0  = R0;

               allocH2(1, R0);
               V_set(R0->instr = MCpair);            /* build <v1, v2>                  */
               R1 = V->info.pair.v0;                 /* <v0, v1>                        */
               R0->info.pair.v0 = R1->info.pair.v1;  /* <v1, _>                         */
               R0->info.pair.v1 = V->info.pair.v1;   /* <v1, v2>                        */

               V = R0;                               /* set V to point to <v1, v2>      */
               PC++;
               break;
             case MCpr:
               disp("pr\n");
               V_assert(V->instr == MCpair);

               pushD4(DPpr1);
               D4->info.pr1 = V->info.pair.v1;     /* pr1(v1, c)                      */
               PC++;
               break;
	     case MCfunc:
	       disp("FUNC\n");

	       pushD1(DPcont);
	       D1->info.code = PC + 1;

	       PC               = PC->info.funcCode;
	       break;
             case MCparm: 
               disp1("`%d\n",PC->info.macroParm);

               pushD1(DPcont);
               D1->info.code = PC + 1;

               pushD3(DPreload);
               D3->info.reload = A;
               
	       V_assert(A->info.macroFrame.arg);
               PC = A->info.macroFrame.arg[PC->info.macroParm];  
               A  = A->info.macroFrame.prev;
               break;
             case MCldparm:
               disp("MCldparm\n");

	       V_assert(PC->info.macroList);

	       allocH1(1, R0);
	       V_set(R0->instr = MCldparm);
               R0->info.macroFrame.prev = A;
               R0->info.macroFrame.arg  = PC->info.macroList;
               A                        = R0;

               PC++;
               break;
             case MCreload:
               disp("MCreload\n");
               D_assert(D3->instr == DPreload); 
               A = D3->info.reload;
               popD3();
               PC++;
	       break;
             case MCunload:
               disp("MCunload\n");

               A = A->info.macroFrame.prev;
               PC++;
               break;
	     case MCbang:
	       disp("!\n");
	       V = _BANG;
	       PC++;
	       break;
	     case MCcons:       /* Build the cons structure in the Heap */
	       disp1("cons{%d}\n", PC->info.structorPosn);

	       allocH1(1, R0);
	       V_set(R0->instr = PC->instr);

	       R0->info.structor.posn = PC->info.structorPosn;
	       R0->info.structor.next = V;
	       V                      = R0;
	       PC++;
	       break;
	     case MCjump:
	       disp("jump\n");

	       pushD1(DPcont);
	       D1->info.code = PC + 1;

	       PC               = PC->info.code;
	       break;

	     case MCjumpc:                                 /* [#@]                    */
	       disp ("jump?...");

	       V_assert (V->instr == MCpair);

	       if (V->info.pair.v0->info.at.next &&        /* AT TAG?                 */
		   V->info.pair.v0->info.at.posn == 0)
		 {
		   disp ("no\n");                          /* YES---DON'T JUMP        */
		   V = V->info.pair.v0->info.at.next;      /*    ...AND STRIP THE TAG */

		   PC++;
		 }
	       else
		 {
		   disp ("yes\n");                         /* NO--- JUMP              */

		   pushD1 (DPcont);
		   D1->info.code = PC + 1;

		   PC = PC->info.code;
		 }

	       break;

	     case MCinduct:
	       disp("induct\n");

	       pushD1(DPcont);
               D1->info.code = PC + 1;

               V_assert(V->instr == MCpair);
               allocH2(1, R0);
               R1 = V->info.pair.v0;

               V_assert(R1->instr == MCcons);
               V_set(R0->instr = MCpair);

               R0->info.pair.v1 = V->info.pair.v1;
               R0->info.pair.v0 = R1->info.structor.next;
               V                = R0;

               PC = PC->info.inductCode;
               PC = PC + (R1->info.structor.posn - 1);     /* [#@] */

	       break;
             case MCalloc:
               disp1("alloc{%d}\n", PC->info.allocNum);
               allocH1(PC->info.allocNum, R0);
               PC++;
               break;
	     case MCldmacroframe:  /* load the current macro frame into the record */
	       disp("ldmacroframe\n");

	       V_set(R0->instr = MCldmacroframe);

	       R0->info.recMacroFrame.frame = A;
	       R0->info.recMacroFrame.gcId  = PC->info.numDestrs;
               R1 = V;
	       V = R0;
	       R0++;
	       PC++;
	       break;
             case MCbclosure:
               disp("bclosure\n");

               V_set(R0->instr = MCbclosure);
               R0->info.closure.v              = R1;
               R0->info.closure.c              = PC->info.closureCode;
               PC++;
               break;
             case MCclosure:
               disp("closure\n");

               R0++;
               V_set(R0->instr = MCclosure);

               R0->info.closure.v              = R1;
               R0->info.closure.c              = PC->info.closureCode;
               PC++;
               break;          
             case MCdest:
               disp1("dest(%d)\n",PC->info.structorPosn);

	       pushD3(DPreload);
	       D3->info.reload = A;

               pushD1(DPcont);
               D1->info.code = PC + 1;

               V_assert(V->instr == MCldmacroframe); 
               R1 = V;
               R0 = V + PC->info.structorPosn;

               V_assert((R0->instr == MCclosure) || (R0->instr == MCbclosure));
	       A  = V->info.recMacroFrame.frame;

               V  = R0->info.closure.v;
               PC = R0->info.closure.c;
               break;

/*
 * [H-O] ADDED THIS CASE/INSTRUCTION FOR H-O DESTRUCTORS (IE. WE NOW CAN
 *       DESTRUCT WITH INPUT):
 *
 */

             case MCdestHO:
               disp1 ("destHO(%d)\n", PC->info.structorPosn);

	       pushD3 (DPreload);
	       D3->info.reload = A;

               pushD1 (DPcont);
               D1->info.code = PC + 1;

	       V_assert (V->instr == MCpair);

	       allocH2 (1, R0);
	       V_set (R0->instr = MCpair);

               R0->info.pair.v0 = V->info.pair.v0;
	       R1               = V->info.pair.v1;
	       V                = R0;

	       V_assert (R1->instr == MCldmacroframe);

	       A  = R1->info.recMacroFrame.frame;
	       R0 = R1 + PC->info.structorPosn;

	       V_assert (   (R0->instr == MCclosure)
			 || (R0->instr == MCbclosure));

	       V->info.pair.v1 = R0->info.closure.v;
	       PC              = R0->info.closure.c;

               break;

             case MCmkupdate:
               disp("mkupdate\n");

               V_assert((R1[1].instr == MCclosure) || (R1[1].instr == MCbclosure));
               pushD2(DPupdate);
               D2->info.update = R1;

               PC++;
               break;
             case MCupdate:
               disp("update\n");
               D_assert(D2->instr == DPupdate); 

	       if (D2->info.update) {  /* see if we need to do the update */
		    R0 = D2->info.update + PC->info.updateClo;

		    V_assert((R0->instr == MCclosure) || (R0->instr == MCbclosure));
		    R0->info.closure.v = V;
		    R0->info.closure.c = &_RET;
	       }
	       popD2();

	       D_assert(D1->instr == DPcont); 

	       PC = D1->info.code;
               popD1();
               break;

	     case MCint:     /* [BI] BUILTIN INTEGERS */

	       disp1 ("int{%d}\n", PC->info.i);

	       allocH1 (1, V);

	       V_set (V->instr = PC->instr);

	       V->info.integer.i    = PC->info.i;
	       V->info.integer.gcId = GC_BUILTIN;

	       PC++;

	       break;

	     case MCchar:     /* [BI] BUILTIN CHARACTERS */

	       disp1 ("char{%d}\n", (int)PC->info.c);

	       allocH1 (1, V);

	       V_set (V->instr = PC->instr);

	       V->info.character.c    = (int)PC->info.c;
	       V->info.character.gcId = GC_BUILTIN;

	       PC++;

	       break;

	     case MCadd:     /* [BI] BUILTIN ADDITION */

	       disp ("add\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCint);

	       R0->info.integer.i    = V->info.pair.v0->info.integer.i + V->info.pair.v1->info.integer.i;
	       R0->info.integer.gcId = GC_BUILTIN;

	       V = R0;

	       PC++;

	       break;

	     case MCsub:     /* [BI] BUILTIN SUBTRACTION */

	       disp ("sub\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCint);

	       R0->info.integer.i    = V->info.pair.v0->info.integer.i - V->info.pair.v1->info.integer.i;
	       R0->info.integer.gcId = GC_BUILTIN;

	       V = R0;

	       PC++;

	       break;

	     case MCmul:     /* [BI] BUILTIN MULTIPLICATION */

	       disp ("mul\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCint);

	       R0->info.integer.i    = V->info.pair.v0->info.integer.i * V->info.pair.v1->info.integer.i;
	       R0->info.integer.gcId = GC_BUILTIN;

	       V = R0;

	       PC++;

	       break;

	     case MCdiv:     /* [BI] BUILTIN DIVISION */

	       disp ("div\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCint);

	       R0->info.integer.i    = V->info.pair.v0->info.integer.i / V->info.pair.v1->info.integer.i;
	       R0->info.integer.gcId = GC_BUILTIN;

	       V = R0;

	       PC++;

	       break;

	     case MCmod:     /* [BI] BUILTIN MODULUS OPERATION (INTEGER DIVISION REMAINDER) */

	       disp ("mod\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCint);

	       R0->info.integer.i    = V->info.pair.v0->info.integer.i % V->info.pair.v1->info.integer.i;
	       R0->info.integer.gcId = GC_BUILTIN;

	       V = R0;

	       PC++;

	       break;

	     case MClt_int:     /* [BI] [#@] BUILTIN LESS-THAN FOR INTEGERS */

	       disp ("lt_int\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCcons);

	       R0->info.structor.posn = (V->info.pair.v0->info.integer.i < V->info.pair.v1->info.integer.i) + 1;
	       R0->info.structor.next = NULL;

	       V = R0;

	       PC++;

	       break;

	     case MCle_int:     /* [BI] [#@] BUILTIN LESS-THAN-EQUAL-TO FOR INTEGERS */

	       disp ("le_int\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCcons);

	       R0->info.structor.posn = (V->info.pair.v0->info.integer.i <= V->info.pair.v1->info.integer.i) + 1;
	       R0->info.structor.next = NULL;

	       V = R0;

	       PC++;

	       break;

	     case MCgt_int:     /* [BI] [#@] BUILTIN GREATER-THAN FOR INTEGERS */

	       disp ("gt_int\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCcons);

	       R0->info.structor.posn = (V->info.pair.v0->info.integer.i > V->info.pair.v1->info.integer.i) + 1;
	       R0->info.structor.next = NULL;

	       V = R0;

	       PC++;

	       break;

	     case MCge_int:     /* [BI] [#@] BUILTIN GREATER-THAN-EQUAL-TO FOR INTEGERS */

	       disp ("ge_int\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCcons);

	       R0->info.structor.posn = (V->info.pair.v0->info.integer.i >= V->info.pair.v1->info.integer.i) + 1;
	       R0->info.structor.next = NULL;

	       V = R0;

	       PC++;

	       break;

	     case MCeq_int:     /* [BI] [#@] BUILTIN EQUALITY FOR INTEGERS */

	       disp ("eq_int\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCcons);

	       R0->info.structor.posn = (V->info.pair.v0->info.integer.i == V->info.pair.v1->info.integer.i) + 1;
	       R0->info.structor.next = NULL;

	       V = R0;

	       PC++;

	       break;

	     case MClt_char:     /* [BI] [#@] BUILTIN LESS-THAN FOR CHARACTERS */

	       disp ("lt_char\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCcons);

	       R0->info.structor.posn = (V->info.pair.v0->info.character.c < V->info.pair.v1->info.character.c) + 1;
	       R0->info.structor.next = NULL;

	       V = R0;

	       PC++;

	       break;

	     case MCle_char:     /* [BI] [#@] BUILTIN LESS-THAN-EQUAL-TO FOR CHARACTERS */

	       disp ("le_char\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCcons);

	       R0->info.structor.posn = (V->info.pair.v0->info.character.c <= V->info.pair.v1->info.character.c) + 1;
	       R0->info.structor.next = NULL;

	       V = R0;

	       PC++;

	       break;

	     case MCgt_char:     /* [BI] [#@] BUILTIN GREATER-THAN FOR CHARACTERS */

	       disp ("gt_char\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCcons);

	       R0->info.structor.posn = (V->info.pair.v0->info.character.c > V->info.pair.v1->info.character.c) + 1;
	       R0->info.structor.next = NULL;

	       V = R0;

	       PC++;

	       break;

	     case MCge_char:     /* [BI] [#@] BUILTIN GREATER-THAN-EQUAL-TO FOR CHARACTERS */

	       disp ("ge_char\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCcons);

	       R0->info.structor.posn = (V->info.pair.v0->info.character.c >= V->info.pair.v1->info.character.c) + 1;
	       R0->info.structor.next = NULL;

	       V = R0;

	       PC++;

	       break;

	     case MCeq_char:     /* [BI] [#@] BUILTIN EQUALITY FOR CHARACTERS */

	       disp ("eq_char\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCcons);

	       R0->info.structor.posn = (V->info.pair.v0->info.character.c == V->info.pair.v1->info.character.c) + 1;
	       R0->info.structor.next = NULL;

	       V = R0;

	       PC++;

	       break;

	     case MCcode:     /* [BI] BUILTIN CHARACTER-TO-ASCII-CODE CONVERSION */

	       disp ("code\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCint);

	       R0->info.integer.i    = (int)V->info.character.c;
	       R0->info.integer.gcId = GC_BUILTIN;

	       V = R0;

	       PC++;

	       break;

	     case MCdecode:     /* [BI] BUILTIN ASCII-CODE-TO-CHARACTER CONVERSION */

	       disp ("decode\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = MCchar);

	       R0->info.character.c    = (char)V->info.integer.i;
	       R0->info.character.gcId = GC_BUILTIN;

	       V = R0;

	       PC++;

	       break;

	     case MCat:     /* [#@] */

	       disp ("@\n");

	       allocH1 (1, R0);

	       V_set (R0->instr = PC->instr);

	       R0->info.at.next = V;
	       R0->info.at.posn = 0;

	       V = R0;

	       PC++;

	       break;

	     case MCinvalid:
	       printMsg(FATAL_MSG, "invalid instruction in machine.");
	       break;
	     default:
	       printMsg(FATAL_MSG, "invalid machine instruction.");
	       break;
	  }
	}
     *value = V;
     return(machineOps);
}


/***************************************
 *                                     *
 *             Evaluate                *
 *                                     *
 ***************************************/
COMB_EXPR
*Evaluate(COMB_EXPR *expr, ST_TYPE *resultType)
{
     V_INSTR         *value        = NULL;
     int              machineOps   = 0;
     CLOSURE         *closure      = NULL;
     COMB_EXPR       *result       = NULL;
     ST_TYPE         *type         = resultType;

#if mcDebug
     printMsg(MSG, "Machine: Evaluating combinator expression...");
#endif

     if (expr->tag == CTT_CLOSURE) {
	  closure = getClosure(expr->info.closure);

	  V      = closure->clo->info.closure.v;
	  PC     = closure->clo->info.closure.c;
	  A      = closure->rec->info.recMacroFrame.frame;
	  R1     = closure->rec;

	  pushD1(DPcont);
	  D1->info.code = &_HALT;

	  machineOps = _Machine(&value);
	  type = closure->type;
     }
     else {
	  MachineReset();
	  PC         = _CompileHalt(expr);
	  machineOps = _Machine(&value);
     }

#if mcDebug
     printf("Done\n");
#endif

     if (value)
	  result = deCompile(value, type);
     else /* here we have a bang */
	  result = deCompile(_BANG, type);

     return(result);
}

/***************************************
 *                                     *
 *        mc_MachineOpen               *
 *                                     *
 ***************************************/
void
mc_MachineOpen(void)
{
     Dstack_size  = INIT_D_STACK_SIZE;
}

/***************************************
 *                                     *
 *        mc_MachineClose              *
 *                                     *
 ***************************************/
void
mc_MachineClose(void)
{
}

/***************************************
 *                                     *
 *        mc_MachineConstruct          *
 *                                     *
 ***************************************/
void
mc_MachineConstruct(void)
{
     Dstack1_2HD  = MemAlloc("machine D stack 1/2", INIT_D_STACK_SIZE, sizeof(D_INSTR));
     Dstack1_2    = (D_INSTR *) MemHeapAlloc(Dstack1_2HD, INIT_D_STACK_SIZE, sizeof(D_INSTR));

     Dstack3_4HD  = MemAlloc("machine D stack 3/4", INIT_D_STACK_SIZE, sizeof(D_INSTR));
     Dstack3_4    = (D_INSTR *) MemHeapAlloc(Dstack3_4HD, INIT_D_STACK_SIZE, sizeof(D_INSTR));

     bangHD = MemAlloc("machine bang", 1, sizeof(V_INSTR));
     _BANG = (V_INSTR *) MemHeapAlloc(bangHD, 1, sizeof(V_INSTR));
     V_set(_BANG->instr = MCbang);
     _BANG->info.bang.bangId1   = NULL;
     _BANG->info.bang.bangId2   = NULL;

     deCompileConstruct();

     Hp_size      = INIT_HP_SIZE;
     Hp1HeapDesc  = MemAlloc("machine hp1", Hp_size, sizeof(V_INSTR));
     Hp1          = (V_INSTR *) MemHeapAlloc(Hp1HeapDesc, Hp_size, sizeof(V_INSTR));

     Hp2HeapDesc  = MemAlloc("machine hp2", Hp_size, sizeof(V_INSTR));
     Hp2          = (V_INSTR *) MemHeapAlloc(Hp2HeapDesc, Hp_size, sizeof(V_INSTR));

     HeapStore    = Hp1;
}

/***************************************
 *                                     *
 *        mc_MachineDestruct           *
 *                                     *
 ***************************************/
void
mc_MachineDestruct(void)
{
     deCompileDestruct();
     MemDealloc(Hp1HeapDesc);
     MemDealloc(Hp2HeapDesc);
     MemDealloc(Dstack1_2HD);
     MemDealloc(Dstack3_4HD);
}
