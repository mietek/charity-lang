/******************************************************************************
 *                                                                            *
 *   emit.c                                                                   *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
 
#include <stdio.h>
#include <stdlib.h>
#include "ioChar.h"
#include "codetab.h"
#include "emit.h"
#include "pmem.h"

#define DEBUG        0

#define demit        0
#define MAXCODE      50000
#define MAXPARM      1000
#define EMITOUTFILE  "code.chx"
/*****************************************************************************
 *                                                                           *
 *                           internal prototypes                             *
 *                                                                           *
 *****************************************************************************/
static void Instr2str(void);

/*****************************************************************************
 *                                                                           *
 *                           internal variables                              *
 *                                                                           *
 *****************************************************************************/

static FILE     *outFile       = NULL;
static MEMORY    emitHeapDesc;       
static M_INSTR  *codeStore     = NULL;
static int       codePtr       = 0;

static MEMORY    parmHD;

M_INSTR emitCode;


/***************************************
 *                                     *
 *            EmitSet                  *
 *                                     *
 ***************************************/
void
EmitSet(EMIT addr)
{
     codePtr = addr;
}

/***************************************
 *                                     *
 *            EmitInc                  *
 *                                     *
 ***************************************/
M_INSTR
**EmitParm(int num)
{
     return((M_INSTR **) MemHeapAlloc(parmHD, num+1, sizeof(M_INSTR *)));
}


/***************************************
 *                                     *
 *            EmitIndex                *
 *                                     *
 ***************************************/
EMIT
EmitIndex(void)
{
     return(codePtr);
}


/***************************************
 *                                     *
 *            EmitReset                *
 *                                     *
 ***************************************/
void
EmitReset(void)
{
     codePtr = 0;
     MemReset(parmHD);
#if DEBUG
     fclose(outFile);
     outFile = fopen(EMITOUTFILE, "w");
#endif
}

/***************************************
 *                                     *
 *            EmitStart                *
 *                                     *
 ***************************************/
struct _M_INSTR *
EmitStart(void)
{
     return(codeStore);
}

#if demit
static
void
Instr2str(void)
{
#if DEBUG
/*     fprintf(outFile, "%d\t",codePtr); */
/*     fprintf(outFile, "\t"); */
     switch (emitCode.instr) {
	case MCparm:
	  fprintf(outFile, "\tMCparm(%d)\n", emitCode.info.macroParm);
	  break;
	case MCbang:
	  fprintf(outFile, "\tMCbang\n");
	  break;
	case MCcons:
	  fprintf(outFile, "\tMCcons(%d)\n", emitCode.info.structorPosn);
	  break;
	case MCfunc:
	  fprintf(outFile, "\tMCfunc(???)\n"); 
	  break;
	case MCinduct:
	  fprintf(outFile, "\tMCinduct\n");
	  break;
	case MCret:
	  fprintf(outFile, "\tMCret\n");
	  break;
	case MCunload:
	  fprintf(outFile, "\tMCunload\n");
	  break;
	case MCreload:
	  fprintf(outFile, "\tMCreload\n");
	  break;
	case MChalt:
	  fprintf(outFile, "\tMChalt\n");
	  break;
	case MCldparm:
	  fprintf(outFile, "\tMCldparm\n");
	  break;
	case MCalloc:
	  fprintf(outFile, "\tMCalloc\n");
	  break;
	case MCdest:
  	  fprintf(outFile, "\tMCdest(%d)\n", emitCode.info.structorPosn);
	  break;
	case MCbclosure:
	  fprintf(outFile, "\tMCbclosure\n");
	  break;
	case MCclosure:
	  fprintf(outFile, "\tMCclosure\n");
	  break;
	case MCmkupdate:
	  fprintf(outFile, "\tMCmkupdate\n");
	  break;
	case MCupdate:
	  fprintf(outFile, "\tMCupdate\n");
	  break;
	case MCjump:
	  fprintf(outFile, "\tMCjump\n");
	  break;
	case MCgoto:
	  fprintf(outFile, "\tMCgoto\n");
	  break;
	case MCldmacroframe:
	  fprintf(outFile, "\tMCldmacroframe\n");
	  break;
	case MCsaveenv:
	  fprintf(outFile, "\tMCsaveenv\n");
	  break;
	default:
	  printMsg(FATAL_MSG, "Error in Instr2str\n");
	  break;
     }
     fflush(outFile);
#endif
}
#endif

/***************************************
 *                                     *
 *            EmitConstruct            *
 *                                     *
 ***************************************/
void
EmitConstruct(void)
{
#if DEBUG
     outFile = fopen(EMITOUTFILE, "w");
     if (outFile == NULL) {
	  fprintf(stderr, "Can not open output file: %s\n", EMITOUTFILE);
	  exit(1);
     }
     fprintf(outFile, "%% %s\n", EMITOUTFILE);
#endif

     codePtr   = 0;

     emitHeapDesc = MemAlloc("emit", MAXCODE, sizeof(M_INSTR));
     codeStore = (M_INSTR *) MemHeapAlloc(emitHeapDesc, MAXCODE, sizeof(M_INSTR));
     assert(codeStore);
     parmHD = MemAlloc("emit parm store", 1, MAXPARM);
     assert(parmHD);
}

/***************************************
 *                                     *
 *           EmitNew                   *
 *                                     *
 ***************************************/
M_INSTR
*EmitNew(char *function)
{
#if DEBUG
     char *result  = NULL;
     FILE *newFile = NULL;

     fprintf(outFile, "%s:\n", function);
#endif
     return(&codeStore[codePtr]);
}

/***************************************
 *                                     *
 *            EmitMsg                  *
 *                                     *
 ***************************************/
void
EmitMsg(char *msg)
{
#if DEBUG
     fprintf(outFile, "%% %s\n", msg);
#endif
}

/***************************************
 *                                     *
 *            EmitDestruct             *
 *                                     *
 ***************************************/
void
EmitDestruct(void)
{
#if DEBUG
     fclose(outFile);
#endif
     MemDealloc(emitHeapDesc);
     codePtr  = 0;
     MemDealloc(parmHD);
}

/***************************************
 *                                     *
 *            EmitAddr                 *
 *                                     *
 ***************************************/
M_INSTR
*EmitAddr(void)
{
     return(&codeStore[codePtr]);
}

/***************************************
 *                                     *
 *            emit                     *
 *                                     *
 ***************************************/
void 
emit(void)
{
#if DEBUG
     Instr2str();
     fflush(outFile);
#endif
     if (codePtr >= MAXCODE) {
	  printMsg(ERROR_MSG, "emit: Out of compiling memory");
	  exit(1);
     }
     codeStore[codePtr].instr = emitCode.instr;
     codeStore[codePtr].cexpr = emitCode.cexpr;
     codeStore[codePtr].info  = emitCode.info;
     codePtr++;

     emitCode.cexpr = NULL;
}


#if 0
/***************************************
 *                                     *
 *            DisplayCode              *
 *                                     *
 ***************************************/
void
DisplayCode(M_INSTR code)
{
     switch (code.instr)  {
	case MCparm:
	  printf("parm\n");
	  break;
	case MCret:
	  printf("ret\n");
	  break;
	case MChalt:
	  printf("halt\n");
	  break;
	case MCbang:
	  printf("!\n");
	  break;
	case MCpair:
	  printf("pair\n");
	  break;
	case MCp0:
	  printf("p0\n");
	  break;
	case MCp1:
	  printf("p1\n");
	  break;
	case MCcons:
	  printf("cons(%s)\n",code.info.structor.name);
	  break;
	case MCinduct:
	  printf("induct\n");
	  break;
	case MCdest:
	  printf("dest(%s)\n",code.info.structor.name);
	  break;
	case MCfunc:
	  printf("func(%s)\n", code.info.func.name);
	  break;
	case MCbclosure:
	  printf("bclosure\n");
	  break;
	case MCclosure:
	  printf("closure\n");
	  break;
	case MCupdate:
	  printf("update\n");
	  break;
	case MCmkupdate:
	  printf("mkupdate\n");
	  break;
	case MCalloc:
	  printf("alloc(%d)\n", code.info.n);
	  break;
	case MCunload:
	  printf("unload\n");
	  break;
	case MCldparm:
	  printf("ldparm\n");
	  break;
	case MCreload:
	  printf("reload\n");
	  break;
	case MCjump:
	  printf("jump\n");	  
	  break;
	default:
	  printf("ERROR DisplayCode: unknown code\n");
	  exit();
     }
}

/***************************************
 *                                     *
 *            EmitShowCode             *
 *                                     *
 ***************************************/
void
EmitShowCode(void)
{
     int count = 0;

     for (count = 0; count < codePtr; count++) {
	  printf("%d\t", count);
	  DisplayCode(codeStore[count]); 
     }
}


/***************************************
 *                                     *
 *            EmitChk                  *
 *                                     *
 ***************************************/
void
EmitChk(void)
{
     int count;

/*     printf("start consistency check..."); */
     for (count = 0; count < codePtr; count++) {
	  switch (codeStore[count].instr) {
	     case MCgc_ptr:
	       printf("Consistency check failed: EmitChk in heap\n");
	       exit(-1);
	       break;
	     case MCbang:
	     case MCcons:
	     case MCpair:
	     case MCsave:
	     case MCswap:
	     case MCret:
	     case MChalt:
	     case MCpr:
	     case MCp0:
	     case MCp1:
	     case MCinduct:
	     case MCmap_prod:
	     case MCparm:
	       break;
	     case MCgoto:
	     case MCfunc:
	     case MCjump:
	       if (   (codeStore[count].info.code < codeStore)
		   || (codeStore[count].info.code > &codeStore[codePtr])
		   ) {
		    printf("Consistency check failed: emitChk in code\n");
		    exit();
	       }
	       break;
	     case MCldparm:
	     case MCarg:
	       if (   (codeStore[count].info.arg.code < codeStore)
		   || (codeStore[count].info.arg.code > &codeStore[codePtr])
		   ) {
		    printf("Consistency check failed: emitChk in code\n");
		    exit();
	       }
	       break;
	     case MCalloc:
	       break;
	     case MCclosure:
	     case MCbclosure:
	       if (   (codeStore[count].info.closure.c < codeStore)
		   || (codeStore[count].info.closure.c > &codeStore[codePtr])
		   ) {
		    printf("Consistency check failed: emitChk in code\n");
		    exit();
	       }
	       break;
	     case MCmkupdate:
	     case MCupdate:
	     case MCdest:
	     case MCnewA:
	     case MCunload:
	     case MCreload:
	       break;
	     default:
	       printf("Consistency check failed: unknown code item\n");
	       EmitShowCode();
	       exit(-1);
	       break;
	  }
     }
/*     printf("Done\n"); */
}

#endif
