/******************************************************************************
 *                                                                            *
 *   main.c                                                                   *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#define YYDEBUG   0
/*#define MAX_HEAPS 25*/
#define MAX_HEAPS 99


#include <stdio.h> 
#include "ioChar.h"
#include "pmem.h"
#include "parse.h"
#include "symtab.h"
#include "codetab.h"
#include "ctTranslate.h"
#include "y.tab.h"
#include "commands.h"

/*****************************************************************************
 *                                                                           *
 *                           internal prototypes                             *
 *                                                                           *
 *****************************************************************************/

void SystemConstruct(void);
void SystemDestruct(void);

/*****************************************************************************
 *                                                                           *
 *                           internal variables                              *
 *                                                                           *
 *****************************************************************************/

extern FILE *yyin, *yyout;
extern int yydebug;
int      lineNo   = 1;
int      userEOF = 0;
char    *progname;
char    *usage = "usage: %s [infile]\n";
jmp_buf  topLevelEnv;


/***************************************
 *                                     *
 *        SystemConstruct              *
 *                                     *
 ***************************************/
void
SystemConstruct(void)
{
     MemConstruct(MAX_HEAPS);
     initPrintBuff();

     printMsg(PROMPT_MSG, CHARITY_OPEN_MSG);
     printMsg(PROMPT_MSG, CHARITY_PROMPT);

     initIncludeDirs();

     ParserConstruct();
     yyin = stdin;
     ParseResult.tag = EMPTY_INPUT;

     initSymTable(BTRUE);

     pmInit();

     CodeTableConstruct(CODETAB_HEAP_SIZE);
     ctTranslateConstruct();
     mc_MachineConstruct();
}

/***************************************
 *                                     *
 *        SystemDestruct               *
 *                                     *
 ***************************************/
void
SystemDestruct(void)
{
     ParserDestruct();
     CodeTableDestruct();
     ctTranslateDestruct();
     mc_MachineDestruct();
     st_DestructSymTable();
     pmDestructCTHeap();
     ge_DestructIncDirs();
/*     MemDisplayState();*/
     MemDestruct();
     fclose(yyin);
     printMsg(MSG, "");
}


/*********************************
 *                               *
 *    main                       *
 *                               *
 *********************************/
int
main(int argc, char **argv)
{
/*  yydebug = 1; */
  progname = argv[0];

  if (argc > 2) {
    fprintf(stderr, usage, progname);
    exit(1);
  }
  
  SystemConstruct();

  if (argc > 1) {
    yyin = fopen(argv[1], "r");
    if (yyin == NULL) {
      printMsg(ERROR_MSG, "%s: can not open %s\n", progname, argv[1]);
      yyin = stdin;
    }
  }
  ParseStream();

  SystemDestruct();

  printf ("\n");     /* [H-O] [FIX] SHOULD BE VIA I/O MODULE */
  
  return(0);
}
