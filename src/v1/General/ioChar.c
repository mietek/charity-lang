/******************************************************************************
 *                                                                            *
 *   ioChar.c                                                                 *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
/* module for doing IO */

#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#include "ioChar.h"
#include "pmem.h"
#include "symtab.h"
#include "list.h"

#define MAX_PRINT_BUFF_DEFAULT 100   /* !!!! should be cut back to 100 */
#define MAX_MSG_LENGTH         1000   /* should satisfy all windbags */

static char     *printBuff;
static int      printBuffPtr;
static MEMORY   printBuffDesc;
static int 	 MaxPrintbuff;
int      delayedErrorCount = 0;

/**************************
 *                        *
 * function prototypes    *
 *                        *
 **************************/
static void         initBuff(int bufferSize);  /* create buffer */
static void         remBuff(void);             /* deallocates buffer space */


/**************************
 *                        *
 *    emptyInputLine      *
 *                        *
 **************************/
void
emptyInputLine(void) {

  yyemptyInputLine();

}


/**************************
 *                        *
 *    restoreInputLine    *
 *                        *
 **************************/
void
restoreInputLine(void) {

  yyrestoreInputLine();

}


/**************************
 *                        *
 *    initPrintBuff       *
 *                        *
 **************************/
void
initPrintBuff(void) {

  initBuff(MAX_PRINT_BUFF_DEFAULT);

}


/**************************
 *                        *
 *    getInputLine        *
 *                        *
 **************************/
char *
getInputLine(char *input, int size) {

  fgets(input, size, stdin);
  return(input);

}


/**************************
 *                        *
 *    printMsg            *
 *                        *
 **************************/
void 
printMsg(MSG_TYPE msgType, char *msg, ...) {

  va_list  ap;
  char        *p,
              *strVal,
               convStr[MAX_MSG_LENGTH],
               text[MAX_MSG_LENGTH];
  int          i=0,
               intVal;
  CT_EXPR     *coreTermLogicExpr;
  ST_TYPE_SIG *typeSigVal;
  ST_TYPE     *st_type;
  LIST        *list;
  CT_VAR_BASE *ctVarBase;

  clearBuff();
  switch (msgType) {
  case MSG :  
  case PROMPT_MSG : 
    break;
  case FATAL_MSG :
    appendBuff("*** FATAL ERROR: ");     break;
  case ERROR_MSG :
    appendBuff("*** ERROR: ");           break;
  case DEBUG_MSG :
    appendBuff("-->DEBUG: ");           break;
  case DELAYEDERROR_MSG :
    appendBuff("*** ERROR: ");       delayedErrorCount++;    break;
  case WARN_MSG :
    appendBuff("WARNING: ");         break;
  default :
    printMsg(FATAL_MSG, "printMesg - Invalid value for MSG_TYPE");
  }

  va_start(ap, msg);

  for (p=msg; *p; p++) {
    if (*p != '%') 
      text[i++] = *p;
    else {
      text[i] = 0;      i=0;
      appendBuff(text);
      switch (*++p) {
      case '%' :
	appendBuff("%");
	break;
      case 'd' :
	intVal = va_arg(ap, int);
	sprintf(convStr,"%d",intVal);
	appendBuff(convStr);
	break;
      case 'r' :
	coreTermLogicExpr = va_arg(ap, CT_EXPR*);
	_showCT_expr(coreTermLogicExpr, 0);
	break;
      case 's' :
	strVal = va_arg(ap, char*);
	appendBuff(strVal);
	break;
      case 't' :
	typeSigVal = va_arg(ap, ST_TYPE_SIG*);
	st_ShowSig(typeSigVal);
	break;
      case 'L' :  /* lists */
	list = va_arg(ap, LIST*);
	showList(list, BFALSE);
	break;
      case 'T' :
	st_type = va_arg(ap, ST_TYPE *);
	st_ShowType(st_type);
	break;
      case 'S' :	/* running out of choices */
	typeSigVal = va_arg(ap, ST_TYPE_SIG*);
	st_ShowTypeSig(typeSigVal);
	break;	
      case 'U' :	/* this is to strip out context variables */
	typeSigVal = va_arg(ap, ST_TYPE_SIG*);
	st_ShowTypeSigLessContext(typeSigVal);
	break;
      case 'V' :
        ctVarBase = va_arg(ap, CT_VAR_BASE*);
        ctShowVarBase(ctVarBase);
        break;
      default :
	printMsg(FATAL_MSG, "Invalid format specifier: %s", p);
      }   /*  hctiws  */
    }   /*  esle  */
  }   /*  rof  */

  text[i] = 0;
  appendBuff(text);     



  switch (msgType) {
  case MSG :
  case WARN_MSG :
  case DEBUG_MSG :
  case DELAYEDERROR_MSG :
    appendBuff("\n");
    outputBuff(stdout);
    break;
  case PROMPT_MSG :
    outputBuff(stdout);
    break;
  case FATAL_MSG :
    appendBuff("\n");
    outputBuff(stdout);
    exit(-1);
    break;
  case ERROR_MSG :
    appendBuff("\n");
    outputBuff(stdout);
    longjmp(topLevelEnv, 1);
    break;
  default :
    appendBuff("\n");
    outputBuff(stdout);
    printMsg(FATAL_MSG, "printMesg - Invalid value for MSG_TYPE");
  }
  

}   /*  end printMsg  */


/**************************
 *                        *
 *    initBuff            *
 *                        *
 **************************/
void
initBuff(int bufferSize) {

  printBuffPtr = 0;
  MaxPrintbuff = bufferSize;
  printBuffDesc = MemAlloc("ioChar", MaxPrintbuff + 1, sizeof(char));
  printBuff = MemHeapAlloc(printBuffDesc, MaxPrintbuff, sizeof(char));

}


/**************************
 *                        *
 * appendBuff             *
 *                        *
 **************************/
void 
appendBuff(char *str) {

  int i=0;

  if (str) {
    while (str[i]) {
      if (printBuffPtr == (MaxPrintbuff - 1))  {
	printBuff[++printBuffPtr] = 0;
	outputBuff(stdout);
	clearBuff();
      }
      printBuff[printBuffPtr++] = str[i++];
    }   /*  elihw  */
    printBuff[printBuffPtr] = 0;
  }   /*  fi  */
}


/**************************/
/*                        */
/* outputBuff             */
/*                        */
/**************************/
void 
outputBuff(FILE *stream) {

  printf(printBuff);   /* !!!! only works for FILE * = stdout */
  clearBuff();

}


/**************************/
/*                        */
/* clearBuff              */
/*                        */
/**************************/
void 
clearBuff() {

int i=0;

while ((printBuff[i] != NULL) && (i < MaxPrintbuff)) {
  printBuff[i++] = 0;
}

printBuffPtr = 0;

}


/**************************/
/*                        */
/* remBuff                */
/*                        */
/**************************/
void
remBuff() {

  MemDealloc(printBuffDesc);
  printBuff = NULL;

}
