/******************************************************************************
 *                                                                            *
 *   ioChar.h                                                                 *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
/* module for doing IO */

#ifndef __IOCHAR_H__
#define __IOCHAR_H__


#include <stdio.h>
#include <setjmp.h>

#define MAX_INPUT_LENGTH 1000

extern jmp_buf topLevelEnv;
extern int delayedErrorCount;

typedef enum {
MSG,
FATAL_MSG,
ERROR_MSG,
DELAYEDERROR_MSG,
WARN_MSG,
PROMPT_MSG,
DEBUG_MSG
}
MSG_TYPE;

/**************************
 *                        *
 *    Macro definitions   *
 *                        *
 **************************/


/**************************
 *                        *
 * function prototypes    *
 *                        *
 **************************/
extern void         emptyInputLine(void);
extern void         restoreInputLine(void);
extern void         initPrintBuff(void);
extern void         printMsg(MSG_TYPE msgType, char *msg, ...);
extern char        *getInputLine( char *input, int bufsize );
extern void         appendBuff(char *str);     /* put a string in buffer */
extern void         outputBuff(FILE *file);    /* output buffer */
extern void         clearBuff(void);       /* reset pointer and empty buffer */



#endif
