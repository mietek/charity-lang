/******************************************************************************
 *                                                                            *
 *   types.h                                                                  *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __TYPES_H__
#define __TYPES_H__

#define DIR_SEPARATOR "/"

#define MAX_STRING_LENGTH_DEFAULT 100
#define RES_PREFIX                "$$"               /* reserved prefix */
#define STATE_VAR_REP             RES_PREFIX "C"

#define TERMINAL_TYPE             "1"
#define RES_ID                    "id"
#define RES_ID1                   "id1"
#define PROD_TYPE                 "*"                /* infix notation  */
#define PROD0                     "p0"
#define PROD1                     "p1"
#define MAP_PROD                  RES_PREFIX "map_" PROD_TYPE
#define SUM_TYPE                  "coprod"
#define SUM_TYPE_INFIX            "+"
#define SUM0                      "b0"
#define SUM1                      "b1"
#define DONTCARE                  RES_PREFIX "_"
#define RES_VAR_X                 RES_PREFIX "X"
#define ENVIRONMENT               RES_PREFIX "_sigma"

#define HASH_NAME "#"     /* [#@] */
#define AT_NAME   "@"

#define PP_MAX_RECORD_DEPTH 0     /* !!!! should be user definable */
#define PP_MAX_SHOW_DEPTH 100     /* !!!! should be user definable */

/* [H-O] ALTERED THE OPENING MESSAGE, OF COURSE: */

#define CHARITY_VERSION  "1.99 (beta)"
#define CHARITY_DATE     "Sep. 1997"
#define CHARITY_OPEN_MSG "\nThe Charity System, version " CHARITY_VERSION "\n" \
                         " Charity Development Group - " CHARITY_DATE "\n\n"

#define CHARITY_PROMPT      "Charity>> "
#define CHARITY_CONT_PROMPT "          "

#define NUM_OPCOMBS 3     /* number of operation combinators */

typedef enum
{
  BFALSE = 0,
  BTRUE
}
BBOOL;

/* typedef unsigned long int ST_KEY; */    /* SYMBOL TABLE ENTRY KEYS */
typedef long int ST_KEY;    /* SYMBOL TABLE ENTRY KEYS */

#endif
