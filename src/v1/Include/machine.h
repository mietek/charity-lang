/******************************************************************************
 *                                                                            *
 *   machine.h                                                                *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#ifndef __MACHINE__
#define __MACHINE__

#include "types.h"


/**************************
 *                        *
 *     TYPES              *
 *                        *
 **************************/

struct _COMB_EXPR;
struct _ST_TYPE;


/* INSTRUCTIONS (CODE): */

typedef enum _M_INSTR_TAG
{
  MCinvalid     ,
  MCparm        ,
  MCret         ,
  MChalt        ,
  MCbang        ,
  MCsave        ,
  MCswap        ,
  MCpair        ,
  MCp0          ,
  MCp1          ,
  MCpr          ,
  MCcons        ,
  MCmap_prod    ,
  MCinduct      ,
  MCdest        ,
  MCdestHO      ,     /* [H-O] ADDED (SEE machine.c) */
  MCfunc        ,
  MCgc_ptr      ,
  MCbclosure    ,
  MCclosure     ,
  MCupdate      ,
  MCmkupdate    ,
  MCalloc       ,
  MCunload      ,
  MCldparm      ,
  MCreload      ,

  MCjump        ,
  MCjumpc       ,     /* [#@] */

  MCgoto        ,
  MCldmacroframe,

  MCat          ,     /* [#@] */

  MCint         ,     /* [BI] BUILTIN VALUES     */
  MCchar        ,

  MCadd         ,     /* [BI] BUILTIN OPERATIONS */
  MCsub         ,
  MCmul         ,
  MCdiv         ,
  MCmod         ,

  MClt_int      ,
  MCle_int      ,
  MCgt_int      ,
  MCge_int      ,
  MCeq_int      ,

  MClt_char     ,
  MCle_char     ,
  MCgt_char     ,
  MCge_char     ,
  MCeq_char     ,

  MCcode        ,
  MCdecode
}
M_INSTR_TAG;

typedef struct _M_INSTR
{
  M_INSTR_TAG instr;

  struct _COMB_EXPR *cexpr;

  union
    {
      int               structorPosn;     /* constructors/destructor positions           */
      struct _M_INSTR  *closureCode;      /* code for a closure                          */
      struct _M_INSTR  *inductCode;       /* inductive combinator code                   */
      struct _M_INSTR  *funcCode;         /* code for a function call                    */
      int               allocNum;         /* alloc n heap items                          */
      int               updateClo;        /* update closure n of record on the dump      */
      struct _M_INSTR **macroList;        /* list of macros passed to a function         */
      int               macroParm;        /* macro parameter                             */
      int               numDestrs;        /* number of destructors of a record           */
      struct _M_INSTR  *code;             /* for jumps and gotos                         */

      int               i;                /* [BI] FOR BUILTIN INTEGERS                   */
      char              c;                /* [BI] FOR BUILTIN CHARACTERS                 */
    }
  info;
}
M_INSTR;


/**************************
 *                        *
 *     FUNCTIONS          *
 *                        *
 **************************/

extern void mc_MachineConstruct (void);
extern void mc_MachineDestruct  (void);

extern void MachineReset (void);

extern void mc_MachineOpen  (void);
extern void mc_MachineClose (void);

extern struct _COMB_EXPR *Evaluate (struct _COMB_EXPR *expr,
				    struct _ST_TYPE   *resultType);

#endif
