
/******************************************************************************
 *                                                                            *
 *   variance.h                                                               *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

/* [H-O] THIS ENTIRE FILE/MODULE IS NEW TO THE SYSTEM */

#ifndef VARIANCE
#define VARIANCE

typedef enum
{
  V_NEITHER,
  V_POSITIVE,
  V_NEGATIVE,
  V_BOTH
}
V_VARIANCE;

extern V_VARIANCE Subst (V_VARIANCE v1, V_VARIANCE v2);
extern V_VARIANCE Meet  (V_VARIANCE v1, V_VARIANCE v2);
extern V_VARIANCE Flip  (V_VARIANCE v);

#endif

