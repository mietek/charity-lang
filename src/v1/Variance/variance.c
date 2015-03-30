
/******************************************************************************
 *                                                                            *
 *   variance.c                                                               *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include "variance.h"

V_VARIANCE
Subst (V_VARIANCE v1,
       V_VARIANCE v2)
{
  switch (v1)
    {
    case V_NEITHER:
      return V_NEITHER;

    case V_POSITIVE:
      return v2;

    case V_NEGATIVE:
      switch (v2)
        {
        case V_NEITHER:
          return V_NEITHER;

        case V_POSITIVE:
          return V_NEGATIVE;

        case V_NEGATIVE:
          return V_POSITIVE;

        case V_BOTH:
          return V_BOTH;
        }

    case V_BOTH:
      if (v2 == V_NEITHER)
        return V_NEITHER;
      else
        return V_BOTH;
    }
}

V_VARIANCE
Meet (V_VARIANCE v1,
      V_VARIANCE v2)
{
  switch (v1)
    {
    case V_NEITHER:
      return v2;

    case V_POSITIVE:
      switch (v2)
        {
        case V_NEITHER:
        case V_POSITIVE:
          return V_POSITIVE;

        default:
          return V_BOTH;
        }

    case V_NEGATIVE:
      switch (v2)
        {
        case V_NEITHER:
        case V_NEGATIVE:
          return V_NEGATIVE;

        default:
          return V_BOTH;
        }

    case V_BOTH:
      return V_BOTH;
    }
}

V_VARIANCE
Flip (V_VARIANCE v)
{
  switch (v)
    {
    case V_POSITIVE:
      return V_NEGATIVE;

    case V_NEGATIVE:
      return V_POSITIVE;

    default:
      return v;
    }
}

