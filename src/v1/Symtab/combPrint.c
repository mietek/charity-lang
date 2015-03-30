/******************************************************************************
 *                                                                            *
 *   combPrint.c                                                              *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/
/* Code to pretty print combinator strings. Based on structures in
   codetab.h. */

#include <ctype.h>     /* [BI] ADDED THIS INCLUDE */
#include <string.h>

#include "codetab.h"
#include "ioChar.h"

#define CLOSURE_STRING "..."
#define PP_BUFFER_SIZE 50000

typedef enum
{
  PP_DEPTH_OK=0,
  PP_RDEPTH_EXCEEDED,
  PP_SDEPTH_EXCEEDED
}
PP_COMB_WARNING;


static int MaxRecordDepth;          /* the maximum depth to print records to. */
static int MaxShowDepth;
static char integer[400];           /* max length of an integer */

static ST_TYPE *st_type;           /* the type of an expression */

/*****************************************************************************
 *                                                                           *
 *            Prototypes For Internal Functions                              *
 *                                                                           *
 *****************************************************************************/
static PP_COMB_WARNING  combShow(COMB_EXPR *comb, int sDepth, int rDepth);
static PP_COMB_WARNING  showCombExpr(COMB_EXPR *combExpr,
                                     int sDepth,
                                     int rDepth);

/* [H-O] ALTERED THE TYPE OF THE FIRST PARAMETER (SEE BELOW): */

static PP_COMB_WARNING showParams(COMB_PHR **params,
                                  int        sDepth,
                                  int        rDepth,
                                  char     **recNames,
                                  int        numParams);

static void            rightDisplayMode(COMB_EXPR *combExpr,
                                        int maxShowDepth,
                                        int maxRecordDepth);
static void            leftDisplayMode(COMB_EXPR *combExpr,
                                        int maxShowDepth,
                                        int maxRecordDepth);
static BBOOL           structorDomainIsPair(char *structorName);
static PP_COMB_WARNING showComposition(COMB_EXPR *combExpr,
                                       int sDepth,
                                       int rDepth);

static PP_COMB_WARNING st_ShowList(COMB_EXPR *list,
                                int sDepth,
                                int rDepth,
                                BBOOL first);

static PP_COMB_WARNING st_ShowString (COMB_EXPR *string,     /* [BI] ADDED THIS PROTOTYPE (SEE BELOW) */
                                      int        sDepth,
                                      int        rDepth,
                                      BBOOL      first);

static void            showINT(COMB_EXPR *combExpr);
static void            makeDigits(COMB_EXPR *digits, int i);

static void            showSTRING(COMB_EXPR *str);

static void            _combExprPrint(COMB_EXPR *combExpr,
                                      int        maxShowDepth,
                                      int        maxRecordDepth);


/*********************************
 *                               *
 *    combExprPrint              *
 *                               *
 *********************************/
void
combExprPrint(COMB_EXPR *combExpr, int maxShowDepth, int maxRecordDepth, ST_TYPE *type) {
     st_type = type;
     _combExprPrint(combExpr, maxShowDepth, maxRecordDepth);
     deCompileReset();
}

/*********************************
 *                               *
 *    _combExprPrint             *
 *                               *
 *********************************/
void
_combExprPrint(COMB_EXPR *combExpr, int maxShowDepth, int maxRecordDepth) {

  PP_COMB_WARNING warning;

  MaxRecordDepth = maxRecordDepth;
  MaxShowDepth = maxShowDepth;
  clearBuff();
  warning = showCombExpr(combExpr, -1, -1);
  outputBuff(stdout);

  switch (warning) {
    case PP_DEPTH_OK :
      printMsg(MSG, " : %T", st_type);
      break;
    case PP_RDEPTH_EXCEEDED :
      printMsg(PROMPT_MSG, "\n\nRight display mode:\n(q - quit, return - more) >>");
      rightDisplayMode(combExpr, maxShowDepth, maxRecordDepth);
      break;
    case PP_SDEPTH_EXCEEDED :
      printMsg(PROMPT_MSG, "\n\nLeft display mode:\n(q - quit, return - more) >>");
      leftDisplayMode(combExpr, maxShowDepth, maxRecordDepth);
      break;
    default :
      printMsg(FATAL_MSG, "_combExprPrint - %d is not a valid PP_COMB_WARNING tag", warning);
      break;
  }   /*  hctiws  */

}


/*********************************
 *                               *
 *    showCombExpr               *
 *                               *
 *********************************/
static PP_COMB_WARNING
showCombExpr(COMB_EXPR *combExpr, int sDepth, int rDepth) {
/* depth we are at. Top depth is depth 0. The only way to increment the
   rDepth is to go through a record (see combShow).  */

  COMB_EXPR       *newCombExpr;
  PP_COMB_WARNING  rval = PP_DEPTH_OK;

  if (sDepth < MaxShowDepth)
    switch (combExpr->tag) {
      case CTT_COMBINATOR :
        rval = combShow(combExpr, ++sDepth, rDepth);
        break;
      case CTT_COMPOSITION :
        /* left side should only be a constructor */
        rval = showComposition(combExpr, sDepth, rDepth);
        break;
      case CTT_CLOSURE :
        if (rDepth < MaxRecordDepth) {
          newCombExpr = Evaluate(combExpr, NULL);
          combExpr->tag = newCombExpr->tag; /*!!!! may want to take this out */
          combExpr->info = newCombExpr->info;
          rval = showCombExpr(newCombExpr, sDepth, rDepth);
        }
        else {
          appendBuff(CLOSURE_STRING);
          rval = PP_RDEPTH_EXCEEDED;
        }
        break;
      default :
        printMsg(FATAL_MSG, "showCombExpr - %d is not a valid tag", combExpr->tag);
        break;
      }   /*  hctiws  */
  else {
    appendBuff(CLOSURE_STRING);
    rval = PP_SDEPTH_EXCEEDED;
  }

  return(rval);

}   /*  end showCombExpr  */


/*********************************
 *                               *
 *    showComposition            *
 *                               *
 *********************************/
static PP_COMB_WARNING
showComposition(COMB_EXPR *combExpr, int sDepth, int rDepth) {

  COMB_EXPR       *rExpr = combExpr->info.composition.r,
                  *lExpr = combExpr->info.composition.l;
  PP_COMB_WARNING  rval=0;

  if (lExpr->info.combinator.class == CC_CONSTRUCTOR)
      if ((strcmp(lExpr->info.combinator.name, "cons") == 0) ||      /* sugar */
          (strcmp(lExpr->info.combinator.name, "nil") == 0))
        {
          if (strcmp(lExpr->info.combinator.name, "cons") == 0 &&
              rExpr->info.combinator.param[0]->positive->info.combinator.class == CC_BUILTIN_CHAR)
            {
              appendBuff ("\"");
              rval = st_ShowString (rExpr, sDepth + 1, rDepth, BTRUE);     /* [BI] PRINT STRINGS */
              appendBuff ("\"");
            }
          else
            {
              appendBuff("[");
              rval = st_ShowList(rExpr, sDepth+1, rDepth, BTRUE);
              appendBuff("]");
            }
        }
      else if (strcmp(lExpr->info.combinator.name, "INT") == 0)      /* sugar */
        showINT(rExpr);
      else if (strcmp(lExpr->info.combinator.name, "STRING") == 0)  {/* sugar */
        appendBuff("\"");
        showSTRING(rExpr);
        appendBuff("\"");
      }   /*   fi esle  */
      else {                                                     /* no sugar */
        showCombExpr(combExpr->info.composition.l, sDepth, rDepth);

        if (rExpr->tag == CTT_COMBINATOR)
          switch (rExpr->info.combinator.class) {
          case CC_PRIMITIVE :
            /* don't print composed !s */
            if (strcmp(rExpr->info.combinator.name, PRIM_BANG)!=0)
              rval =showCombExpr(combExpr->info.composition.r, ++sDepth, rDepth);
            break;
          case CC_CONSTRUCTOR :
            appendBuff("(");
            appendBuff(rExpr->info.combinator.name);
            appendBuff(")");
            break;

          case CC_BUILTIN_CHAR:     /* [BI] PRINT BUILTIN CHARACTERS */
            {
              char s[100];

              if (isprint (rExpr->info.combinator.c))
                sprintf (s, "('%c')", rExpr->info.combinator.c);
              else
                sprintf (s, "(\\d%d)", (int)rExpr->info.combinator.c);

              appendBuff (s);
            }

            break;

          case CC_BUILTIN_INT:     /* [BI] PRINT BUILTIN INTEGERS */
            {
              char s[100];

              sprintf (s, "(%d)", rExpr->info.combinator.i);
              appendBuff (s);
            }

            break;

        case CC_DESTRUCTOR :
        case CC_FOLD :
        case CC_CASE :
        case CC_MAP_I :
        case CC_UNFOLD :
        case CC_RECORD :
        case CC_MAP_C :
        case CC_FUNCTION :
        case CC_MACRO :
          rval = showCombExpr(combExpr->info.composition.r, ++sDepth, rDepth);
          break;
        default :
          printMsg(FATAL_MSG, "showComposition() - unexpected class %d",
                   rExpr->info.combinator.class);
        }
      else if (structorDomainIsPair(lExpr->info.combinator.name)) {
          appendBuff("(");
          rval = showCombExpr(combExpr->info.composition.r,++sDepth, rDepth);
          appendBuff(")");
        }   /*  fi esle  */
      else
        rval = showCombExpr(combExpr->info.composition.r, ++sDepth, rDepth);
    }   /*  esle  */

  else
    printMsg(FATAL_MSG, "Unexpected combinator class, %d, in showComposition",
             lExpr->info.combinator.class);

  return(rval);

}


/*********************************
 *                               *
 *    st_ShowList                *
 *                               *
 *********************************/
static PP_COMB_WARNING
st_ShowList(COMB_EXPR *list, int sDepth, int rDepth, BBOOL first) {
/* combExpr eg. <!;zero,<!;zero,!;nil>;cons>   (cons case)  or
            eg. !     (nil case) */

  PP_COMB_WARNING  rval=0,
                   tval;
  COMB_EXPR       *listMem,
                  *listTail;

  if ((list->tag == CTT_COMBINATOR) &&
      (list->info.combinator.class == CC_PRIMITIVE))
    if (strcmp(list->info.combinator.name, PRIM_PAIR) == 0) { /* cons case */
      first ? appendBuff("") : appendBuff(", ");

      /* [H-O] ALTERED combinator.param ACCESS (SEE codetab.h): */

      listMem = list->info.combinator.param[0]->positive;
      listTail = list->info.combinator.param[1]->positive;

      rval = showCombExpr(listMem, sDepth,rDepth);
      tval = st_ShowList(listTail->info.composition.r, sDepth, rDepth, BFALSE);

      tval = st_ShowList(listTail, sDepth, rDepth, BFALSE);
      tval ? rval = tval : rval;
    }
    else                                                      /* nil case */
      appendBuff("");
  else
      appendBuff("");
/*    printMsg(FATAL_MSG,
             "Unexpected combinator expression structure in st_ShowList");*/

  return(rval);

}


/*********************************
 *                               *
 *    st_ShowString              *
 *                               *
 *********************************/

/* [BI] ADDED THIS FUNCTION FOR PRINTING STRINGS: */

static
PP_COMB_WARNING
st_ShowString (COMB_EXPR *list,
               int        sDepth,
               int        rDepth,
               BBOOL      first)
{
  /*
   * combExpr eg. <!;'a',!;nil>     (cons case)
   *          eg. !                 (nil case)
   *
   */

  PP_COMB_WARNING  rval = PP_DEPTH_OK,
                   tval;
  COMB_EXPR       *listMem,
                  *listTail;

  if (strcmp(list->info.combinator.name, PRIM_PAIR) == 0)
    {
      /* [H-O] ALTERED combinator.param ACCESS (SEE codetab.h): */

      listMem = list->info.combinator.param[0]->positive;
      listTail = list->info.combinator.param[1]->positive;

/*       rval = showCombExpr(listMem, sDepth,rDepth); */

      {
        char output[10];

        if (listMem->info.combinator.c == '\\')
          sprintf (output, "\\\\");
        else if (isprint (listMem->info.combinator.c))
          sprintf (output, "%c", listMem->info.combinator.c);
        else
          sprintf (output, "\\d%d", (int)listMem->info.combinator.c);

        appendBuff (output);
      }

      tval = st_ShowString(listTail->info.composition.r, sDepth, rDepth, BFALSE);

      tval = st_ShowString(listTail, sDepth, rDepth, BFALSE);
      tval ? rval = tval : rval;
    }

  return rval;
}


/*********************************
 *                               *
 *    showINT                    *
 *                               *
 *********************************/
static void
showINT(COMB_EXPR *combExpr) {
/* combExpr eg.  <!;positive,<!;d1,<!;d2,!;nil>;cons>;cons> */

  char *sign;

  /* [H-O] ALTERED combinator.param ACCESS (SEE codetab.h): */

  if ((combExpr->tag == CTT_COMBINATOR) &&
      (combExpr->info.combinator.class == CC_PRIMITIVE) &&
      (strcmp(combExpr->info.combinator.name, PRIM_PAIR) == 0) &&
      (combExpr->info.combinator.param[0]->positive->tag == CTT_COMPOSITION) &&
      (combExpr->info.combinator.param[0]->positive->info.composition.l->tag == CTT_COMBINATOR) &&
      (combExpr->info.combinator.param[0]->positive->info.composition.l->info.combinator.class == CC_CONSTRUCTOR)) {
    sign =
     (combExpr->info.combinator.param[0]->positive->info.composition.l->info.combinator.name);
    if (strcmp(sign, "positive") == 0)
      appendBuff("");
    else if (strcmp(sign, "negative") == 0)
      appendBuff("-");
    else
      printMsg(FATAL_MSG, "Unexpected sign constructor, %s, in showINT",
               sign);
    makeDigits(combExpr->info.combinator.param[1]->positive, 0);
    appendBuff(integer);
  }   /*  fi  */
  else
    printMsg(FATAL_MSG,
             "Combinator expression is not structured as expected in showINT");
}


/*********************************
 *                               *
 *    makeDigits                 *
 *                               *
 *********************************/
static void
makeDigits(COMB_EXPR *digits, int i) {

  COMB_EXPR *digitsTail;

  /* [H-O] ALTERED combinator.param ACCESS (SEE codetab.h): */

  if ((digits->tag == CTT_COMPOSITION) &&
      (digits->info.composition.l->tag == CTT_COMBINATOR) &&
      (digits->info.composition.l->info.combinator.class == CC_CONSTRUCTOR))
    if (strcmp(digits->info.composition.l->info.combinator.name, "cons") == 0){
      digitsTail = digits->info.composition.r;
      if ((digitsTail->tag == CTT_COMBINATOR) &&
          (digitsTail->info.combinator.class == CC_PRIMITIVE) &&
          (strcmp(digitsTail->info.combinator.name, PRIM_PAIR) == 0) &&
          (digitsTail->info.combinator.param[0]->positive->tag == CTT_COMPOSITION) &&
          (digitsTail->info.combinator.param[0]->positive->info.composition.l->tag ==
           CTT_COMBINATOR) &&
          (digitsTail->info.combinator.param[0]->positive->info.composition.l->info.combinator.class == CC_CONSTRUCTOR))  {
        integer[i] =
          digitsTail->info.combinator.param[0]->positive->info.composition.l->info.combinator.name[1];
        makeDigits(digitsTail->info.combinator.param[1]->positive, i+1);
      }   /*  fi  */
      else
        printMsg(FATAL_MSG,
                "Unexpected structure of combinator expression in showDigits");
    }   /*  fi  */
    else if (strcmp(digits->info.composition.l->info.combinator.name, "nil")
             == 0)
      integer[i] = '\0';
    else
      printMsg(FATAL_MSG, "Unexpected constructor, %s, in showDigits",
               digits->info.composition.l->info.combinator.name);

}


/*********************************
 *                               *
 *    showSTRING                 *
 *                               *
 *********************************/
static void
showSTRING(COMB_EXPR *str) {
/* str eg. cons(CHAR(cons(d1, cons(d0, cons(d4, nil)))), cons(CHAR(cons(d1, cons(d0, cons(d5, nil)))), nil)) */

  COMB_EXPR *strTail;
  char       c;

  /* [H-O] ALTERED combinator.param ACCESS (SEE codetab.h): */

  if ((str->tag == CTT_COMPOSITION) &&
      (str->info.composition.l->tag == CTT_COMBINATOR) &&
      (str->info.composition.l->info.combinator.class == CC_CONSTRUCTOR))
    if (strcmp(str->info.composition.l->info.combinator.name, "cons") == 0){
      strTail = str->info.composition.r;
      if ((strTail->tag == CTT_COMBINATOR) &&
          (strTail->info.combinator.class == CC_PRIMITIVE) &&
          (strcmp(strTail->info.combinator.name, PRIM_PAIR) == 0) &&
          (strTail->info.combinator.param[0]->positive->tag == CTT_COMPOSITION) &&
          (strTail->info.combinator.param[0]->positive->info.composition.l->tag ==
           CTT_COMBINATOR) &&
          (strTail->info.combinator.param[0]->positive->info.composition.l->info.combinator.class == CC_CONSTRUCTOR) &&
          (strcmp(strTail->info.combinator.param[0]->positive->info.composition.l->info.combinator.name, "CHAR") == 0))  {
        makeDigits(strTail->info.combinator.param[0]->positive->info.composition.r, 0);
        c = (char)atoi(integer);
        appendBuff(&c);
        showSTRING(strTail->info.combinator.param[1]->positive);
      }   /*  fi  */
      else
        printMsg(FATAL_MSG,
                "Unexpected structure of combinator expression in showSTRING");
    }   /*  fi  */
    else if (strcmp(str->info.composition.l->info.combinator.name, "nil")
             == 0)
      appendBuff("");
    else
      printMsg(FATAL_MSG, "Unexpected constructor, %s, in showSTRING",
               str->info.composition.l->info.combinator.name);

}


/*********************************
 *                               *
 *    structorDomainIsPair       *
 *                               *
 *********************************/
static BBOOL
structorDomainIsPair(char *structorName) {

  ST_TYPE *domainType = getStructorType(structorName);

  if (domainType->tag != TYPE_PROD)
    return(BTRUE);
  else
    return(BFALSE);

}


/*********************************
 *                               *
 *    combShow                   *
 *                               *
 *********************************/
static PP_COMB_WARNING
combShow(COMB_EXPR *combExpr, int sDepth, int rDepth) {

char             *pairNames[] = {NULL, NULL},
                  **recNames;
  PP_COMB_WARNING   rval = PP_DEPTH_OK;

  switch (combExpr->info.combinator.class) {
    case CC_PRIMITIVE :
      if (strcmp(combExpr->info.combinator.name, PRIM_BANG)==0)
        appendBuff("()");
      else if (strcmp(combExpr->info.combinator.name, PRIM_PAIR)==0) {
        appendBuff("(");
        rval = showParams(combExpr->info.combinator.param,
                          sDepth,   rDepth,   pairNames,    2);
        appendBuff(")");
      }
      else
        printMsg(FATAL_MSG, "combShow - %s is not a valid primitive name", combExpr->info.combinator.name);
      break;
    case CC_CONSTRUCTOR :
      if (strcmp(combExpr->info.combinator.name, "nil") == 0)
        appendBuff("[]");
      else
        appendBuff(combExpr->info.combinator.name);
      break;
    case CC_RECORD :
      appendBuff("(");
      recNames = getStructorNames(combExpr->info.combinator.parentName);
      rval = showParams(combExpr->info.combinator.param,
                        sDepth,    ++rDepth,    recNames,
                        getNumStructors(combExpr->info.combinator.parentName));
      appendBuff(")");
      break;
    case CC_DESTRUCTOR :
    case CC_FOLD :
    case CC_CASE :
    case CC_MAP_I :
    case CC_UNFOLD :
    case CC_MAP_C :
    case CC_FUNCTION :
      printMsg(FATAL_MSG, "combShow - %d is an unexpected tag", combExpr->info.combinator.class);
      break;

    case CC_BUILTIN_INT:     /* [BI] PRINT BUILTIN INTEGERS */
      {
        char s[100];

        sprintf (s, "%d", combExpr->info.combinator.i);
        appendBuff (s);
      }

      break;

    case CC_BUILTIN_CHAR:     /* [BI] PRINT BUILTIN CHARACTERS */
      {
        char s[100];

        if (isprint (combExpr->info.combinator.c))
          sprintf (s, "'%c'", combExpr->info.combinator.c);
        else
          sprintf (s, "\\d%d", (int)combExpr->info.combinator.c);

        appendBuff (s);
      }

      break;

    default :
      printMsg(FATAL_MSG, "combShow - %d is an invalid tag", combExpr->info.combinator.class);
      break;
  }   /*   hctiws  */

  return(rval);

}   /*  end combShow  */


/*********************************
 *                               *
 *    showParams                 *
 *                               *
 *********************************/

/*
 * [H-O] - ALTERED THE TYPE OF THE FIRST PARAMETER (SEE codetab.h)
 *       - ALTERED THE FUNCTION TO DISALLOW POKING OF H-O CLOSURES
 *
 */

static
PP_COMB_WARNING
showParams(COMB_PHR **params,
           int        sDepth,
           int        rDepth,
           char     **recNames,
           int        numParams)
{
  int              i;
  PP_COMB_WARNING  newRval;
  PP_COMB_WARNING  rval       = PP_DEPTH_OK;
  BBOOL            canNotPoke = BFALSE;

  for (i = 0; i < numParams; i++)
    {
      canNotPoke = BFALSE;

      if (recNames[i])     /* pair won't have destructor names */
        {
          ST_KEY destKey;

          appendBuff (recNames[i]);
          appendBuff (": ");

          destKey = st_NameToKey (recNames[i]);

          assert (destKey);

          if (st_IsHO (destKey))
            canNotPoke = BTRUE;
        }

      /* [H-O] ALTERED TO PRINT OUT BIVARIANT COMBINATOR PHRASES: */

      if (params[i]->positive)
        {
          if (canNotPoke)
            {
              appendBuff ("<function>");
              newRval = PP_DEPTH_OK;
            }
          else
            newRval = showCombExpr (params[i]->positive, sDepth, rDepth);

          if (newRval > rval)
            rval = newRval;

          if (params[i]->negative)
            appendBuff (" & ");
        }

      if (params[i]->negative)
        {
          assert (!canNotPoke);

          newRval = showCombExpr (params[i]->negative, sDepth, rDepth);

          if (newRval > rval)
            rval = newRval;
        }

      if (!params[i]->positive && !params[i]->negative)
        {
          assert (!canNotPoke);
          appendBuff ("_");
        }

      if (i != numParams - 1)
        appendBuff(", ");
    }

  return (rval);
}


/*********************************
 *                               *
 *    rightDisplayMode           *
 *                               *
 *********************************/
static void
rightDisplayMode(COMB_EXPR *combExpr, int maxShowDepth, int maxRecordDepth) {
/* !!!! This needs to be generalized */
  char input[MAX_INPUT_LENGTH];

  emptyInputLine();  getInputLine(input,MAX_INPUT_LENGTH);  restoreInputLine();

/*  if (kludge) {
    getInputLine(input,MAX_INPUT_LENGTH);
    kludge = 0;
  }
  getInputLine(input,MAX_INPUT_LENGTH);
*/

  if ((input[0] != 'q') && (input[0] != 'Q'))
    _combExprPrint(combExpr, maxShowDepth, ++maxRecordDepth);
}


/*********************************
 *                               *
 *    leftDisplayMode            *
 *                               *
 *********************************/
static void
leftDisplayMode(COMB_EXPR *combExpr, int maxShowDepth, int maxRecordDepth) {
/* !!!! This needs to be generalized */
  char input[MAX_INPUT_LENGTH];

  emptyInputLine();  getInputLine(input,MAX_INPUT_LENGTH);  restoreInputLine();

  if ((input[0] != 'q') && (input[0] != 'Q'))
    _combExprPrint(combExpr, ++maxShowDepth, maxRecordDepth);

}
