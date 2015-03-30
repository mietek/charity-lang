/******************************************************************************
 *                                                                            *
 *   printParse.c                                                             *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

/* Display routines for the parser  */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "parse.h"
#include "ioChar.h"
#include "symtab.h"
/*****************************************************************************
 *                                                                           *
 *                           internal prototypes                             *
 *                                                                           *
 *****************************************************************************/


static void printTypeSig         (PE_TYPE_SIG          *type_sig);

/* [H-O] ADDED THIS PROTOTYPE (SEE BELOW): */

static void printStructorTypeSig (PE_STRUCTOR_TYPE_SIG *sig);

static void printVarBase(PE_VAR_BASE *var_base);

static void printRecArr(P_STRUCTOR **rec);
static void printPStructor(P_STRUCTOR *structor);
static void printPatt(PE_PATT *parms);

/* [H-O] ALTERED THIS PROTOTYPE (SEE BELOW): */

static void printPhraseArr (PE_MAP_PHRASE *phrases, int numParams);

static void printTerm(PE_TERM *term);
static void printCase(PE_LIST_T_PHRASE *cases);



/***************
 * strings
 ***************/

/*********************************
 *                               *
 *    printString                *
 *                               *
 *********************************/
void
printString(char *str) { printf("%s", str); }


/*********************************
 *                               *
 *    showString                 *
 *                               *
 *********************************/
void
showString(char *str) { appendBuff(str); }


/*************************
 * display data definition
 *************************/

/*********************************
 *                               *
 *    printType                  *
 *                               *
 *********************************/
void
printType(PE_TYPE *type)
{
     assert(type);

     printf("%s(", type->ident);
     printList((LIST *) type->parms);
     printf(")");
}

/*********************************
 *                               *
 *    printTypeSig               *
 *                               *
 *********************************/

static
void
printTypeSig(PE_TYPE_SIG *type_sig)
{
     assert(type_sig);

     printType(type_sig->domain);
     printf(" -> ");
     printType(type_sig->codomain);
}

/*********************************
 *                               *
 *    printStructorTypeSig       *
 *                               *
 *********************************/

/* [H-O] ADDED TO HANDLE TYPE PE_STRUCTOR_TYPE_SIG (SEE parse.h): */

static
void
printStructorTypeSig (PE_STRUCTOR_TYPE_SIG *sig)
{
  assert (sig);

  printType (sig->domain);
  printf (" -> ");

  if (sig->param)
    {
      printType (sig->param);
      printf (" => ");
    }

  printType (sig->codomain);
}

/*********************************
 *                               *
 *    printStructor              *
 *                               *
 *********************************/
void
printStructor(PE_STRUCTOR *structor)
{
     assert(structor);

     printf("\t%s : ", structor->ident);

     /* [H-O] ALTERED TO HANDLE TYPE PE_STRUCTOR_TYPE_SIG (SEE parse.h): */

     printStructorTypeSig(structor->type_sig);

     printf("\n");
}

/*********************************
 *                               *
 *    displayDatadef             *
 *                               *
 *********************************/
void
displayDatadef(PE_DATA *data)
{
     assert(data);

     printf("\n");
     printf("TYPE %s(", data->domainId);
     printList((LIST *) data->domainVars);

     printf(") -> ");

     printf("%s(", data->codomainId);
     printList((LIST *) data->codomainVars);

     printf(")\n");

     printf("Structors:\n");
     printList((LIST *) data->structors);

     printf("\n");
     fflush(stdout);
}



/********************************/
/* Displaying routines for def  */
/********************************/

/*********************************
 *                               *
 *    printMacro                 *
 *                               *
 *********************************/
void
printMacro(PE_MACRO *macro)
{
     assert(macro);
     printf("%s", macro->ident);

     if (macro->type_sig) {
         printf(": ");
         printTypeSig(macro->type_sig);
     }
}

/*********************************
 *                               *
 *    peShowMacro                *
 *                               *
 *********************************/
void
peShowMacro(PE_MACRO *macro) {
     assert(macro);

     appendBuff(macro->ident);

#if 0
Not implemented yet
     if (macro->type_sig) {
         appendBuff(": ");
         showPETypeSig(macro->type_sig);
     }
#endif

}

/*********************************
 *                               *
 *    printPStructor             *
 *                               *
 *********************************/
static
void
printPStructor(P_STRUCTOR *structor)
{
     assert(structor);

     printf("%s:", structor->id);
     if (structor->arg)
          printPatt(structor->arg);
}

/*********************************
 *                               *
 *    printRecArray              *
 *                               *
 *********************************/
static
void
printRecArr(P_STRUCTOR **rec)
{
     int   count  = 0;
     char *parent = NULL;
     int   num    = 0;

     parent = getStructorParent(rec[0]->id);
     num = getNumStructors(parent);

     printf("(");
     for (count = 0; count < num; count++) {
          if (rec[count]) {
               if (count != 0)
                    printf(", ");

               printPStructor(rec[count]);
          }
     }
     printf(")");
}

/*********************************
 *                               *
 *    printPatt                  *
 *                               *
 *********************************/
static
void
printPatt(PE_PATT *patt)
{
     if (patt) {
          switch (patt->tag) {
            case P_BANG:
              printf("()");
              break;
             case P_VAR:
               printf("%s", patt->info.var);
               break;
             case P_HOVAR:
               printf("%s", patt->info.hovar.hovar);
               break;
             case P_PAIR:
               printf("(");
               printPatt(patt->info.ppair.l);
               printf(",");
               printPatt(patt->info.ppair.r);
               printf(")");
               break;
             case P_RECORD:
               printf("RECORD{");
               printRecArr(patt->info.record);
               printf("}");
               break;
             case P_CONSTR:
               printPStructor(patt->info.constr);
               break;
             default:
               printf("Error in printPatt\n");
               exit(-1);
               break;
          }
     }
}

/*********************************
 *                               *
 *    printT_Phrase              *
 *                               *
 *********************************/
void
printT_Phrase(PE_T_PHRASE *phrase)
{
     assert(phrase);

     printPatt(phrase->patt);
     printf(" => ");
     printExpr(phrase->expr);
}

/*********************************
 *                               *
 *    printCase                  *
 *                               *
 *********************************/
static
void
printCase(PE_LIST_T_PHRASE *t_case)
{
     printf("Case statement\n");
     assert(t_case);
     printList((LIST *) t_case);
}

/*********************************
 *                               *
 *    printRecord                *
 *                               *
 *********************************/
void
printRecord(PE_RECORD **record)
{
     int   count        = 0;
     char *parent       = NULL;
     int   numStructors = 0;
     assert(record);

     parent = getStructorParent(record[0]->destr);
     numStructors = getNumStructors(parent);

     printf("(");

     for (count = 0; count < numStructors; count++) {
          if (record[count]) {
               if (count != 0)
                    printf(", ");
               printf("%s:", record[0]->destr);
               printExpr(record[0]->expr);
          }
          else {
               printMsg(WARN_MSG, "printRecord - incomplete number of destructors");
               /* !!!! set parse error flag */
          }
     }
     printf(")");
}

/*********************************
 *                               *
 *    printFold                  *
 *                               *
 *********************************/
void
printFold(PE_FOLD **fold)
{
     int   count        = 0;
     int   numStructors = 0;
     char *parent       = NULL;

     assert(fold);

     parent = getStructorParent(fold[0]->constr);
     numStructors = getNumStructors(parent);

     printf("{| ");
     for (count = 0; count < numStructors; count++) {
          if (fold[count]) {
               if (count) printf(" | ");
               printf("%s:", fold[count]->constr);
               printList((LIST *) fold[count]->phrases);
          }
          else {
               printMsg(WARN_MSG, "printFold - incomplete number of contructors");
               /* !!!! set parse error flag */
          }
     }
     printf("|}");
}


/*********************************
 *                               *
 *    printPhraseArr             *
 *                               *
 *********************************/

/* [H-O] ALTERED THIS FUNCTION TO HANDLE MULTIVARIANT MAPS: */

static
void
printPhraseArr (PE_MAP_PHRASE *phrases,
                int            numParams)
{
  int index = 0;

  assert (phrases);
  assert (numParams > 0);

  for (index = 0; index < numParams; index++)
    {
      if (phrases[index].positive)
        {
          printList ((LIST *)phrases[index].positive);

          if (phrases[index].negative)
            printf (" & ");
        }

      if (phrases[index].negative)
        printList ((LIST *)phrases[index].negative);

      if (!phrases[index].positive && !phrases[index].negative)
        printf ("_");

      if (index < numParams - 1)
        printf (", ");
    }
}


/*********************************
 *                               *
 *    printMap                   *
 *                               *
 *********************************/

/* [H-O] ALTERED THIS FUNCTION TO HANDLE MULTIVARIANT MAPS: */

void
printMap (PE_MAP *map)
{
  assert (map);

  printf ("%s{", map->type_name);
  printPhraseArr (map->phrases, getNumParams (map->type_name));
  printf ("}");
}


/*********************************
 *                               *
 *    printUnfold                *
 *                               *
 *********************************/
void
printUnfold(PE_UNFOLD **unfold)
{
     int   numStructors  = 0;
     int   count         = 0;
     char *parentType    = NULL;
     assert(unfold);

     parentType = getStructorParent(unfold[0]->destr);
     numStructors = getNumStructors(parentType);

     printf("(| ");
     for (count = 0; count < numStructors; count++) {
          if (unfold[count]) {
               if (count) printf("\n | ");
               printf("%s:", unfold[count]->destr);
               printList((LIST *) unfold[count]->phrases);
          }
     }

     printf("|) ");
}

/*********************************
 *                               *
 *    printTerm                  *
 *                               *
 *********************************/
static
void
printTerm(PE_TERM *term)
{
     assert(term);

     switch (term->tag) {
        case T_STRUCTOR:
          printf("%s", term->info.struct_name);
          break;
        case T_MACRO:
          printf("%s", term->info.macro->macro_name);
          break;
        case T_FUNCTION:
          printf("%s", term->info.function->fun_name);
          break;
        case T_CASE:
          printCase(term->info.cases);
          break;
        case T_RECORD:
          printRecord(term->info.records);
          break;
        case T_FOLD:
          printFold(term->info.folds);
          break;
        case T_MAP:
          printMap(term->info.maps);
          break;
        case T_UNFOLD:
          printUnfold(term->info.unfolds);
          break;
        default:
          printf("printTerm(): term not recognized\n");
          exit(-1);
          break;
     }
}


/*********************************
 *                               *
 *    printExpr                  *
 *                               *
 *********************************/
void
printExpr(PE_EXPR *expr)
{
     if (expr) {
          switch (expr->tag) {
             case E_VAR:
               printf("VAR(%s)", expr->info.var);
               break;
             case E_APP:
               printf("APP(");
               printTerm(expr->info.app.term);
               printf(", ");
               printExpr(expr->info.app.expr);
               printf(")");
               break;
             case E_PAIR:
               printf("PAIR(");
               printExpr(expr->info.epair.l);
               printf(", ");
               printExpr(expr->info.epair.r);
               printf(")");
               break;
             case E_BANG:
               printf("()");
               break;
             default:
               printf("printExpr: Error\n");
               exit(-1);
               break;
          }
     }
}

/*********************************
 *                               *
 *    printVarBase               *
 *                               *
 *********************************/
static
void
printVarBase(PE_VAR_BASE *var_base)
{
     if (var_base) {
          switch (var_base->tag) {
             case VB_BANG:
               printf("()");
               break;
             case VB_VAR:
               printf("%s", var_base->info.var);
               break;
             case VB_PAIR:
               printf("(");
               printVarBase(var_base->info.vbpair.l);
               printf(",");
               printVarBase(var_base->info.vbpair.r);
               printf(")");
               break;
             default:
               printf("printExpr: Error\n");
               exit(-1);
               break;
          }
     }
}

/*********************************
 *                               *
 *    displayDef                 *
 *                               *
 *********************************/
void
displayDef(PE_DEF *def)
{
     assert(def);

     printf("FUNCTION: %s {", def->id);

     printList((LIST *) def->macros);

     printf("} ");

     if (def->type_sig) {
          printf(": ");
          printTypeSig(def->type_sig);
     }

     printf(" (");
     printVarBase(def->var_base);
     printf(") = \n");

     printExpr(def->expr);
     printf("\n");
}

