/******************************************************************************
 *                                                                            *
 *   Commands.c                                                               *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "commands.h"
#include "ioChar.h"
#include "parse.h"
#include "y.tab.h"
#include "symtab.h"
#include "codetab.h"
#include "ctTranslate.h"
#include "pm.h"
#include "typecheck.h"
#include "lib.h"

#define INCDIR_HEAP_SIZE 100

STR_LIST *g_strList_IncludeDirs;
MEMORY    incDirHD;

extern FILE *yyin;
extern BBOOL readingFile;
extern BBOOL parsing;

/*****************************************************************************
 *                                                                           *
 *            Prototypes For Internal Functions                              *
 *                                                                           *
 *****************************************************************************/
static CT_VAR_BASE *vb_pmTranslate(PE_VAR_BASE *vb);
static void         ge_ProcessSetCommand(PE_SETCOMM setcommand);
static int          ge_ProcessCommand(PE_COMM command);
static void         ge_ProcessQuery(PE_QUERY query);
static void         ge_ShowHelp(int fi_ForWhat);


/***********************
 *                     *
 * vb_pmTranslate      *
 *                     *
 ***********************/ 
static
CT_VAR_BASE
*vb_pmTranslate(PE_VAR_BASE *vb)
{
     CT_VAR_BASE *result = NULL;

     if (vb) {
	  result = (CT_VAR_BASE *) MemHeapAlloc(parseHeapDesc, 1, sizeof(CT_VAR_BASE));
	  switch (vb->tag) {
	     case VB_BANG:
	       result->tag = CT_VB_BANG;
	       break;
	     case VB_VAR:
	       result->tag = CT_VB_VAR;
	       result->info.var = vb->info.var;
	       break;
	     case VB_PAIR:
	       result->tag = CT_VB_PAIR;	       
	       result->info.pair.l = vb_pmTranslate(vb->info.vbpair.l);
	       result->info.pair.r = vb_pmTranslate(vb->info.vbpair.r);
	       break;
	     default:
	       printMsg(FATAL_MSG, "vb_pmTranslate - Unkown variable base");
	       break;
	  }
     }
     return(result);
}


/***********************
 *                     *
 * ProcessCmd cmd      *
 *                     *
 ***********************/ 
int
ProcessCmd(PARSE_RESULT *result)
{
     CT_EXPR     *ctExpr     = NULL;
     COMB_EXPR   *combExpr   = NULL;
     M_INSTR     *macroCode  = NULL;
     CT_VAR_BASE *var_base   = NULL;
     ST_TYPE     *st_type;
     ST_KEY       funKey;

     switch (result->tag) {
	case SETCOMMAND: 
	  ge_ProcessSetCommand(result->info.setcommand);
	  break;
	case COMMAND: 
	  return (ge_ProcessCommand(result->info.command));
	  break;
	case QUERY:
	  ge_ProcessQuery(result->info.query);
	  break;
	case DATA:
	  addDatatype(result->info.data);
	  break;
	case ALIAS:
	  addAlias (result->info.alias);
	  break;

	case DEF:
#if 0
printf("RHS PARSE TREE:\n");
display_PE_EXPR(result->info.def->expr, 0);
#endif
 	  funKey = st_AddFunction(result->info.def);

	  /* typecheck the term logic parse tree */
	  tc_open_typechecker();
	  tc_typecheck_PE_DEF(result->info.def, funKey);
	  /* if this point reached then typecheck of def was successful */

	  ctExpr = pmTranslate(result->info.def->expr,1);

          if ( printCT_EXPR ) {
 	      printMsg(MSG, "\nCore term logic for the function is: \n");
              printMsg(MSG, "def %s{%L} = %V =>", result->info.def->id,
                                                result->info.def->macros,
                                                var_base);
 	      printMsg(MSG, "%r\n", ctExpr);
          }   /*  fi  */

          /* close after typecheck & patt translation are complete */
          /* we need to remove DEF if patt translation fails */
	  tc_close_typechecker(0);

	  ct_TranslateOpen();

	  /* if function definition contains any macros, we must add the   */
	  /* environment to the variable base                              */
	  if (result->info.def->macros) {
	       var_base = VarBasePairNew(parseHeapDesc, 
					 vb_pmTranslate(result->info.def->var_base),
					 &ct_vb_env);
	  }
	  else 
	       var_base = vb_pmTranslate(result->info.def->var_base);

	  ctExpr = ctPreTranslate (ctExpr);     /* [#@] */

	  combExpr = ctTranslate(result->info.def->id,
				 var_base,
				 ctExpr);

	  printMsg(MSG,"Function added:  %s%S",st_KeyToName(funKey),st_GetTypeSig(funKey));

	  macroCode = Compile(result->info.def->id, combExpr); 
	  CodeTableAdd(result->info.def->id, combExpr,  
		       macroCode);

	  ct_TranslateClose();
	  break;

	case EXPR:
#if 0
printf("PARSE TREE:\n");
display_PE_EXPR(result->info.expr, 0);
#endif
	  /* typecheck the term logic parse tree */
	  tc_open_typechecker();
	  st_type = tc_typecheck_PE_EXPR(result->info.expr);
	  /* if this point reached then typecheck was successful */
	  /* don't close typechecker until AFTER result st_type is printed below */

	  ctExpr = pmTranslate(result->info.expr,0);

          if ( printCT_EXPR )
	      printMsg(MSG, "\nCore term logic for the expression is: \n%r\n", ctExpr);

	  ct_TranslateOpen();

	  ctExpr = ctPreTranslate (ctExpr);     /* [#@] */

	  combExpr = ctTranslate(NULL, vb_pmTranslate(VBbang()), ctExpr);

	  mc_MachineOpen();
	  combExpr = Evaluate(combExpr, st_type);
	  kludge = 1;
	  combExprPrint(combExpr,PP_MAX_SHOW_DEPTH,PP_MAX_RECORD_DEPTH,st_type);
	  mc_MachineClose();
	  tc_close_typechecker(0);
	  ct_TranslateClose();
	  break;
	case EMPTY_INPUT:
	  break;
	default:
	  printMsg(FATAL_MSG, "ProcessCmd - Invalid tag (%d)", result->tag);
	  break;
	}
     return(1);
}   /*  end ProcessCmd  */

/***********************
 *                     *
 *    Readfile         *
 *                     *
 ***********************/ 
void
Readfile(char *file)
{
     char openFile[MAX_STRING_LENGTH_DEFAULT];

     readingFile = BTRUE;
     
     if (FileChange(openFile, file)) {
       printMsg(MSG, "\n[ Opening %s ]", openFile);
       ParseStream();
       FileRestore();
       printMsg(MSG, "[ Closing %s ]", openFile);
     }    /*  fi  */
     else {
       readingFile = BFALSE;       
       strcpy(openFile, file);
       printMsg(ERROR_MSG, "Could not open %s.", file);
     }   /*  esle  */
}


/*********************************
 *                               *
 *    ge_ProcessSetCommand       *
 *                               *
 *********************************/
static void
ge_ProcessSetCommand(PE_SETCOMM setcommand) {

  switch (setcommand.tag) {
  case REPLACE :
    if (strcmp(setcommand.info.replace.entryType, "functions") == 0) 
      if (strcmp(setcommand.info.replace.doReplace, "true") == 0) {
	gb_ReplaceFunctions = BTRUE;
	printMsg(MSG, "All %s will now be replaced silently!", setcommand.info.replace.entryType);
      }
      else {
	gb_ReplaceFunctions = BFALSE;
	printMsg(MSG, "User will be prompted  before replacing %s.", setcommand.info.replace.entryType);
      }
    else
      printMsg(MSG, "Silent replacement for %s is not implemented yet.", setcommand.info.replace.entryType);
    break;
  case INCLUDEDIRS :
    MemReset(incDirHD);
    g_strList_IncludeDirs = libStrListdup(incDirHD, setcommand.info.dirList);
    printMsg(MSG,"Search path set to %L.",(LIST *)g_strList_IncludeDirs);
    break;
  case APPENDDIRS :
    g_strList_IncludeDirs = 
      StrListAppend(g_strList_IncludeDirs, 
		    libStrListdup(incDirHD, setcommand.info.dirList));
    printMsg(MSG,"Search path set to %L.",(LIST *)g_strList_IncludeDirs);
    break;
  case QUERY :
    ge_ShowHelp(SETCOMMAND);
    break;
  case PRINTCTEXPR :
    if (strcmp(setcommand.info.doPrintCT_EXPR, "true") == 0) {
	printCT_EXPR = BTRUE;
	printMsg(MSG, "Core term logic will now be printed.");
      }
      else {
	printCT_EXPR = BFALSE;
	printMsg(MSG, "Core term logic will not be printed.");
      }

    break;
  default: 
    printMsg(FATAL_MSG, "ge_ProcessSetCommand - Invalid tag (%d)", setcommand.tag);
  }
}


/*********************************
 *                               *
 *    ge_ProcessCommand          *
 *                               *
 *********************************/
static int
ge_ProcessCommand(PE_COMM command) {

  switch (command.tag) {
  case READFILE :
    Readfile(command.info.readfile);
    break;
  case QUIT: 
    return(0);
    break;
  case QUERY: 
    ge_ShowHelp(COMMAND);
    break;
  default: 
    printMsg(FATAL_MSG, "ge_ProcessCommand - Invalid tag (%d)", command.tag);
  }

  return(1);
}


/***************************************
 *                                     *
 *        initIncludeDirs              *
 *                                     *
 ***************************************/
void
initIncludeDirs(void) {

    incDirHD = MemAlloc("includeDirHeap", INCDIR_HEAP_SIZE, sizeof(char));
    g_strList_IncludeDirs = StrListCons(".", NULL, incDirHD);

}


/***************************************
 *                                     *
 *        ge_DestructIncDirs           *
 *                                     *
 ***************************************/
void
ge_DestructIncDirs(void) {

  MemDealloc(incDirHD);

}


/*********************************
 *                               *
 *    ge_StrListCons             *
 *                               *
 *********************************/
STR_LIST
*ge_StrListCons(char *x, STR_LIST *l)
{     
     return(StrListCons(x, l, incDirHD));
}


/*********************************
 *                               *
 *    ge_ProcessQuery            *
 *                               *
 *********************************/
static void
ge_ProcessQuery(PE_QUERY query) {

  switch (query.tag) {
  case STRNG :
    st_PrintEntryInfo(st_NameToKey(query.info.query));
    break;
  case ABOUT :
    clearBuff(); 
    appendBuff(CHARITY_CONT_PROMPT "Charity Interpreter version "CHARITY_VERSION " was written by \n"
			    CHARITY_CONT_PROMPT "     Charles Tuckey, \n"
			    CHARITY_CONT_PROMPT "     Peter Vesely and \n"
			    CHARITY_CONT_PROMPT "     Barry Yee \n"
			    CHARITY_CONT_PROMPT "from May to November, 1995.\n");
    outputBuff(stdout);
    break;
  case SHOWCOMB :
    st_PrintEntryInfo(st_NameToKey(query.info.showcomb));
    if (isFunction(query.info.showcomb)) {
      printMsg(MSG, "COMBINATOR DEFN for %s", query.info.showcomb);
      CodeTableShowComb(query.info.showcomb);
    }
    else if (isDatatype(query.info.showcomb)) {
      st_ShowDatatypeCombinators(st_NameToKey(query.info.showcomb));
    }
    else 
      ;   /* do nothing */
    break;
  case DUMPTABLE:
    st_DumpTable();
    break;
  case REPLACE:
    if (gb_ReplaceFunctions)
      printMsg(MSG, "Functions replaced silently.");
    else
      printMsg(MSG, "User prompted to replace functions.");
    printMsg(MSG, "User prompted to replace datatypes.");
    break;
  case INCLUDEDIRS:
    printMsg(MSG,"Search path is %L.",(LIST *)g_strList_IncludeDirs);
    break;
  case SHOWMEM:
    MemDisplayState();
    break;
  case QUERY:
    ge_ShowHelp(QUERY);
    break;
  default: 
    printMsg(FATAL_MSG, "ge_ProcessQuery - Invalid tag (%d)", query.tag);
  }
}


/*********************************
 *                               *
 *    ge_ShowHelp                *
 *                               *
 *********************************/
static void
ge_ShowHelp(int fi_ForWhat) {

  switch (fi_ForWhat) {
  case COMMAND : printMsg(MSG, "\n"
    "Command Help: \n"			 
    "   [r | R | rf | readfile] <fname>    - reads <fname> into charity \n"
    "   [q | Q | quit]                     - exits the charity interpreter \n"
    "   ?                                  - produces this help listing \n"
    );
    break;
  case SETCOMMAND : printMsg(MSG, "\n"
    "Set Command Help : \n"
    "   replace functions [true | false] - if true don't display replacement warnings \n"
    "   searchpath \"<dirname>\", ...    - sets up search path for charity files \n"   
    "   appendpath \"<dirname>\", ...    - appends <dirname>(s) to search path \n"   
    "   printCTEXPR [true | false]       - displays core term logic (or not)\n"
    "   ?                                - produces this help listing \n"
    );
    break;
  case QUERY : printMsg(MSG, "\n"
    "Query Command Help : \n"
    "   <ident>          - shows type info for <ident>\n"
    "   comb <ident>     - shows type info for <ident>. In addition,\n"
    "                      shows combinator code for functions and\n"
    "                      shows operation combinator types for datatypes.\n"
    "   dump table       - shows all entries in symbol table\n"
    "   mem use          - gives a listing of memory usage\n"
    "   set replace      - shows status of replacement prompts\n"
    "   set searchpath   - shows search path list\n"
    "   ?                - produces this help listing \n"
			);
    break;
  default: 
    printMsg(FATAL_MSG, "ge_ShowHelp - Invalid tag (%d)", fi_ForWhat);
  }

}   /*  ge_ShowHelp  */

