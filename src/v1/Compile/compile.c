/******************************************************************************
 *                                                                            *
 *   compile.c                                                                *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "lib.h"
#include "compile.h"
#include "decompile.h"
#include "list.h"
#include "pmem.h"
#include "emit.h"
#include "codetab.h"
#include "symtab.h"
#include "machine.h"
#include "ctTranslate.h"
#include "ioChar.h"

#define MAX_STRING    256
#define TMP_MEM_SIZE  100000
#define MAX_PRIM_COMB 10

#define max(x,y) (x > y) ? x : y

/*****************************************************************************
 *                                                                           *
 *                           internal prototypes                             *
 *                                                                           *
 *****************************************************************************/

static char    *_getType(char *comb);
static BBOOL    _isIdentityComb(COMB_EXPR *cexpr);
static BBOOL    _isP1Comb(COMB_EXPR *cexpr);

static void     _CompileSC(COMB_EXPR *cexpr);
static void     _CompileC(COMB_EXPR *cexpr);
static void     _CompileConstr(COMB_EXPR *cexpr);
static void     _CompileDestr(COMB_EXPR *cexpr);
static void     _CompileFunction(COMB_EXPR *cexpr);
static void     _CompilePrimitive(COMB_EXPR *cexpr);
static void     _CompileMacro(COMB_EXPR *cexpr);

static void _CompileInt  (COMB_EXPR *cexpr);     /* [BI] ADDED (SEE BELOW) */
static void _CompileChar (COMB_EXPR *cexpr);     /* [BI] ADDED (SEE BELOW) */

static void _CompileAt (void);     /* [#@] */

static void     _CompilePass2(M_INSTR *start);
static M_INSTR *_Compile(struct _COMB_EXPR *code);

static M_INSTR *_CompileCase(int numParms, COMB_PHR **parms, M_INSTR *ret);
static M_INSTR *_CompileRecord(int numParms, COMB_PHR **parms, char *parentTypeName);
static M_INSTR *_CompileFold(char *comb, int numParms, COMB_PHR **parms, COMB_EXPR *state);
static M_INSTR *_CompileMapInduct(char *comb, COMB_PHR **parms, COMB_EXPR *state);
static M_INSTR *_CompileMapCoinduct(char *comb, COMB_PHR **parms,
				    char *parentType, COMB_EXPR *state);
static M_INSTR *_CompileUnfold(int numParms, COMB_PHR **parms,
			       char *parentType, COMB_EXPR *state);
static M_INSTR *_CompileFunctionParam(COMB_EXPR *expr);

static M_INSTR *_CompileCata  (int         numParms,     /* [#@] */
			       COMB_PHR  **parms,
			       COMB_EXPR  *state);
static M_INSTR *_CompileAna   (int         numParms,
			       COMB_PHR  **parms,
			       COMB_EXPR  *state,
			       char       *parentType);
static void     SelfReference (COMB_EXPR **expr,
			       int         selfTag,
			       COMB_EXPR  *selfExpr);

static void     _CompileParms(M_INSTR *instr);

static M_INSTR_TAG  _lookupComb(char *name);

static void         _AddPrim(char *id, M_INSTR_TAG tag);

/* [H-O] ALTERED PROTOTYPE (SEE BELOW): */

static COMB_PHR **makeP0Arr (int num, MEMORY heapHD);


/*****************************************************************************
 *                                                                           *
 *                           internal variables                              *
 *                                                                           *
 *****************************************************************************/

typedef struct _ALIAS {
     char       *id;
     M_INSTR    *macro_code;
     COMB_EXPR  *cexpr;
} ALIAS;

typedef struct _PRIMITIVE_COMB {
     char         *ch_id;
     M_INSTR_TAG   MC_id;
} PRIMITIVE_COMB;

static int primCount; /* primitive combinators */

static PRIMITIVE_COMB combTbl[MAX_PRIM_COMB];

static MEMORY tmpHD    = -1;
static MEMORY aliasHD  = -1;
static MEMORY strHD    = -1;

/*****************************************************************************
 *                                                                           *
 *                           lists for ALIASES                               *
 *                                                                           *
 *****************************************************************************/

static CP_LIST_ALIAS      *AliasListCons(ALIAS *x, CP_LIST_ALIAS *l);
static BBOOL               AliasListMemberCexpr(CP_LIST_ALIAS *alist, COMB_EXPR *expr);
static ALIAS              *AliasListFind(CP_LIST_ALIAS *alist, COMB_EXPR *expr);
static ALIAS              *AliasListHead(CP_LIST_ALIAS *list);
static CP_LIST_ALIAS      *AliasListTail(CP_LIST_ALIAS *list);


static ALIAS      *AliasNew(M_INSTR *macro_code, COMB_EXPR *expr);


/***************************************
 *                                     *
 *        AliasListCons                *
 *                                     *
 ***************************************/
static
CP_LIST_ALIAS
*AliasListCons(ALIAS *x, CP_LIST_ALIAS *l)
{
     return((CP_LIST_ALIAS *) _cons((char *) x, (LIST *) l, L_ALIAS, aliasHD));
}

/***************************************
 *                                     *
 *        AliasListMemberCexpr         *
 *                                     *
 ***************************************/
static 
BBOOL             
AliasListMemberCexpr(CP_LIST_ALIAS *alist, COMB_EXPR *expr)
{
     CP_LIST_ALIAS *list = alist;
     ALIAS         *item = NULL;
     while (list) {
	  item = AliasListHead(list);
	  if (item->cexpr == expr) {
	       return(BTRUE);
	  }
	  list = AliasListTail(list);
     }
     return(BFALSE);
}

/***************************************
 *                                     *
 *        AliasListFind                *
 *                                     *
 ***************************************/
static
ALIAS
*AliasListFind(CP_LIST_ALIAS *alist, COMB_EXPR *expr)
{
     CP_LIST_ALIAS *list = alist;
     ALIAS         *head = NULL;

     while (list) {
	  head = AliasListHead(list);
	  if (head->cexpr == expr) {
	       return(head);
	  }
	  list = AliasListTail(list);
     }
     return(NULL);
}

/***************************************
 *                                     *
 *        AliasListHead                *
 *                                     *
 ***************************************/
static 
ALIAS
*AliasListHead(CP_LIST_ALIAS *list)
{
     return( (ALIAS *) ListHead( (LIST *) list )) ;
}

/***************************************
 *                                     *
 *        AliasListTail                *
 *                                     *
 ***************************************/
static
CP_LIST_ALIAS
*AliasListTail(CP_LIST_ALIAS *list)
{
     return( (CP_LIST_ALIAS *) ListTail( (LIST *)list) );
}


/*****************************************************************************
 *                                                                           *
 *                           Compiler functions                              *
 *                                                                           *
 *****************************************************************************/

/***************************************
 *                                     *
 *        AliasNew                     *
 *                                     *
 ***************************************/
static
ALIAS
*AliasNew(M_INSTR *macro_code, COMB_EXPR *expr)
{
     ALIAS *result = (ALIAS *) MemHeapAlloc(tmpHD, 1, sizeof(ALIAS));

     assert(macro_code);
     assert(expr);

     result->macro_code = macro_code;
     result->cexpr = expr;

     return(result);
}

/***************************************
 *                                     *
 *           AddPrim                   *
 *                                     *
 ***************************************/
static
void
_AddPrim(char *id, M_INSTR_TAG tag)
{
     
     if (primCount>= MAX_PRIM_COMB) {
	  printf("_AddPrim(): Can not add primitive combinator %s\n", id);
	  exit(-1);
     }
     combTbl[primCount].ch_id = id;
     combTbl[primCount].MC_id = tag;
     primCount++;
}

/***************************************
 *                                     *
 *          CompilerConstruct          *
 *                                     *
 ***************************************/
void 
CompilerConstruct(void)
{
     EmitConstruct();
     strHD   = MemAlloc("compile string scratch", TMP_MEM_SIZE, sizeof(char));

     primCount = 0;

     _AddPrim("pair",   MCpair);
     _AddPrim("!",      MCbang);
     _AddPrim(RES_ID,   MCret);
     _AddPrim(RES_ID1,  MCbang);
     _AddPrim(PROD0,    MCp0);
     _AddPrim(PROD1,    MCp1);
     _AddPrim("map_1",  MCp0);
     _AddPrim(MAP_PROD, MCmap_prod);
     _AddPrim(NULL,     MCinvalid);
}

/***************************************
 *                                     *
 *            CompilerReset            *
 *                                     *
 ***************************************/
void
CompilerReset(void)
{
     MemReset(strHD);
     EmitReset();
}

/***************************************
 *                                     *
 *          CompilerDestruct           *
 *                                     *
 ***************************************/
void
CompilerDestruct(void)
{
     MemDealloc(strHD);
     EmitDestruct();
}


/***************************************
 *                                     *
 *            makeP0Arr                *
 *                                     *
 ***************************************/

/* [H-O] EXTENDED TO HANDLE MULTIVARIANT PHRASES: */

static
COMB_PHR **
makeP0Arr (int    num,
	   MEMORY heapDesc)
{
  COMB_PHR  **P0arr = NULL;
  COMB_EXPR  *P0    = NULL;
  int         count = 0;

  if (num > 0)
    {
      P0arr = (COMB_PHR **)MemHeapAlloc (heapDesc, num, sizeof (COMB_PHR *));
      P0    = CombExprP0 (heapDesc);

      for (count = 0; count < num; count++)
	{
	  P0arr[count] = (COMB_PHR *)MemHeapAlloc (heapDesc,
						   1,
						   sizeof (COMB_PHR));

	  P0arr[count]->positive = P0;
	  P0arr[count]->negative = P0;
	}
    }

  return P0arr;
}


/***************************************
 *                                     *
 *            lookupComb               *
 *                                     *
 ***************************************/
static
M_INSTR_TAG
lookupComb(char *name)
{
     int count = 0;

     assert(name);

     while ((combTbl[count].ch_id != NULL)) {
	  if (strcmp(combTbl[count].ch_id, name) == 0)
	       return(combTbl[count].MC_id);
	  else
	       count++;
     }

     return(MCinvalid);
}

/***************************************
 *                                     *
 *           _isIdentityComb           *
 *                                     *
 ***************************************/
static
BBOOL
_isIdentityComb(COMB_EXPR *cexpr)
{
     BBOOL result = BFALSE;

     if (cexpr->tag == CTT_COMBINATOR &&
         cexpr->info.combinator.class == CC_PRIMITIVE &&
         strcmp(cexpr->info.combinator.name, "id") == 0) {
          result = BTRUE;
     }

     return(result);
}

/***************************************
 *                                     *
 *           _isP1Comb                 *
 *                                     *
 ***************************************/
static
BBOOL
_isP1Comb(COMB_EXPR *cexpr)
{
     BBOOL result = BFALSE;

     if (cexpr->tag == CTT_COMBINATOR &&
         cexpr->info.combinator.class == CC_PRIMITIVE &&
         strcmp(cexpr->info.combinator.name, PROD1) == 0) {
          result = BTRUE;
     }

     return(result);
}

/***************************************
 *                                     *
 *           _CompilePrimitive         *
 *                                     *
 ***************************************/
static
void
_CompilePrimitive(COMB_EXPR *cexpr)
{
     COMB_PHR **parms = cexpr->info.combinator.param;  /* H-O */

     assert(cexpr);
     
     emitCode.instr = lookupComb(cexpr->info.combinator.name);
     switch (emitCode.instr) {
	case MCpair:         /* H-O */
         if (_isIdentityComb(parms[1]->positive)) { /* <f, Id> => push.f.pair */
               emitCode.instr = MCsave;
               emit();
               
               _CompileSC(parms[0]->positive);  /* H-O */
               
               emitCode.instr = MCpair;
               emit();         /* H-O */
          } else if (_isP1Comb(parms[1]->positive)) { /* <f, p1> => pr.f.pair */
               emitCode.instr = MCpr;
               emit();
               
               _CompileSC(parms[0]->positive);  /* H-O */
               
               emitCode.instr = MCpair;
               emit();
          } else {                          /* <f, g>  => push.g.swap.f.pair */
               emitCode.instr = MCsave;
               emit();
               
               assert(parms);
               assert(parms[1]);
               assert(parms[1]->positive);
               _CompileSC(parms[1]->positive);  /* H-O */

               emitCode.instr = MCswap;
               emit();
               
               assert(parms[0]);
               assert(parms[0]->positive);
               _CompileSC(parms[0]->positive);  /* H-O */
               emitCode.instr = MCpair;
               emit();
          }
	  break;
	case MCmap_prod:
          emitCode.instr = MCmap_prod;
          emit();
          
          assert(parms);
          assert(parms[1]);
          assert(parms[1]->positive);
          _CompileSC(parms[1]->positive);  /* H-O */

          emitCode.instr = MCswap;
          emit();

          assert(parms[0]);
          assert(parms[0]->positive);
          _CompileSC(parms[0]->positive);  /* H-O */
          emitCode.instr = MCpair;
          emit();
	  break;
	case MCbang:
	case MCp0:
	case MCp1:
	  emit();
	  break;
	case MCret:
	  break;
	default:
	  printMsg(FATAL_MSG, "_CompilePrimitive - Unknown primitive instruction");
	  break;
     }
}

/***************************************
 *                                     *
 *            CompileConstr            *
 *                                     *
 ***************************************/
static
void
_CompileConstr(COMB_EXPR *cexpr)
{
     assert(cexpr);
     emitCode.instr             = MCcons;
     emitCode.info.structorPosn = getStructorPosn(cexpr->info.combinator.name) + 1;     /* [#@] */
     emit();
}


/***************************************
 *                                     *
 *           _CompileAt                *
 *                                     *
 ***************************************/

/* [#@] */

static
void
_CompileAt (void)
{
  emitCode.instr = MCat;

  emit ();
}


/***************************************
 *                                     *
 *           _CompileInt               *
 *                                     *
 ***************************************/

/* [BI] ADDED THIS FUNCTION TO HANDLE BUILTIN INTEGERS: */

static
void
_CompileInt (COMB_EXPR *cexpr)
{
  assert (cexpr);

  emitCode.instr  = MCint;
  emitCode.info.i = cexpr->info.combinator.i;

  emit ();
}


/***************************************
 *                                     *
 *           _CompileChar              *
 *                                     *
 ***************************************/

/* [BI] ADDED THIS FUNCTION TO HANDLE BUILTIN CHARACTERS: */

static
void
_CompileChar (COMB_EXPR *cexpr)
{
  assert (cexpr);

  emitCode.instr  = MCchar;
  emitCode.info.c = cexpr->info.combinator.c;

  emit ();
}


/***************************************
 *                                     *
 *           _CompileInduct            *
 *                                     *
 ***************************************/
static
void
_CompileInduct(COMB_EXPR *cexpr)
{
     char        *name = NULL;
     M_INSTR     *rec  = NULL;

    assert(cexpr);

     switch (cexpr->info.combinator.class) {
	case CC_MAP_I:
        case CC_CATA:      /* [#@] */
	case CC_FOLD:
	case CC_CASE:
          assert (cexpr->info.combinator.class != CC_FOLD);     /* [#@] */

	  emitCode.instr = MCinduct;        /* use constructor on V as offset into code */
	  emitCode.cexpr = cexpr;
	  emit();
	  break;
	default:
	  printMsg(FATAL_MSG, "_CompileInduct:  invalid class.");
	  exit(1);
     }
}


/***************************************
 *                                     *
 *          _CompileDestr              *
 *                                     *
 ***************************************/

/* [H-O] EXTENDED THIS FUNCTION TO HANDLE H-O DESTRUCTORS: */

static
void
_CompileDestr (COMB_EXPR *cexpr)
{
  assert (cexpr);

  if (st_IsHO (st_NameToKey (cexpr->info.combinator.name)))
    emitCode.instr = MCdestHO;
  else
    emitCode.instr = MCdest;

  emitCode.info.structorPosn =
    getStructorPosn (cexpr->info.combinator.name) + 1;

  emit ();

  emitCode.instr = MCreload;
  emit ();
}


/***************************************
 *                                     *
 *         _CompileFunctionParam       *
 *                                     *
 ***************************************/
static
M_INSTR 
*_CompileFunctionParam(COMB_EXPR *expr)
{
     M_INSTR *result = NULL;

     result = EmitAddr();
     _CompileSC(expr);
     return(result);
}


/***************************************
 *                                     *
 *           _CompileFunction          *
 *                                     *
 ***************************************/

/* [BI] ALTERED THIS FUNCTION TO HANDLE BUILTIN FUNCTIONS: */

static
void
_CompileFunction (COMB_EXPR *cexpr)
{
  int           count  = 0;
  COMB_PHR    **fparms = NULL;     /* [H-O] */
  M_INSTR      *parm1  = NULL;
  ST_KEY        key;
  M_INSTR_TAG   instr;

  assert (cexpr);
  assert (cexpr->tag == CTT_COMBINATOR);
  assert (cexpr->info.combinator.class == CC_FUNCTION);
  assert (cexpr->info.combinator.name);

  key = st_NameToKey (cexpr->info.combinator.name);

  assert (key);

  instr = st_GetInstruction (key);

  if (instr != MCinvalid)
    {
      emitCode.instr = instr;
      emit ();
    }
  else
    {
      fparms = cexpr->info.combinator.param;

      if (fparms) {
	emitCode.instr          = MCldparm;
	emitCode.info.macroList = EmitParm(cexpr->info.combinator.numParams);
	emitCode.cexpr          = cexpr;
	emit();
      }

      emitCode.instr         = MCfunc;
      emitCode.info.funcCode = CodeTableGetMC(cexpr->info.combinator.name);
      emit();

      if (fparms) {
	emitCode.instr = MCunload;
	emit();
      }
    }
}


/***************************************
 *                                     *
 *           _CompileMacro             *
 *                                     *
 ***************************************/
static
void
_CompileMacro(COMB_EXPR *cexpr)
{
     int            count  = 0;
     COMB_PHR     **fparms = NULL; /* H-O */
     M_INSTR       *parm1  = NULL;

     assert(cexpr);
     assert(cexpr->info.combinator.class == CC_MACRO);
     
     fparms = cexpr->info.combinator.param;

     if (fparms) {
	  emitCode.instr          = MCldparm;
	  emitCode.info.macroList = EmitParm(cexpr->info.combinator.numParams);
	  emitCode.cexpr          = cexpr;
	  emit();
     }

     emitCode.instr          = MCparm;
     emitCode.info.macroParm = cexpr->info.combinator.parameter;
     emit();

     if (fparms) {
	  emitCode.instr = MCunload;
	  emit();
     }
}

/***************************************
 *                                     *
 *          _CompileParms              *
 *                                     *
 ***************************************/
static
void
_CompileParms(M_INSTR *instr)
{
     COMB_EXPR  *expr      = NULL;
     COMB_PHR  **combArgs  = NULL;   /* H-O */
     M_INSTR   **args      = NULL;
     int         count     = 0;

     assert(instr->instr == MCldparm);
     args = instr->info.macroList;
     expr = instr->cexpr;
     assert(expr);
     combArgs = expr->info.combinator.param;

     for (count = 0; count < expr->info.combinator.numParams; count++) {
	  args[count] = EmitAddr();
	  _CompileSC(combArgs[count]->positive); /* H-O */

	  emitCode.instr = MCreload;
	  emit();
	  emitCode.instr = MCret;
	  emit();

     }
}

/***************************************
 *                                     *
 *          _CompileC                  *
 *                                     *
 ***************************************/
static
void
_CompileC(COMB_EXPR *cexpr)
{
     assert(cexpr);

     switch (cexpr->info.combinator.class) { 
        case CC_PRIMITIVE:
	  _CompilePrimitive(cexpr);
          break;
        case CC_UNFOLD:
        case CC_RECORD:
	case CC_MAP_C:
	  assert (cexpr->info.combinator.class != CC_UNFOLD);     /* [#@] */

	  emitCode.instr = MCjump;
	  emitCode.cexpr = cexpr;
	  emit();
	  break;

	case CC_ANA:     /* [#@] */

	  emitCode.instr = MCjumpc;
	  emitCode.cexpr = cexpr;
	  emit ();

	  break;

        case CC_CONSTRUCTOR:
	  _CompileConstr(cexpr);
	  break;
	case CC_CATA:     /* [#@] */
        case CC_FOLD:
        case CC_CASE:
        case CC_MAP_I:
	  assert (cexpr->info.combinator.class != CC_FOLD);     /* [#@] */

	  _CompileInduct(cexpr);
          break;
        case CC_DESTRUCTOR:
	  _CompileDestr(cexpr);
          break;
        case CC_FUNCTION:
	  _CompileFunction(cexpr);
          break;
	case CC_MACRO:
	  _CompileMacro(cexpr);
	  break;

	case CC_BUILTIN_INT:        /* [BI] ADDED (SEE ABOVE) */
	  _CompileInt (cexpr);

	  break;

	case CC_BUILTIN_CHAR:       /* [BI] ADDED (SEE ABOVE) */
	  _CompileChar (cexpr);

	  break;

	case CC_AT:          /* [#@] */
	  _CompileAt ();

	  break;

        default:
          printMsg(FATAL_MSG, "_CompileC():  Combinator class not def.");
     }
}

/***************************************
 *                                     *
 *            _CompileSC               *
 *                                     *
 ***************************************/
static
void
_CompileSC(COMB_EXPR *cexpr)
{
     assert(cexpr);

     switch (cexpr->tag) {
        case CTT_COMPOSITION:
          _CompileSC(cexpr->info.composition.l);
          _CompileSC(cexpr->info.composition.r);
          break;
        case CTT_COMBINATOR:
	  _CompileC(cexpr);
          break;
        default:
          fprintf(stderr, "_CompileSC(): Internal error: trying to Compile\n");
	  exit(1);
          break;
     }
}


/***************************************
 *                                     *
 *           _CompileRecord            *
 *                                     *
 ***************************************/

/* [H-O] EXTENDED THIS FUNCTION TO HANDLE H-O RECORDS: */

static
M_INSTR *
_CompileRecord (int        numParms,
		COMB_PHR **parms,
		char      *parentTypeName)
{
  M_INSTR  *recordStart = NULL;
  M_INSTR  *curr        = NULL;
  int       count       = 0;
  char    **destrs      = NULL;

  assert (numParms > 0);
  assert (parms);
  assert (parentTypeName);

  destrs = getStructorNames (parentTypeName);

  recordStart = EmitAddr ();

  EmitMsg ("Record Table");

  emitCode.instr         = MCalloc;
  emitCode.info.allocNum = numParms + 1;
  emit ();

  emitCode.instr          = MCldmacroframe;
  emitCode.info.numDestrs = GC_BUILTIN - numParms;     /* [BI] ALTERED (SEE machine_private.h) */
  emit ();

  curr = EmitAddr ();

  /* SET UP THE ARRAY---OFFSET TABLE FOR THE PHRASES: */

  for (count = 0; count < numParms; count++)
    {
      emitCode.instr = MCclosure;
      emit ();
    }

  curr->instr = MCbclosure;

  emitCode.instr = MCret;
  emit ();

  /* FILL IN POINTERS TO THE COMPILED CODE: */

  for (count = 0; count < numParms; count++)
    {
      BBOOL isFO = !st_IsHO (st_NameToKey (destrs[count]));

      EmitMsg ("Records");

      (curr + count)->info.closureCode = EmitAddr ();

      if (isFO)
	{
	  emitCode.instr = MCmkupdate;
	  emit ();
	}

      _CompileSC (parms[count]->positive);

      if (isFO)
	{
	  emitCode.instr          = MCupdate;      /* DOES IMPLICIT MCret */
	  emitCode.info.updateClo = count + 1;
	  emit ();
	}
      else
	{
	  emitCode.instr = MCret;
	  emit ();
	}
    }

  return recordStart;
}


/***************************************
 *                                     *
 *           _CompileCase              *
 *                                     *
 ***************************************/
static
M_INSTR
*_CompileCase(int numParms, COMB_PHR **parms, M_INSTR *ret)  /* H-O */
{
     M_INSTR    *caseStart = NULL;
     int         count     = 0;

     assert(numParms > 0);
     assert(parms);
     assert(ret);

     caseStart = EmitAddr();

     EmitMsg("Case Table");

     /* set up the array -- offset table for the phrases */
     for (count = 0; count < numParms; count++) {
	  emitCode.instr = MCgoto;
	  emitCode.cexpr = NULL;
	  emit();
     }

     /* fill in pointers to the compiled code */
     for (count = 0; count < numParms; count++) {
	  EmitMsg("Cases");

	  (caseStart + count)->info.inductCode = EmitAddr();
	  _CompileSC(parms[count]->positive);  /* H-O */

	  /* set return from executing phrase */
	  emitCode.instr     = MCret;
	  emit();     
     }

     return(caseStart);
}

/***************************************
 *                                     *
 *           _getType                  *
 *                                     *
 ***************************************/
static
char
*_getType(char *comb)
{
     return(libStrdup(strHD, 
		      st_GetOpCombParentByName(comb)));
}


/***************************************
 *                                     *
 *           _CompileAna               *
 *                                     *
 ***************************************/

/* [#@] */

static
M_INSTR *
_CompileAna (int         numParms,
	     COMB_PHR  **parms,
	     COMB_EXPR  *state,
	     char       *parentType)
{
  M_INSTR  *start  = NULL;
  M_INSTR  *curr   = NULL;
  int       count  = 0;
  char    **destrs = NULL;

  assert (numParms > 0);
  assert (parms);
  assert (state);
  assert (state->tag == CTT_COMBINATOR);
  assert (state->info.combinator.class == CC_ANA);
  assert (parentType);

  destrs = getStructorNames (parentType);

  SelfReference (&state, state->info.combinator.self, state);

  start = EmitAddr ();

  EmitMsg ("Ana Table");

  emitCode.instr         = MCalloc;
  emitCode.info.allocNum = numParms + 1;
  emit ();

  emitCode.instr          = MCldmacroframe;
  emitCode.info.numDestrs = GC_BUILTIN - numParms;
  emit ();

  curr = EmitAddr ();

  for (count = 0; count < numParms; count++)
    {
      emitCode.instr = MCclosure;
      emit ();
    }

  curr->instr = MCbclosure;

  emitCode.instr = MCret;
  emit ();

  for (count = 0; count < numParms; count++)
    {
      BBOOL isFO = !st_IsHO (st_NameToKey (destrs[count]));

      EmitMsg ("Anas");

      curr[count].info.closureCode = EmitAddr ();

      if (isFO)
	{
	  emitCode.instr = MCmkupdate;
	  emit ();
	}

      _CompileSC (parms[count]->positive);

      if (isFO)
	{
	  emitCode.instr          = MCupdate;
	  emitCode.info.updateClo = count + 1;
	  emit ();
	}
      else
	{
	  emitCode.instr = MCret;
	  emit ();
	}
    }

  return start;
}


/***************************************
 *                                     *
 *           _CompileCata              *
 *                                     *
 ***************************************/

/* [#@] */

static
M_INSTR *
_CompileCata (int         numParms,
	      COMB_PHR  **parms,
	      COMB_EXPR  *state)
{
  M_INSTR *start = NULL;
  int      count = 0;

  assert (numParms > 0);
  assert (parms);
  assert (state);
  assert (state->tag == CTT_COMBINATOR);
  assert (state->info.combinator.class == CC_CATA);

  SelfReference (&state, state->info.combinator.self, state);

  start = EmitAddr ();

  EmitMsg ("Cata Table");

  for (count = 0; count < numParms; count++)
    {
      emitCode.instr = MCgoto;
      emit ();
    }

  for (count = 0; count < numParms; count++)
    {
      EmitMsg ("Catas");

      (start + count)->info.code = EmitAddr ();

      _CompileSC (parms[count]->positive);

      emitCode.instr = MCret;
      emit ();
    }

  return start;
}


/***************************************
 *                                     *
 *           SelfReference             *
 *                                     *
 ***************************************/

/* [#@] */

static
void
SelfReference (COMB_EXPR **expr,
	       int         selfTag,
	       COMB_EXPR  *selfExpr)
{
  assert (expr);
  assert (*expr);
  assert (selfExpr);

  switch ((*expr)->tag)
    {
    case CTT_COMPOSITION:
      SelfReference (&((*expr)->info.composition.l), selfTag, selfExpr);
      SelfReference (&((*expr)->info.composition.r), selfTag, selfExpr);

      break;

    case CTT_COMBINATOR:
      if ((*expr)->info.combinator.class == CC_SELF)
	{
	  if ((*expr)->info.combinator.self == selfTag)
	    *expr = selfExpr;
	}
      else
	{
	  int index     = 0;
	  int numParams = (*expr)->info.combinator.numParams;

	  for (index = 0; index < numParams; index++)
	    {
	      if ((*expr)->info.combinator.param[index]->positive)
		SelfReference (&((*expr)->info.combinator.param[index]->positive), selfTag, selfExpr);

	      if ((*expr)->info.combinator.param[index]->negative)
		SelfReference (&((*expr)->info.combinator.param[index]->negative), selfTag, selfExpr);
	    }
	}

      break;

    default:
      assert (BFALSE);
    }
}


/***************************************
 *                                     *
 *           _CompileFold              *
 *                                     *
 ***************************************/
static
M_INSTR
*_CompileFold(char *comb, int numParms, COMB_PHR **parms, COMB_EXPR *state) /* H-O */
{
     M_INSTR    *foldStart       = NULL;
     char       *parentType      = NULL;
     char      **constrs         = NULL;
     COMB_EXPR  *phraseComb      = NULL;
     int         count           = 0;
     int         parametricParms = 0;
     assert(numParms > 0);
     assert(parms);
     assert(state);
     assert(comb);

     foldStart = EmitAddr();

     EmitMsg("Fold Table");

     parentType      = _getType(comb);
     parametricParms = getNumParams(parentType);
     constrs         = getStructorNames(parentType);

     /* set up the array -- offset table for the phrases */
     for (count = 0; count < numParms; count++) {
	  emitCode.instr = MCgoto;
	  emit();
     }

     /* fill in pointers to the compiled code */
     for (count = 0; count < numParms; count++) {
	  EmitMsg("Folds");

	  (foldStart + count)->info.code = EmitAddr();

	  phraseComb = CombExprComposition(tmpHD,
					   CombExprPair(tmpHD,
							mapEi(tmpHD,
							      constrs[count], 
							      makeP0Arr(max(parametricParms, numParms),
									    tmpHD),
							      state),
							CombExprP1(tmpHD)),
					   parms[count]->positive); /* H-O */

	  _CompileSC(phraseComb);

	  emitCode.instr     = MCret;
	  emit();     
     }
     return(foldStart);
}


/***************************************
 *                                     *
 *           _CompileUnfold            *
 *                                     *
 ***************************************/

/* [H-O] EXTENDED THIS FUNCTION TO HANDLE H-O UNFOLDS: */

static
M_INSTR *
_CompileUnfold (int        numParms,
		COMB_PHR **parms,
		char      *parentType,
		COMB_EXPR *state)
{
  M_INSTR    *unfoldStart     = NULL;
  M_INSTR    *curr            = NULL;
  COMB_EXPR  *phraseComb      = NULL;
  int         count           = 0;
  char      **destrs          = NULL;
  int         parametricParms = 0;
  COMB_EXPR  *second          = NULL;

  assert (numParms > 0);
  assert (parms);
  assert (parentType);
  assert (state);

  parametricParms = getNumParams (parentType);
  destrs          = getStructorNames (parentType);

  unfoldStart = EmitAddr ();

  EmitMsg ("Unfold Table");

  emitCode.instr         = MCalloc;
  emitCode.info.allocNum = numParms + 1;
  emit ();

  emitCode.instr          = MCldmacroframe;
  emitCode.info.numDestrs = GC_BUILTIN - numParms;     /* [BI] ALTERED (SEE machine_private.h) */
  emit ();

  curr = EmitAddr ();

  /* SET UP THE ARRAY---OFFSET TABLE FOR THE PHRASES: */

  for (count = 0; count < numParms; count++)
    {
      emitCode.instr = MCclosure;
      emit ();
    }

  curr->instr = MCbclosure;     /* FIRST CLOSURE IS SPECIAL */

  emitCode.instr = MCret;
  emit ();

  /* FILL IN POINTERS TO THE COMPILED CODE: */

  for (count = 0; count < numParms; count++)
    {
      BBOOL isFO = !st_IsHO (st_NameToKey (destrs[count]));

      EmitMsg ("Unfolds");

      curr[count].info.closureCode = EmitAddr ();     /* ARRAY OF CLOSURES */

      if (isFO)
	{
	  emitCode.instr = MCmkupdate;
	  emit ();
	}

      if (isFO)
	second = CombExprP1 (tmpHD);
      else
	second = CombExprComposition (tmpHD,
				      CombExprP1 (tmpHD),
				      CombExprP1 (tmpHD));

      phraseComb = CombExprComposition (tmpHD,
					CombExprPair (tmpHD,
						      parms[count]->positive,
						      second),
					mapEi (tmpHD,
					       destrs[count], 
					       makeP0Arr (max (parametricParms,
							       numParms),
							  tmpHD),
					       state));

      _CompileSC (phraseComb);

      if (isFO)
	{
	  emitCode.instr          = MCupdate;
	  emitCode.info.updateClo = count + 1;
	  emit ();
	}
      else
	{
	  emitCode.instr = MCret;
	  emit ();
	}
    }

  return unfoldStart;
}


/***************************************
 *                                     *
 *           _CompileMapInduct         *
 *                                     *
 ***************************************/
static
M_INSTR
*_CompileMapInduct(char *comb, COMB_PHR **parms, COMB_EXPR *state) /* H-O */
{
     M_INSTR    *mapStart     = NULL;
     char       *parentType   = NULL;
     char      **constrs      = NULL;
     COMB_EXPR  *phraseComb   = NULL;
     int         count        = 0;
     int         numStructors = 0;

     assert(parms);
     assert(state);
     assert(comb);

     parentType = _getType(comb);
     constrs    = getStructorNames(parentType);
     numStructors = getNumStructors(parentType);

     mapStart = EmitAddr();

     EmitMsg("Map Inductive Table");

     /* set up the array -- offset table for the phrases */
     for (count = 0; count < numStructors; count++) {
	  emitCode.instr = MCgoto;
	  emit();
     }

     /* fill in pointers to the compiled code */
     for (count = 0; count < numStructors; count++) {
	  EmitMsg("Map Table");

	  (mapStart + count)->info.code = EmitAddr();

	  phraseComb = CombExprComposition(tmpHD,
					   mapEi(tmpHD, constrs[count], parms, state),
					   CombExprConstructor(tmpHD, constrs[count]));
	  
	  _CompileSC(phraseComb);

	  emitCode.instr     = MCret;
	  emit();     
     }
     return(mapStart);
}


/***************************************
 *                                     *
 *          _CompileMapCoinduct        *
 *                                     *
 ***************************************/

/* [H-O] EXTENDED THIS FUNCTION TO HANDLE H-O COINDUCTIVE MAPS: */

static
M_INSTR *
_CompileMapCoinduct (char       *comb,
		     COMB_PHR  **parms,
		     char       *parentType1,
		     COMB_EXPR  *state)
{
  M_INSTR    *mapStart     = NULL;
  M_INSTR    *curr         = NULL;
  COMB_EXPR  *phraseComb   = NULL;
  int         count        = 0;
  int         numStructors = 0;
  char      **destrs       = NULL;
  char       *parentType   = NULL;
  COMB_EXPR  *first        = NULL;

  assert (comb);
  assert (parms);
/*  assert (parentType1); */
  assert (state);

  parentType   = _getType (comb);
  destrs       = getStructorNames (parentType);
  numStructors = getNumStructors (parentType);

  mapStart = EmitAddr ();

  EmitMsg ("Map Coinductive Table");

  emitCode.instr         = MCalloc;
  emitCode.info.allocNum = numStructors + 1;
  emit ();

  emitCode.instr          = MCldmacroframe;
  emitCode.info.numDestrs = GC_BUILTIN - numStructors;     /* [BI] ALTERED (SEE machine_private.h) */
  emit ();

  curr = EmitAddr ();

  /* SET UP THE ARRAY---OFFSET TABLE FOR THE PHRASES: */

  for (count = 0; count < numStructors; count++)
    {
      emitCode.instr = MCclosure;
      emit ();
    }

  emitCode.instr = MCret;
  emit ();

  curr->instr = MCbclosure;     /* FIRST CLOSURE IS SPECIAL */

  /* FILL IN POINTERS TO THE COMPILED CODE: */

  for (count = 0; count < numStructors; count++)
    {
      BBOOL isFO = !st_IsHO (st_NameToKey (destrs[count]));

      EmitMsg ("Maps");

      (curr + count)->info.closureCode = EmitAddr ();   /* ARRAY OF CLOSURES */

      if (isFO)
	{
	  emitCode.instr = MCmkupdate;
	  emit ();
	}

      if (isFO)
	first = CombExprPair (tmpHD,
			      CombExprComposition (tmpHD,
						   CombExprP0 (tmpHD),
						   CombExprDestructor (tmpHD,
								       destrs[count])),
			      CombExprP1 (tmpHD));
      else
	first = CombExprPair (tmpHD,
			      CombExprComposition (tmpHD,
						   CombExprPair (tmpHD,
								 CombExprComposition (tmpHD,
										      CombExprPair (tmpHD,
												    CombExprP0 (tmpHD),
												    CombExprComposition (tmpHD,
															 CombExprP1 (tmpHD),
															 CombExprP1 (tmpHD))),
										      mapEiHO (tmpHD,
											       destrs[count],
											       parms)),
								 CombExprComposition (tmpHD,
										      CombExprP1 (tmpHD),
										      CombExprP0 (tmpHD))),

						   CombExprDestructor (tmpHD,
								       destrs[count])),
			      CombExprComposition (tmpHD,
						   CombExprP1 (tmpHD),
						   CombExprP1 (tmpHD)));

      phraseComb = CombExprComposition (tmpHD,
					first,
					mapEi (tmpHD,
					       destrs[count],
					       parms,
					       state));

      _CompileSC (phraseComb);

      if (isFO)
	{
	  emitCode.instr          = MCupdate;
	  emitCode.info.updateClo = count + 1;
	  emit ();
	}
      else
	{
	  emitCode.instr = MCret;
	  emit ();
	}
    }

/*
  for (count = 0; count < numStructors; count++)
    {
      BBOOL isFO = !st_IsHO (st_NameToKey (destrs[count]));

      EmitMsg ("Maps");

      (curr + count)->info.closureCode = EmitAddr ();   / * ARRAY OF CLOSURES * /

      if (isFO)
	{
	  emitCode.instr = MCmkupdate;
	  emit ();
	}

      if (isFO)
	first = CombExprPair (tmpHD,
			      CombExprComposition (tmpHD,
						   CombExprP0 (tmpHD),
						   CombExprDestructor (tmpHD,
								       destrs[count])),
			      CombExprP1 (tmpHD));
      else
	first = CombExprPair (tmpHD,
			      CombExprComposition (tmpHD,
						   CombExprPair (tmpHD,
								 CombExprComposition (tmpHD,
										      CombExprP0 (tmpHD),
										      CombExprP0 (tmpHD)),
								 CombExprComposition (tmpHD,
										      CombExprPair (tmpHD,
												    CombExprP1 (tmpHD),
												    CombExprComposition (tmpHD,
															 CombExprP0 (tmpHD),
															 CombExprP1 (tmpHD))),
										      mapEiHO (tmpHD,
											       destrs[count],
											       parms))),
						   CombExprDestructor (tmpHD,
								       destrs[count])),
			      CombExprComposition (tmpHD,
						   CombExprP0 (tmpHD),
						   CombExprP1 (tmpHD)));

      phraseComb = CombExprComposition (tmpHD,
					first,
					mapEi (tmpHD,
					       destrs[count],
					       parms,
					       state));

      _CompileSC (phraseComb);

      if (isFO)
	{
	  emitCode.instr          = MCupdate;
	  emitCode.info.updateClo = count + 1;
	  emit ();
	}
      else
	{
	  emitCode.instr = MCret;
	  emit ();
	}
    }
*/

  return mapStart;
}

/***************************************
 *                                     *
 *            _CompilePass2            *
 *                                     *
 ***************************************/
static
void
_CompilePass2(M_INSTR *start)
{
     M_INSTR       *current   = start;
     COMB_EXPR     *expr      = NULL;
     ALIAS         *alias     = NULL;
     CP_LIST_ALIAS *aliasList = NULL;
     assert(start);

     while (current < EmitAddr()) {
	  expr = current->cexpr;
	  if (expr) {
	       if (!AliasListMemberCexpr(aliasList, expr)) {  
		    aliasList = AliasListCons(AliasNew(current, expr), aliasList);
		    if (current->instr != MCldparm) {
			 switch (expr->info.combinator.class) {
			    case CC_CASE:
			      current->info.inductCode =
				   _CompileCase(expr->info.combinator.numParams,
						expr->info.combinator.param,
						current + 1);
			      break;
			    case CC_RECORD:
			      current->info.code =
				   _CompileRecord(expr->info.combinator.numParams,
						  expr->info.combinator.param,
						  expr->info.combinator.parentName);
			      break;

			    case CC_CATA:     /* [#@] */

			      current->info.inductCode =
				_CompileCata (expr->info.combinator.numParams,
					      expr->info.combinator.param,
					      expr);

			      break;

			    case CC_ANA:     /* [#@] */

			      current->info.code =
				_CompileAna (expr->info.combinator.numParams,
					     expr->info.combinator.param,
					     expr,
					     expr->info.combinator.parentName);

			      break;

			    case CC_FOLD:
			      assert (BFALSE);     /* [#@] */

			      current->info.inductCode =
				   _CompileFold(expr->info.combinator.name,
						expr->info.combinator.numParams,
						expr->info.combinator.param,
						expr);

			      break;
			    case CC_UNFOLD:
			      assert (BFALSE);     /* [#@] */

			      current->info.code = 
				   _CompileUnfold(expr->info.combinator.numParams,
						  expr->info.combinator.param,
						  expr->info.combinator.parentName,
						  expr);
			      break;
			    case CC_MAP_I:
			      current->info.inductCode =
				   _CompileMapInduct(expr->info.combinator.name,
						     expr->info.combinator.param,
						     expr);
			      break;
			    case CC_MAP_C:
			      current->info.code =
				   _CompileMapCoinduct(expr->info.combinator.name,
						       expr->info.combinator.param,
						       expr->info.combinator.parentName,
						       expr);
			      break;
			    default: 
			      printMsg(FATAL_MSG, "_CompilePass2 - unrecognized combinator class");
			      break;
			 }
		    }
		    else {
			 _CompileParms(current);
		    }
	       }
	       else {
		    alias = AliasListFind(aliasList, current->cexpr);

		    if (isInductiveType(alias->cexpr->info.combinator.parentName)) {
			 current->info.inductCode = alias->macro_code->info.inductCode;
		    }
		    else {
			 current->info.code = alias->macro_code->info.code;
		    }
	       }
	  }
	  current++; 
     }
}


/***************************************
 *                                     *
 *            Compile                  *
 *                                     *
 ***************************************/
M_INSTR
*Compile(char *functionName, struct _COMB_EXPR *code)
{
     char     str[MAX_STRING];
     M_INSTR *result = NULL;

     tmpHD = MemAlloc("compile scratch", TMP_MEM_SIZE, sizeof(char));
     aliasHD = MemAlloc("compile alias scratch", TMP_MEM_SIZE, sizeof(char));

     strncpy(str, functionName, MAX_STRING - 10);
     strcat(str, " --------");
     EmitMsg(str);

     result = _Compile(code);

     emitCode.instr = MCret;
     emit();

     _CompilePass2(result);

     MemDealloc(tmpHD); 
     MemDealloc(aliasHD);

     return(result);
}

/***************************************
 *                                     *
 *          _CompileHalt               *
 *                                     *
 ***************************************/
M_INSTR
*_CompileHalt(struct _COMB_EXPR *code)
{
     M_INSTR *result = NULL;
     EMIT    index;

     tmpHD = MemAlloc("compile scratch", TMP_MEM_SIZE, sizeof(char));
     aliasHD = MemAlloc("compile alias scratch", TMP_MEM_SIZE, sizeof(char));

     index = EmitIndex();

     result = _Compile(code);

     emitCode.instr = MChalt;
     emit();

     _CompilePass2(result);

     EmitSet(index);

     MemDealloc(tmpHD); 
     MemDealloc(aliasHD);

     return(result);
}

/***************************************
 *                                     *
 *            _Compile                 *
 *                                     *
 ***************************************/
static
M_INSTR
*_Compile(struct _COMB_EXPR *code)
{
     char        msg[50];
     M_INSTR     *result = NULL;

     assert(code);


     EmitMsg("-------------------");

     result = EmitAddr();

     _CompileSC(code);

     return(result);
}

