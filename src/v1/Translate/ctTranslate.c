/******************************************************************************
 *                                                                            *
 *   ctTranslate.c                                                            *
 *                                                                            *
 *   COPYRIGHT (c) 1995 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include <stdio.h>
#include <assert.h>
#include <string.h>     /* [#@] */

#include "types.h"
#include "lib.h"
#include "ioChar.h"
#include "pmem.h"
#include "symtab.h"
#include "ctTranslate.h"
#include "listParse.h"
#include "variance.h"        /* [H-O] ADDED THIS INCLUDE (SEE BELOW): */

BBOOL     printCT_EXPR = BFALSE;

/****************************************************************
 *                                                              *
 *            Constant defintions                               *
 *                                                              *
 ****************************************************************/

#define DEBUG              0
#define MAX_TMP_HP         10000

#define RES_PAIR   "pair"
#define RES_BANG   "!"
#define RES_CASE   "case"
#define RES_FOLD   "fold"
#define RES_RECORD "record"
#define RES_MAP    "map"
#define RES_UNFOLD "unfold"

#define _makeIdentity(heap)         CombExprNew(heap, CC_PRIMITIVE, RES_ID, 0, NULL)
#define _makeEnv(heap, v, e)        CombExprPair(heap, _ctTranslate(heap, v, e), _makeIdentity(heap))
#define _makeEnvSigma(heap, v, e)   CombExprPair(heap, _ctTranslate(heap, v, e), _ctTranslate(heap, v, &ct_expr_env))
#define CombExprAlloc(HD)           (COMB_EXPR *) MemHeapAlloc(HD, 1, sizeof(COMB_EXPR))

/****************************************************************
 *                                                              *
 *                 function prototypes (internal)               *
 *                                                              *
 ****************************************************************/

static CT_TERM   *ctPreTranslateTerm   (CT_TERM   *term);       /* [#@] */
static CT_FOLD   *ctPreTranslateFold   (CT_FOLD   *fold,
					int        self,
					CT_EXPR   *context);
static CT_UNFOLD *ctPreTranslateUnfold (CT_UNFOLD *unfold,
					int        self,
					CT_EXPR   *context);

static BBOOL        _isIdentity(char *vb1, char *vb2);
static BBOOL        _varOccurs(char *var, CT_VAR_BASE *var_base);
static BBOOL        _bangOccurs(CT_VAR_BASE *var_base);

static void         _translateError(void);

static CT_EXPR     *_ExprBang(void);

static COMB_EXPR *_translateApp  (MEMORY heapDesc, CT_VAR_BASE *var_base, CT_EXPR    *expr);
static COMB_EXPR *_transMap      (MEMORY heapDesc, CT_VAR_BASE *var_base, CT_MAP     *maps);
static COMB_EXPR *_transFold     (MEMORY heapDesc, CT_VAR_BASE *var_base, CT_FOLD   **folds);
static COMB_EXPR *_transUnfold   (MEMORY heapDesc, CT_VAR_BASE *var_base, CT_UNFOLD **unfolds);
static COMB_EXPR *_transCase     (MEMORY heapDesc, CT_VAR_BASE *var_base, CT_CASE   **cases);
static COMB_EXPR *_transRecord   (MEMORY heapDesc, CT_VAR_BASE *var_base, CT_RECORD **records);
static COMB_EXPR *_transAbs      (MEMORY heapDesc, CT_VAR_BASE *var_base, CT_ABS     *absn);
static COMB_EXPR *_transStructor (MEMORY heapDesc, char *struct_name);
static COMB_EXPR *_transFunction (MEMORY heapDesc, CT_VAR_BASE *var_base, CT_FUNCTION *function);
static COMB_EXPR *_transFunctionName(MEMORY heapDesc, char *function_name);
static COMB_EXPR *_transMacro    (MEMORY heapDesc, char *macroName,
				  CT_VAR_BASE *var_base, CT_MACROS   *macros);
static COMB_EXPR *_transMacroName(MEMORY heapDesc, char *macro_name);

static COMB_EXPR *_transCata (MEMORY       heapDesc,     /* [#@] */
			      CT_VAR_BASE *var_base,
			      CT_CATA     *cata);
static COMB_EXPR *_transAna  (MEMORY       heapDesc,
			      CT_VAR_BASE *var_base,
			      CT_ANA      *ana);

static COMB_EXPR *_ctTranslate   (MEMORY heapDesc, CT_VAR_BASE *var_base, CT_EXPR *expr);

static COMB_EXPR *_CombinatorTemplate(MEMORY heapDesc, char *type, char *comb);

static COMB_EXPR *_transBuiltin (MEMORY      heapDesc,     /* [BI] ADDED (SEE BELOW) */
				 CT_BUILTIN *builtin);

/* mapEi(): */

static COMB_EXPR *_makeMap1    (MEMORY heapDesc);
static COMB_EXPR *_makeMapProd (MEMORY     heapDesc,
				COMB_EXPR *l,
				COMB_EXPR *r);

/* [H-O] ALTERED THESE PROTOTYPES (SEE BELOW): */

static COMB_EXPR *_makeMapUserData (MEMORY       heapDesc,
				    ST_TYPE     *type,  
				    COMB_PHR   **parametric,
				    COMB_EXPR   *state,
				    V_VARIANCE   variance);
static COMB_EXPR *_mapEi           (MEMORY       heapDesc,
				    ST_TYPE     *type,
				    COMB_PHR   **parametric,
				    COMB_EXPR   *state,
				    V_VARIANCE   variance);


/* [#@] */

static CT_TERM *ct_map1        (void);
static CT_TERM *ct_mapProd     (CT_TERM *l,
				CT_TERM *r);
static CT_TERM *ct_mapUserData (ST_TYPE *type,
				int      self,
				CT_EXPR *context);
static CT_TERM *ct_mapEi       (ST_TYPE *type,
				int      self,
				CT_EXPR *context);


static void       _CombExprPrint(COMB_EXPR *expr);
static void       _CombinatorPrint(COMB_EXPR *expr);

/* [H-O] ALTERED THE TYPE OF THE LAST PARAMETER (SEE BELOW): */

static void _CombParmsPrint (int num, COMB_PHR **parms);

/* print term logic stuff */

static int   _showCT_abs(CT_ABS *absn, int indent);
static int   _showCT_term(CT_TERM *term, int indent);
static int   _showCT_case(CT_CASE **cases, int indent);
static int   _showCT_fold(CT_FOLD **folds, int indent);
static int   _showCT_map(CT_MAP *maps, int indent);
static int   _showCT_unfold(CT_UNFOLD **unfolds, int indent);
static int   _showCT_record(CT_RECORD **records, int indent);

/* misc */

static COMB_EXPR *_CombDup(MEMORY heapDesc, COMB_EXPR *expr);

static int        _macroPosition(char *macro_name);

static COMB_EXPR *_determineIdent(MEMORY heapDesc, CT_VAR_BASE *var_base, CT_EXPR *expr);

static BBOOL      isFunctionMacro(char *name);


/* [#@] */

static CT_EXPR *makeExprFromVarBase (MEMORY       heapDesc,
				     CT_VAR_BASE *varBase);

static CT_VAR_BASE *_makeVarBase    (MEMORY       heapDesc,
				     char        *var);
static CT_VAR_BASE *_make0tupleBase (MEMORY       heapDesc);
static CT_VAR_BASE *_make2tupleBase (MEMORY       heapDesc,
				     CT_VAR_BASE *v0,
				     CT_VAR_BASE *v1);

static CT_EXPR *_makeVarExpr    (MEMORY   heapDesc,
				 char    *var);
static CT_EXPR *_make0tupleExpr (MEMORY   heapDesc);
static CT_EXPR *_make2tupleExpr (MEMORY   heapDesc,
				 CT_EXPR *t0,
				 CT_EXPR *t1);
static CT_EXPR *_makeAppExpr    (MEMORY   heapDesc,
				 CT_TERM *f,
				 CT_EXPR *t);

static CT_TERM *_makeAbsTerm (MEMORY       heapDesc,
			      CT_VAR_BASE *varBase,
			      CT_EXPR     *expr);

static CT_TERM *_makeId (MEMORY heapDesc);


/****************************************************************
 *                                                              *
 *                     Local Variables                          *
 *                                                              *
 ****************************************************************/

static MEMORY         tmpHD;
static char          *functionName;

CT_VAR_BASE          ct_vb_env;
static CT_EXPR       ct_expr_env;

/*==============================================================*/

/*********************************
 *                               *
 *    TranslateConstruct         *
 *                               *
 *********************************/
void
ctTranslateConstruct(void)
{
     ct_vb_env.tag        = CT_VB_VAR;
     ct_vb_env.info.var   = ENVIRONMENT;

     ct_expr_env.tag      = CT_VAR;
     ct_expr_env.info.var = ENVIRONMENT;
}

/*********************************
 *                               *
 *    TranslateDestruct          *
 *                               *
 *********************************/
void          
ctTranslateDestruct(void)
{
}

/*********************************
 *                               *
 *    TranslateReset             *
 *                               *
 *********************************/
void
ctTranslateReset(void)
{
}

/*********************************
 *                               *
 *    ct_TranslateOpen           *
 *                               *
 *********************************/
void
ct_TranslateOpen(void)
{
     tmpHD = MemAlloc("translate scratch", 1, MAX_TMP_HP);
}


/*********************************
 *                               *
 *    ct_TranslateClose          *
 *                               *
 *********************************/
void
ct_TranslateClose(void)
{
     MemDealloc(tmpHD);
}

/**************************
 *                        *
 *    _translateError     *
 *                        *
 **************************/
static
void
_translateError(void)
{
     if (functionName) {
	  st_RemoveEntry(st_NameToKey(functionName));
     }
     ct_TranslateClose();
}

/**************************
 *                        *
 *    _isIdentity         *
 *                        *
 **************************/
static
BBOOL
_isIdentity(char *vb1, char *vb2)
{
     if (strcmp(vb1, vb2) == 0)
	  return(BTRUE);

     return(BFALSE);
}

/**************************
 *                        *
 *    _ExprBang           *
 *                        *
 **************************/
static 
CT_EXPR
*_ExprBang(void)
{
     CT_EXPR *result = (CT_EXPR *) MemHeapAlloc(tmpHD, 1, sizeof(CT_EXPR));

     assert(result);
     result->tag = CT_BANG;

     return(result);
}


/**************************
 *                        *
 *    _varOccurs          *
 *                        *
 **************************/
static
BBOOL
_varOccurs(char *var, CT_VAR_BASE *var_base)
{
     BBOOL result = BFALSE;
     assert(var);
     assert(var_base);

     switch (var_base->tag) {
	case CT_VB_BANG:
	  result = BFALSE;
	  break;
	case CT_VB_PAIR:
	  if (! (result = _varOccurs(var, var_base->info.pair.l))) 
	       result = _varOccurs(var, var_base->info.pair.r);
	  break;
	case CT_VB_VAR:
	  if (strcmp(var, var_base->info.var) == 0)
	       result = BTRUE;
	  break;
	default:
	  assert (BFALSE);
	  break;
     }

     return(result);
}

/**************************
 *                        *
 *    _bangOccurs         *
 *                        *
 **************************/
static
BBOOL
_bangOccurs(CT_VAR_BASE *var_base)
{
     BBOOL result = BFALSE;
     assert(var_base);

     switch (var_base->tag) {
	case CT_VB_BANG:
	  result = BTRUE;
	  break;
	case CT_VB_PAIR:
	  if (! (result = _bangOccurs(var_base->info.pair.l))) 
	       result = _bangOccurs(var_base->info.pair.r);
	  break;
	case CT_VB_VAR:
	       result = BFALSE;
	  break;
	default:
	  break;
     }

     return(result);
}


/**************************
 *                        *
 *    makeExprFromVarBase *
 *                        *
 **************************/

/* [#@] */

static
CT_EXPR *
makeExprFromVarBase (MEMORY       heapDesc,
		     CT_VAR_BASE *varBase)
{
  CT_EXPR *result = (CT_EXPR *)MHA (heapDesc, 1, sizeof (CT_EXPR));

  assert (varBase);
  assert (result);

  switch (varBase->tag)
    {
    case CT_VB_BANG:
      result->tag = CT_BANG;

      break;

    case CT_VB_VAR:
      result->tag      = CT_VAR;
      result->info.var = varBase->info.var;

      break;

    case CT_VB_PAIR:
      result->tag         = CT_PAIR;
      result->info.pair.l = makeExprFromVarBase (heapDesc, varBase->info.pair.l);
      result->info.pair.r = makeExprFromVarBase (heapDesc, varBase->info.pair.r);

      break;

    default:
      assert (BFALSE);
    }

  return result;
}


/**************************
 *                        *
 *    _makeVarExpr        *
 *                        *
 **************************/
static
CT_EXPR
*_makeVarExpr(MEMORY heapDesc, char *var)
{
     CT_EXPR *result = (CT_EXPR *) MemHeapAlloc(heapDesc, 1, sizeof(CT_EXPR));
     
     result->tag = CT_VAR;
     result->info.var = var;
     return(result);
}


/**************************
 *                        *
 *    _make0tupleExpr     *
 *                        *
 **************************/

/* [#@] */

static
CT_EXPR *
_make0tupleExpr (MEMORY heapDesc)
{
  CT_EXPR *expr = (CT_EXPR *)MHA (heapDesc, 1, sizeof (CT_EXPR));

  assert (expr);

  expr->tag = CT_BANG;

  return expr;
}


/**************************
 *                        *
 *    _make2tupleExpr     *
 *                        *
 **************************/

/* [#@] */

static
CT_EXPR *
_make2tupleExpr (MEMORY   heapDesc,
		  CT_EXPR *t0,
		  CT_EXPR *t1)
{
  CT_EXPR *expr = (CT_EXPR *)MHA (heapDesc, 1, sizeof (CT_EXPR));

  assert (t0);
  assert (t1);
  assert (expr);

  expr->tag         = CT_PAIR;
  expr->info.pair.l = t0;
  expr->info.pair.r = t1;

  return expr;
}


/**************************
 *                        *
 *    _makeAppExpr        *
 *                        *
 **************************/

/* [#@] */

static
CT_EXPR *
_makeAppExpr (MEMORY   heapDesc,
	       CT_TERM *f,
	       CT_EXPR *t)
{
  CT_EXPR *expr = (CT_EXPR *)MHA (heapDesc, 1, sizeof (CT_EXPR));

  assert (f);
  assert (t);
  assert (expr);

  expr->tag           = CT_APP;
  expr->info.app.term = f;
  expr->info.app.expr = t;

  return expr;
}


/**************************
 *                        *
 *    _makeAbsTerm        *
 *                        *
 **************************/

/* [#@] */

static
CT_TERM *
_makeAbsTerm (MEMORY       heapDesc,
	      CT_VAR_BASE *varBase,
	      CT_EXPR     *expr)
{
  CT_TERM *term = (CT_TERM *)MHA (heapDesc, 1, sizeof (CT_TERM));

  assert (varBase);
  assert (expr);
  assert (term);

  term->tag      = CT_T_ABS;
  term->info.abs = (CT_ABS *)MHA (heapDesc, 1, sizeof (CT_ABS));

  assert (term->info.abs);

  term->info.abs->var_base = varBase;
  term->info.abs->expr     = expr;

  return term;
}


/**************************
 *                        *
 *    _makeId             *
 *                        *
 **************************/

/* [#@] */

static
CT_TERM *
_makeId (MEMORY heapDesc)
{
  CT_TERM *id = NULL;

  id = _makeAbsTerm (heapDesc,
		     _makeVarBase (heapDesc, "x"),
		     _makeVarExpr (heapDesc, "x"));

  return id;
}


/**************************
 *                        *
 *    _makeVarBase        *
 *                        *
 **************************/

/* [#@] */

static
CT_VAR_BASE *
_makeVarBase (MEMORY  heapDesc,
	      char   *var)
{
  CT_VAR_BASE *varBase = (CT_VAR_BASE *)MHA (heapDesc, 1, sizeof (CT_VAR_BASE));

  assert (var);
  assert (varBase);

  varBase->tag      = CT_VB_VAR;
  varBase->info.var = var;

  return varBase;
}


/**************************
 *                        *
 *    _make0tupleBase     *
 *                        *
 **************************/

/* [#@] */

static
CT_VAR_BASE *_make0tupleBase (MEMORY heapDesc)
{
  CT_VAR_BASE *varBase = (CT_VAR_BASE *)MHA (heapDesc, 1, sizeof (CT_VAR_BASE));

  assert (varBase);

  varBase->tag = CT_VB_BANG;

  return varBase;
}


/**************************
 *                        *
 *    _make2tupleBase     *
 *                        *
 **************************/

/* [#@] */

static
CT_VAR_BASE *_make2tupleBase (MEMORY       heapDesc,
			      CT_VAR_BASE *v0,
			      CT_VAR_BASE *v1)
{
  CT_VAR_BASE *varBase = (CT_VAR_BASE *)MHA (heapDesc, 1, sizeof (CT_VAR_BASE));

  assert (v0);
  assert (v1);
  assert (varBase);

  varBase->tag         = CT_VB_PAIR;
  varBase->info.pair.l = v0;
  varBase->info.pair.r = v1;

  return varBase;
}


/**************************
 *                        *
 *    CombExprP0          *
 *                        *
 **************************/
COMB_EXPR
*CombExprP0(MEMORY heapDesc)
{
     COMB_EXPR *result = CombExprAlloc(heapDesc);
     assert(result);

     result->tag                       = CTT_COMBINATOR;
     result->info.combinator.class     = CC_PRIMITIVE;

     result->info.combinator.name      = PROD0;
     result->info.combinator.numParams = 0;
     result->info.combinator.param     = NULL;

     return(result);
}

/**************************
 *                        *
 *    CombExprP1          *
 *                        *
 **************************/
COMB_EXPR
*CombExprP1(MEMORY heapDesc)
{
     COMB_EXPR *result = CombExprAlloc(heapDesc);
     assert(result);

     result->tag                       = CTT_COMBINATOR;
     result->info.combinator.class     = CC_PRIMITIVE;

     result->info.combinator.name      = PROD1;
     result->info.combinator.numParams = 0;
     result->info.combinator.param     = NULL;

     return(result);
}

/**************************
 *                        *
 *    _transFunction      *
 *                        *
 **************************/
static
COMB_EXPR 
*_transFunction(MEMORY heapDesc, CT_VAR_BASE *var_base, CT_FUNCTION *function)
{
     COMB_EXPR  *result  = CombExprAlloc(heapDesc);
     COMB_PHR  **parms   = NULL;
     CT_PHRASE **phrases = NULL;     
     int         len     = 0;
     int         count   = 0;

     assert(var_base);
     assert(function);
     assert(result);

     result->tag                   = CTT_COMBINATOR;
     result->info.combinator.class = CC_FUNCTION;
     result->info.combinator.name  = function->fun_name; 

     /* Passing functions as parameters */
     /* !!!! Need to check for recursive calls */
     phrases = function->macros;
     result->info.combinator.numParams = len = PtrArrayLen((char **) function->macros);

     if (getNumMacros(function->fun_name) != len) {
	  _translateError();
	  printMsg(ERROR_MSG, "Illegal number of macros passed to function %s", function->fun_name);
     }

     /* [H-O] ALTERED parms TYPE TO BE COMB_PHR (SEE codetab.h): */

     if (len > 0)     /* any parameters */
       {
	 result->info.combinator.param = parms =
	   (COMB_PHR **)MemHeapAlloc (heapDesc,
				      len + 1,
				      sizeof (COMB_PHR *));

	  for (count = 0; count < len; count++)
	    {
	      parms[count] = (COMB_PHR *)MemHeapAlloc (heapDesc,
						       1,
						       sizeof (COMB_PHR));

	      parms[count]->positive =
		_ctTranslate (heapDesc,
			      VarBasePairNew (tmpHD,
					      phrases[count]->var_base,
					      var_base),
			      phrases[count]->expr);

	      parms[count]->negative = NULL;
	    }
       }

     return (result);
}


/**************************
 *                        *
 *    _transMacroName     *
 *                        *
 **************************/
static
COMB_EXPR
*_transMacroName(MEMORY heapDesc, char *macro_name)
{
     COMB_EXPR *result = CombExprAlloc(heapDesc);

     assert(macro_name);
     assert(result);

     result->tag                       = CTT_COMBINATOR;
     result->info.combinator.class     = CC_MACRO;
     result->info.combinator.name      = lb_BuildMacroName(heapDesc, 
							   functionName, macro_name);
     result->info.combinator.numParams = 0;
     result->info.combinator.param     = NULL;

/*
     result->info.combinator.param     = (COMB_EXPR **) MemHeapAlloc(heapDesc,
								     2,
								     sizeof(COMB_EXPR));
     result->info.combinator.param[0]  = CombExprPair(heapDesc,
						      CombExprNew(heapDesc, CC_PRIMITIVE, RES_BANG, 0, NULL),
						      _makeIdentity(heapDesc)
						      );
*/
     result->info.combinator.parameter = _macroPosition(result->info.combinator.name);

     return(result);
}


/**************************
 *                        *
 *    _transFunctionName  *
 *                        *
 **************************/
static
COMB_EXPR
*_transFunctionName(MEMORY heapDesc, char *function_name)
{
     COMB_EXPR *result = CombExprAlloc(heapDesc);

     assert(function_name);
     assert(result);

     result->tag                       = CTT_COMBINATOR;
     result->info.combinator.class     = CC_FUNCTION;
     result->info.combinator.name      = libStrdup(heapDesc, function_name);
     result->info.combinator.numParams = 0;
     result->info.combinator.param     = NULL;
     return(result);
}

/**************************
 *                        *
 *    _macroPosition      *
 *                        *
 **************************/
static
int
_macroPosition(char *macro_name)
{
     return(st_GetMacroPosn(st_NameToKey(macro_name)));
}

/**************************
 *                        *
 *    _transMacro         *
 *                        *
 **************************/
static
COMB_EXPR 
*_transMacro(MEMORY heapDesc, char *macroName, CT_VAR_BASE *var_base, CT_MACROS *macros)
{
     COMB_EXPR   *result  = CombExprAlloc(heapDesc);
     COMB_PHR   **parms   = NULL;
     CT_PHRASE  **phrases = NULL;
     int          len    = 0;
     int          count  = 0;

     assert(var_base);
     assert(macros);
     assert(result);

     result->tag                       = CTT_COMBINATOR;
     result->info.combinator.class     = CC_MACRO;
     result->info.combinator.name      = lb_BuildMacroName(heapDesc,
							   functionName,
							   macroName);
     result->info.combinator.parameter = _macroPosition(result->info.combinator.name);

     /* Load up the function parameters for a macro */
     /* !!!! Need to check for recursive calls */
     result->info.combinator.numParams = len = PtrArrayLen((char **) macros->macros);

     /* [H-O] ALTERED parms TYPE TO BE COMB_PHR (SEE codetab.h): */

     if (len > 0)     /* any parameters */
       {
	 phrases = macros->macros;

	 result->info.combinator.param = parms =
	   (COMB_PHR **)MemHeapAlloc (heapDesc, len + 1, sizeof (COMB_PHR *));

	 for (count = 0; count < len; count++)
	   {
	     parms[count] = (COMB_PHR *)MemHeapAlloc (heapDesc,
						      1,
						      sizeof (COMB_PHR));

	     parms[count]->positive =
	       _ctTranslate (heapDesc,
			     VarBasePairNew(tmpHD,
					    phrases[count]->var_base,
					    var_base),
			     phrases[count]->expr);

	     parms[count]->negative = NULL;
	   }
       }

     return (result);
}


/**************************
 *                        *
 *  CombExprConstructor   *
 *                        *
 **************************/
COMB_EXPR
*CombExprConstructor(MEMORY heapDesc, char *constr)
{
     COMB_EXPR *result = CombExprAlloc(heapDesc);
     assert(result);

     result->tag                        = CTT_COMBINATOR;
     result->info.combinator.class      = CC_CONSTRUCTOR;
     result->info.combinator.name       = libStrdup(heapDesc, constr);
     result->info.combinator.parentName = getStructorParent(constr);
     result->info.combinator.numParams  = 0;
     result->info.combinator.param      = NULL;

     return(result);
}


/**************************
 *                        *
 *    CombExprDestructor  *
 *                        *
 **************************/
COMB_EXPR
*CombExprDestructor(MEMORY heapDesc, char *destr)
{
     COMB_EXPR *result = CombExprAlloc(heapDesc);
     assert(result);

     result->tag                        = CTT_COMBINATOR;
     result->info.combinator.class      = CC_DESTRUCTOR;
     result->info.combinator.name       = destr;
     result->info.combinator.parentName = getStructorParent(destr);
     result->info.combinator.numParams  = 0;
     result->info.combinator.param      = NULL;

     return(result);
}

/**************************
 *                        *
 *    VarBasePairNew      *
 *                        *
 **************************/
CT_VAR_BASE
*VarBasePairNew(MEMORY heapDesc, CT_VAR_BASE *l, CT_VAR_BASE *r)
{
     CT_VAR_BASE *result = 
	  (CT_VAR_BASE *) MemHeapAlloc(heapDesc, 1, sizeof(CT_VAR_BASE));
     
     assert(l);
     assert(r);
     assert(result);

     result->tag         = CT_VB_PAIR;
     result->info.pair.l = l;
     result->info.pair.r = r;

     return(result);
}


/**************************
 *                        *
 *    CombExprNew         *
 *                        *
 **************************/

/* [H-O] ALTERED THE TYPE OF THE LAST PARAMETER (SEE codetab.h): */

COMB_EXPR *
CombExprNew (MEMORY       heapDesc,
	     COMB_CLASS   class,
	     char        *name,
	     int          numParms,
	     COMB_PHR   **parms)
{
  COMB_EXPR *result = CombExprAlloc (heapDesc);

/*   assert (name); */

  assert (numParms ? (int)parms : (int)!parms);
  assert (result);

  result->tag = CTT_COMBINATOR;

  if (name)
    result->info.combinator.name = libStrdup (heapDesc, name);

  result->info.combinator.class      = class;
  result->info.combinator.parentName = NULL;
  result->info.combinator.numParams  = numParms;
  result->info.combinator.param      = parms;

  return (result);
}

/**************************
 *                        *
 *  CombExprComposition   *
 *                        *
 **************************/
COMB_EXPR
*CombExprComposition(MEMORY heapDesc, COMB_EXPR *l, COMB_EXPR *r)
{
     COMB_EXPR *result = CombExprAlloc(heapDesc);

     assert(l);
     assert(r);
     assert(result);

     result->tag                = CTT_COMPOSITION;
     result->info.composition.l = l;
     result->info.composition.r = r;

     return(result);
}


/**************************
 *                        *
 *    CombExprPair        *
 *                        *
 **************************/

COMB_EXPR
*CombExprPair (MEMORY     heapDesc,
	       COMB_EXPR *l,
	       COMB_EXPR *r)
{
  COMB_EXPR *result = CombExprAlloc (heapDesc);

  assert(l);
  assert(r);
  assert(result);

  result->tag                       = CTT_COMBINATOR;
  result->info.combinator.class     = CC_PRIMITIVE;
  result->info.combinator.name      = RES_PAIR;
  result->info.combinator.numParams = 2;

  /* [H-O] ALTERED param TYPE TO BE COMB_PHR (SEE codetab.h): */

  result->info.combinator.param = 
    (COMB_PHR **)MemHeapAlloc (heapDesc, 3, sizeof (COMB_PHR *));

  result->info.combinator.param[0] =
    (COMB_PHR *)MemHeapAlloc (heapDesc, 1, sizeof (COMB_PHR));
  result->info.combinator.param[1] =
    (COMB_PHR *)MemHeapAlloc (heapDesc, 1, sizeof (COMB_PHR));

  result->info.combinator.param[0]->positive = l;
  result->info.combinator.param[0]->negative = NULL;
  result->info.combinator.param[1]->positive = r;
  result->info.combinator.param[1]->negative = NULL;

  return (result);
}


/******************************************************************************************/


/**************************
 *                        *
 *  _CombinatorTemplate   *
 *                        *
 **************************/
static
COMB_EXPR
*_CombinatorTemplate(MEMORY heapDesc, char *type, char *comb)
{
     COMB_EXPR  *result = CombExprAlloc(heapDesc);

     assert(type);
     assert(comb);
     assert(result);

     result->tag = CTT_COMBINATOR;
     if (strcmp(comb, RES_MAP) == 0) {
	  result->info.combinator.numParams = getNumParams(type);
     }
     else {
	  result->info.combinator.numParams = getNumStructors(type);
     }
     result->info.combinator.name       = lb_BuildCombString(heapDesc, comb, type);
     result->info.combinator.parentName = type;

     return(result);
}


/**************************
 *                        *
 *    _transMap           *
 *                        *
 **************************/

/* [H-O] ALTERED THE TRANSLATION OF MAPS FOR THE H-O EXTENSION: */

static
COMB_EXPR *
_transMap (MEMORY       heapDesc,
	   CT_VAR_BASE *var_base,
	   CT_MAP      *maps)
{
  COMB_EXPR      *result  = NULL;
  COMB_PHR      **params  = NULL;
  int             count   = 0;
  CT_MAP_PHRASE **phrases = NULL;

  assert (var_base);
  assert (maps);

  result = _CombinatorTemplate (heapDesc, maps->type_name, RES_MAP);

  if (isInductiveType (maps->type_name))
    result->info.combinator.class = CC_MAP_I;
  else
    result->info.combinator.class = CC_MAP_C;

  result->info.combinator.param = params =
    (COMB_PHR **)MemHeapAlloc (heapDesc,
			       result->info.combinator.numParams + 1,
			       sizeof (COMB_PHR *));

  assert (params);

  phrases = maps->phrases;

  for (count = 0; count < result->info.combinator.numParams; count++)
    {
      params[count] = (COMB_PHR *)MemHeapAlloc (heapDesc,
						1,
						sizeof (COMB_PHR));

      assert (params[count]);

      if (phrases[count]->var_base)
	params[count]->positive =
	  _ctTranslate (heapDesc,
			VarBasePairNew (tmpHD,
					phrases[count]->var_base,
					var_base),
			phrases[count]->expr);
      else
	params[count]->positive = NULL;

      if (phrases[count]->neg_var_base)
	params[count]->negative =
	  _ctTranslate (heapDesc,
			VarBasePairNew (tmpHD,
					phrases[count]->neg_var_base,
					var_base),
			phrases[count]->neg_expr);
      else
	params[count]->negative = NULL;
    }

  return result;
}


/**************************
 *                        *
 *    _transAna           *
 *                        *
 **************************/

/* [#@] */

static
COMB_EXPR *
_transAna (MEMORY       heapDesc,
	   CT_VAR_BASE *var_base,
	   CT_ANA      *ana)
{
  COMB_EXPR  *result    = (COMB_EXPR *)MHA (heapDesc, 1, sizeof (COMB_EXPR));
  int         numParams = 0;
  int         index     = 0;
  CT_UNFOLD **unfolds   = NULL;
  char       *parent    = NULL;

  assert (var_base);
  assert (ana);
  assert (result);

  unfolds = ana->unfolds;

  parent    = getStructorParent (unfolds[0]->destr);
  numParams = getNumStructors (parent);

  result->tag                        = CTT_COMBINATOR;
  result->info.combinator.class      = CC_ANA;
  result->info.combinator.numParams  = numParams;
  result->info.combinator.param      = (COMB_PHR **)MHA (heapDesc, numParams + 1, sizeof (COMB_PHR *));
  result->info.combinator.self       = ana->self;
  result->info.combinator.parentName = parent;

  assert (result->info.combinator.param);

  result->info.combinator.name      = NULL;
  result->info.combinator.parameter = 0;

  for (index = 0; index < numParams; index++)
    {
      result->info.combinator.param[index] = (COMB_PHR *)MHA (heapDesc, 1, sizeof (COMB_PHR));

      assert (result->info.combinator.param[index]);

      if (unfolds[index]->var_base2)
	result->info.combinator.param[index]->positive =
	  _ctTranslate (heapDesc,
			VarBasePairNew (heapDesc,
					unfolds[index]->var_base2,
					VarBasePairNew (heapDesc,
							unfolds[index]->var_base,
							var_base)),
			unfolds[index]->expr);
      else
	result->info.combinator.param[index]->positive =
	  _ctTranslate (heapDesc,
			VarBasePairNew (heapDesc,
					unfolds[index]->var_base,
					var_base),
			unfolds[index]->expr);

      result->info.combinator.param[index]->negative = NULL;
    }

  result->info.combinator.param[index] = NULL;

  return result;
}


/**************************
 *                        *
 *    _transCata          *
 *                        *
 **************************/

/* [#@] */

static
COMB_EXPR *
_transCata (MEMORY       heapDesc,
	    CT_VAR_BASE *var_base,
	    CT_CATA     *cata)
{
  COMB_EXPR  *result    = (COMB_EXPR *)MHA (heapDesc, 1, sizeof (COMB_EXPR));
  int         numParams = 0;
  int         index     = 0;
  CT_FOLD   **folds     = NULL;
  char       *parent    = NULL;

  assert (var_base);
  assert (cata);
  assert (result);

  folds = cata->folds;

  parent    = getStructorParent (folds[0]->constr);
  numParams = getNumStructors (parent);

  result->tag                        = CTT_COMBINATOR;
  result->info.combinator.class      = CC_CATA;
  result->info.combinator.numParams  = numParams;
  result->info.combinator.param      = (COMB_PHR **)MHA (heapDesc, numParams + 1, sizeof (COMB_PHR *));
  result->info.combinator.self       = cata->self;
  result->info.combinator.parentName = parent;

  assert (result->info.combinator.param);

  result->info.combinator.name      = NULL;
  result->info.combinator.parameter = 0;

  for (index = 0; index < numParams; index++)
    {
      result->info.combinator.param[index] = (COMB_PHR *)MHA (heapDesc, 1, sizeof (COMB_PHR));

      assert (result->info.combinator.param[index]);

      result->info.combinator.param[index]->positive =
	_ctTranslate (heapDesc,
		      VarBasePairNew (heapDesc,
				      folds[index]->var_base,
				      var_base),
		      folds[index]->expr);

      result->info.combinator.param[index]->negative = NULL;
    }

  result->info.combinator.param[index] = NULL;

  return result;
}


/**************************
 *                        *
 *    _transFold          *
 *                        *
 **************************/
static
COMB_EXPR
*_transFold(MEMORY heapDesc, CT_VAR_BASE *var_base, CT_FOLD **folds)
{
     COMB_EXPR  *result     = NULL;
     COMB_PHR  **params     = NULL;
     int         count      = 0;
     char       *parentType = NULL;

     assert (BFALSE);     /* [#@] */

     assert(folds);
     assert(var_base);

     parentType = getStructorParent(folds[0]->constr);
     result = _CombinatorTemplate(heapDesc, parentType, RES_FOLD);

     result->info.combinator.class     = CC_FOLD;

     /* [H-O] ALTERED params TYPE TO BE COMB_PHR (SEE codetab.h): */

     result->info.combinator.param = params =
       (COMB_PHR **)MemHeapAlloc (heapDesc,
				  result->info.combinator.numParams + 1,
				  sizeof (COMB_PHR *));

     assert (params);

     for (count = 0; count < result->info.combinator.numParams; count++)
       {
	 params[count] = (COMB_PHR *)MemHeapAlloc (heapDesc,
						   1,
						   sizeof (COMB_PHR));

	 params[count]->positive
	   = _ctTranslate (heapDesc,
			   VarBasePairNew (tmpHD,
					   folds[count]->var_base,
					   var_base),
			   folds[count]->expr);

	 params[count]->negative = NULL;
       }

     return (result);
}


/**************************
 *                        *
 *    _transUnfold        *
 *                        *
 **************************/

/* [H-O] ALTERED THE TRANSLATION OF UNFOLDS FOR THE H-O EXTENSION: */

static
COMB_EXPR *
_transUnfold (MEMORY        heapDesc,
	      CT_VAR_BASE  *var_base,
	      CT_UNFOLD   **unfolds)
{
  COMB_EXPR  *result     = NULL;
  COMB_PHR  **params     = NULL;
  int         count      = 0;
  char       *parentType = NULL;

  assert (BFALSE);     /* [#@] */

  assert (var_base);
  assert (unfolds);

  parentType = getStructorParent (unfolds[0]->destr);
  result = _CombinatorTemplate (heapDesc, parentType, RES_UNFOLD);
  result->info.combinator.class = CC_UNFOLD;

  result->info.combinator.param = params =
    (COMB_PHR **)MemHeapAlloc (heapDesc,
			       result->info.combinator.numParams + 1,
			       sizeof (COMB_PHR *));

  assert (params);

  for (count = 0; count < result->info.combinator.numParams; count++)
    {
      params[count] = (COMB_PHR *)MemHeapAlloc (heapDesc,
						1,
						sizeof (COMB_PHR));

      assert (params[count]);

      params[count]->negative = NULL;

      if (unfolds[count]->var_base2)
	params[count]->positive =
	  _ctTranslate (heapDesc,
			VarBasePairNew (tmpHD,
					unfolds[count]->var_base2,
					VarBasePairNew (tmpHD,
							unfolds[count]->var_base,
							var_base)),
			unfolds[count]->expr);
      else
	params[count]->positive =
	  _ctTranslate (heapDesc,
			VarBasePairNew (tmpHD,
					unfolds[count]->var_base,
					var_base),
			unfolds[count]->expr);
    }

  return result;
}


/**************************
 *                        *
 *    _transCase          *
 *                        *
 **************************/
static
COMB_EXPR 
*_transCase(MEMORY heapDesc, CT_VAR_BASE *var_base, CT_CASE **cases)
{
     COMB_EXPR  *result       = NULL;
     COMB_PHR  **params       = NULL;
     int         count        = 0;
     char       *parentType   = NULL;

     assert(cases);
     assert(var_base);

     parentType = getStructorParent(cases[0]->constr);
     result = _CombinatorTemplate(heapDesc, parentType, RES_CASE);

     result->info.combinator.class     = CC_CASE;

     /* [H-O] ALTERED params TYPE TO BE COMB_PHR (SEE codetab.h): */

     result->info.combinator.param = params =
       (COMB_PHR **)MemHeapAlloc (heapDesc,
				  result->info.combinator.numParams + 1,
				  sizeof (COMB_PHR *));

     assert (params);

     for (count = 0; count < result->info.combinator.numParams; count++)
       {
	 params[count] = (COMB_PHR *)MemHeapAlloc (heapDesc,
						   1,
						   sizeof (COMB_PHR));

	 assert (params[count]);

	 params[count]->positive =
	   _ctTranslate(heapDesc,
			VarBasePairNew (tmpHD,
					cases[count]->var_base,
					var_base),
			cases[count]->expr);

	 params[count]->negative = NULL;
       }

     assert (result);
     return (result);
}


/**************************
 *                        *
 *    _transRecord        *
 *                        *
 **************************/

/* [H-O] ALTERED THE TRANSLATION OF RECORDS FOR THE H-O EXTENSION: */

static
COMB_EXPR *
_transRecord (MEMORY        heapDesc,
	      CT_VAR_BASE  *var_base,
	      CT_RECORD   **records)
{
  COMB_EXPR  *result     = NULL;
  COMB_PHR  **params     = NULL;
  int         count      = 0;
  char       *parentType = NULL;

  assert (var_base);
  assert (records);

  parentType = getStructorParent (records[0]->destr);
  result = _CombinatorTemplate (heapDesc, parentType, RES_RECORD);
  result->info.combinator.class = CC_RECORD;
  result->info.combinator.parentName = parentType;

  result->info.combinator.param = params =
    (COMB_PHR **)MemHeapAlloc (heapDesc,
			       result->info.combinator.numParams + 1,
			       sizeof (COMB_PHR *));

  assert (params);

  for (count = 0; count < result->info.combinator.numParams; count++)
    {
      params[count] = (COMB_PHR *)MemHeapAlloc (heapDesc,
						1,
						sizeof (COMB_PHR));

      assert (params[count]);

      params[count]->negative = NULL;

      if (records[count]->var_base)
	params[count]->positive =
	  _ctTranslate (heapDesc,
			VarBasePairNew (tmpHD,
					records[count]->var_base,
					var_base),
			records[count]->expr);
      else
	params[count]->positive =
	  _ctTranslate (heapDesc, var_base, records[count]->expr);
    }

  return result;
}


/**************************
 *                        *
 *    _transAbs           *
 *                        *
 **************************/
static
COMB_EXPR
*_transAbs(MEMORY heapDesc, CT_VAR_BASE *var_base, CT_ABS *absn)
{
     COMB_EXPR *result   = NULL;
     
     assert(var_base);
     assert(absn);

     result = _ctTranslate(heapDesc, 
			   VarBasePairNew(tmpHD, absn->var_base, var_base), 
			   absn->expr);

     assert(result);
     return(result);
}

/**************************
 *                        *
 *    _transStructor      *
 *                        *
 **************************/
static
COMB_EXPR
*_transStructor(MEMORY heapDesc, char *struct_name)
{
     COMB_EXPR *result = NULL;

     if (isConstructor(struct_name)) {
	  result = CombExprConstructor(heapDesc, struct_name);	  
     }
     else if (isDestructor(struct_name)) {
	  if (strcmp(struct_name, PROD0) == 0) {
	       result = CombExprP0(heapDesc);
	  }
	  else if (strcmp(struct_name, PROD1) == 0) {
     	       result = CombExprP1(heapDesc);
	  }
	  else {
	       result = CombExprDestructor(heapDesc, struct_name);	  
	  }
     }
     else {
	  _translateError();
	  printMsg(ERROR_MSG, "unknown structor %s", struct_name);
     }
     return(result);
}

/**************************
 *                        *
 *    _isFunctionMacro    *
 *                        *
 **************************/
static
BBOOL
isFunctionMacro(char *name)
{    
     if (functionName)
	  return(st_IsMacroByName(lb_BuildMacroName(tmpHD, functionName, name)));

     return(BFALSE);
}

/**************************
 *                        *
 *    _translateApp       *
 *                        *
 **************************/
static
COMB_EXPR
*_translateApp(MEMORY heapDesc, CT_VAR_BASE *var_base, CT_EXPR *expr)
{
     COMB_EXPR *result = NULL;
     CT_TERM   *aterm  = NULL;
     CT_EXPR   *aexpr  = NULL;
     
     assert(var_base);
     assert(expr);
     assert(expr->tag == CT_APP);
     aterm = expr->info.app.term;
     aexpr = expr->info.app.expr;

     assert(aterm);
     assert(aexpr);

     switch (aterm->tag) {
	case CT_T_STRUCTOR:
	  if (_varOccurs(aterm->info.struct_name, var_base)) {
	       result = CombExprVar(heapDesc, var_base, 
				    _makeVarExpr(heapDesc, aterm->info.struct_name));     
          } else if (isFunctionMacro(aterm->info.struct_name)) {  /* Macro? */
	       result = CombExprComposition(heapDesc, 
					    _makeEnvSigma(heapDesc, var_base, aexpr),
					    _transMacroName(heapDesc, aterm->info.struct_name));
	  } else {
	       result = CombExprComposition(heapDesc, 
					    _ctTranslate(heapDesc, var_base, aexpr),
					    _transStructor(heapDesc, aterm->info.struct_name));
	  }
	  break;
	case CT_T_FUNCTION:
	  if (strcmp (aterm->info.function->fun_name, AT_NAME) == 0)     /* [#@] */
	    {
	      COMB_EXPR *at = (COMB_EXPR *)MHA (heapDesc, 1, sizeof (COMB_EXPR));

	      assert (at);

	      at->tag                   = CTT_COMBINATOR;
	      at->info.combinator.class = CC_AT;

	      at->info.combinator.name       = NULL;
	      at->info.combinator.parentName = NULL;
	      at->info.combinator.parameter  = 0;
	      at->info.combinator.numParams  = 0;
	      at->info.combinator.param      = NULL;
	      at->info.combinator.i          = 0;
	      at->info.combinator.c          = '\0';
	      at->info.combinator.self       = 0;

	      result = CombExprComposition (heapDesc,
					    _ctTranslate (heapDesc, var_base, aexpr),
					    at);
	    }
	  else
	    {
	      if (_varOccurs(aterm->info.function->fun_name, var_base)) {
		printMsg(WARN_MSG, "%s assumed to be a function or macro, not a variable",
			 aterm->info.function->fun_name);
	      }

	      if (isFunctionMacro(aterm->info.function->fun_name)) {  /* Macro? */
		result = CombExprComposition(heapDesc, 
					     _makeEnvSigma(heapDesc, var_base, aexpr),
					     _transMacro(heapDesc, aterm->info.function->fun_name,
							 var_base, aterm->info.macro));
	      } else {
		if (   aterm->info.function->macros == NULL
		    || aterm->info.function->macros[0] == NULL) {
		  result = CombExprComposition(heapDesc,
					       _ctTranslate(heapDesc, var_base, aexpr),
					       _transFunction(heapDesc, var_base, aterm->info.function));
		}
		else {
		  result = CombExprComposition(heapDesc,
					       _makeEnv(heapDesc, var_base, aexpr),
					       _transFunction(heapDesc, var_base, aterm->info.function));
		}
	      }
	    }
	  break;
	case CT_T_MACRO:
	  if (_varOccurs(aterm->info.macro->macro_name, var_base)) {
	       /* Spit out warning message since variable might shadow macro name */
	       printMsg(WARN_MSG, "%s assumed to be a function macro, not a variable",
			aterm->info.macro->macro_name);
	  }

	  if (isFunctionMacro(aterm->info.macro->macro_name)) {  /* Macro? */
	       result = CombExprComposition(heapDesc,
					    _makeEnvSigma(heapDesc, var_base, aexpr),
					    _transMacro(heapDesc, aterm->info.macro->macro_name,
							var_base, aterm->info.macro));
	  } else {
	       _translateError();
	       printMsg(ERROR_MSG, "Macro/function %s undefined", aterm->info.macro->macro_name);
	  }	  
	  break;
	case CT_T_MAP:
	  result = CombExprComposition(heapDesc,
				       _makeEnv(heapDesc, var_base, aexpr),
				       _transMap(heapDesc, var_base, aterm->info.maps));
	  break;
	case CT_T_FOLD:
	  result = CombExprComposition(heapDesc,
				       _makeEnv(heapDesc, var_base, aexpr),
				       _transFold(heapDesc, var_base, aterm->info.folds));
	  break;
	case CT_T_UNFOLD:
	  result = CombExprComposition(heapDesc,
				       _makeEnv(heapDesc, var_base, aexpr),
				       _transUnfold(heapDesc, var_base, aterm->info.unfolds));
	  break;

	case CT_T_CATA:     /* [#@] */
	  {
	    CT_EXPR *temp = makeExprFromVarBase (heapDesc, var_base);

	    *aterm->info.cata->context = *temp;

	    result = CombExprComposition (heapDesc,
					  _makeEnv (heapDesc, var_base, aexpr),
					  _transCata (heapDesc, var_base, aterm->info.cata));
	  }

	  break;

	case CT_T_ANA:     /* [#@] */
	  {
	    CT_EXPR *temp = makeExprFromVarBase (heapDesc, var_base);

	    *aterm->info.ana->context = *temp;

	    result = CombExprComposition (heapDesc,
					  _makeEnv (heapDesc, var_base, aexpr),
					  _transAna (heapDesc, var_base, aterm->info.ana));
	  }

	  break;

	case CT_T_SELF:     /* [#@] */

	  result = (COMB_EXPR *)MHA (heapDesc, 1, sizeof (COMB_EXPR));

	  assert (result);

	  result->tag                       = CTT_COMBINATOR;
	  result->info.combinator.class     = CC_SELF;
	  result->info.combinator.numParams = 0;
	  result->info.combinator.self      = aterm->info.self;

	  result->info.combinator.name       = NULL;
	  result->info.combinator.parentName = NULL;
	  result->info.combinator.parameter  = 0;

	  result = CombExprComposition (heapDesc,
					_ctTranslate (heapDesc, var_base, aexpr),
					result);

	  break;

	case CT_T_CASE:
	  result = CombExprComposition(heapDesc,
				       _makeEnv(heapDesc, var_base, aexpr),
				       _transCase(heapDesc, var_base, aterm->info.cases));
	  break;
	case CT_T_RECORD:
	  result = _transRecord(heapDesc, var_base, aterm->info.records);
	  break;
	case CT_T_ABS:
	  result = CombExprComposition(heapDesc,
				       _makeEnv(heapDesc, var_base, aexpr),
				       _transAbs(heapDesc, var_base, aterm->info.abs));
	  break;

	case CT_T_BUILTIN:     /* [BI] ADDED (SEE BELOW) */

	  result = CombExprComposition (heapDesc,
					_ctTranslate (heapDesc, var_base, aexpr),
					_transBuiltin (heapDesc, aterm->info.builtin));

	  break;

	default:
	  printMsg(FATAL_MSG, "_translateApp - %d is not a valid tag", aterm->tag);
	  break;
     }

     assert(result);
     return(result);
}

/**************************
 *                        *
 *    CombExprVar         *
 *                        *
 **************************/
COMB_EXPR 
*CombExprVar(MEMORY heapDesc, CT_VAR_BASE *var_base, CT_EXPR *var_expr)
{
     COMB_EXPR *result = NULL;
     char      *var    = NULL;
     
     assert(var_base);
     assert(var_expr);
     assert(var_expr->tag == CT_VAR);

     var = var_expr->info.var;

     switch (var_base->tag) {
	case CT_VB_BANG:
	  _translateError();
	  printMsg(ERROR_MSG, "Type - Variable %s not within scope of function",var_expr->info.var);
	  break;
	case CT_VB_VAR:
	  if (_isIdentity(var_base->info.var, var)) {
	       result = _makeIdentity(heapDesc);
	  }
	  else {
	       _translateError();
	       printMsg(ERROR_MSG, "Type - Variable %s not within scope of function", var_expr->info.var);
	  }
	  break;
	case CT_VB_PAIR:
	  if (_varOccurs(var, var_base->info.pair.l)) {
	       result = CombExprComposition(heapDesc,
					    CombExprP0(heapDesc), 
					    _ctTranslate(heapDesc, var_base->info.pair.l, var_expr));
	  }
	  else {
	       result = CombExprComposition(heapDesc,
					    CombExprP1(heapDesc), 
					    _ctTranslate(heapDesc, var_base->info.pair.r, var_expr));
	  }     
	  break;
	default:
	  printMsg(FATAL_MSG, "CombExprVar - %d is not a known variable base", var_base->tag);
	  break;
     }

     assert(result);
     return(result);
}


/**************************
 *                        *
 *    _determineIdent     *
 *                        *
 **************************/
static
COMB_EXPR
*_determineIdent(MEMORY heapDesc, CT_VAR_BASE *var_base, CT_EXPR *expr)
{
     /*------------------------------------------*/
     /* Must first check to see if identifier is */
     /* Variable, Macro, Function, or Structor   */
     /*------------------------------------------*/
     COMB_EXPR *result = NULL;

     if (_varOccurs(expr->info.var, var_base)) {     /* variable    */
	  result = CombExprVar(heapDesc, var_base, expr);
     } else if (isFunctionMacro(expr->info.var)) {  /* macro       */
	  result = CombExprComposition(heapDesc, 
				       _makeEnvSigma(heapDesc, var_base, _ExprBang()),
				       _transMacroName(heapDesc, expr->info.var));
     } else if (isStructor(expr->info.var)) {       /* structor    */
	  result = CombExprComposition(heapDesc, 
				       _ctTranslate(heapDesc, var_base, _ExprBang()),
				       _transStructor(heapDesc, expr->info.var));
     } else if (isFunction(expr->info.var)) {       /* function    */
	  result = CombExprComposition(heapDesc,
				       _ctTranslate(heapDesc, var_base, _ExprBang()),
				       _transFunctionName(heapDesc, expr->info.var));
     } else {                                       /* unbound var */
	  _translateError();
	  printMsg(ERROR_MSG, "Unbound Variable: %s", expr->info.var);
     }
     return(result);
}

/**************************
 *                        *
 *    _ctTranslate        *
 *                        *
 **************************/
COMB_EXPR
*_ctTranslate(MEMORY heapDesc, CT_VAR_BASE *var_base, CT_EXPR *expr)
{
     COMB_EXPR *result = NULL;

     assert(var_base);

     if (expr) {
	  switch (expr->tag) {
	     case CT_VAR:
	       result = _determineIdent(heapDesc, var_base, expr);
	       break;
	     case CT_BANG:
	       if (var_base) {
		    switch (var_base->tag) {
		       case CT_VB_BANG:  /* T[ () => () ] */
			 result = CombExprNew(heapDesc, CC_PRIMITIVE, RES_ID1, 0, NULL);
			 break;
		       case CT_VB_VAR:   /* T[ x => () ] */
			 result = CombExprNew(heapDesc, CC_PRIMITIVE, RES_BANG, 0, NULL);	
			 break;
		       case CT_VB_PAIR:  /* T[ (x, y) => () ] where x, y is a var or () */
			 if (_bangOccurs(var_base->info.pair.l)) /* T[ ((), x) => () ] */
			      result = CombExprComposition(heapDesc, 
							   CombExprP0(heapDesc),
							   _ctTranslate(heapDesc, 
									var_base->info.pair.l,
									expr)
							   );
			 else if (_bangOccurs(var_base->info.pair.r)) /* T[ (x, ()) => ()] */
			      result = CombExprComposition(heapDesc, 
							   CombExprP1(heapDesc),
							   _ctTranslate(heapDesc, 
									var_base->info.pair.r,
									expr)
							   ); 
			 else  /* T[ x => () ] */
			      result = CombExprNew(heapDesc, CC_PRIMITIVE, RES_BANG, 0, NULL);	
			 break;
		       default:
			 /* should never reach this point */
			 printMsg(FATAL_MSG, "_ctTranslate: Unknown tag field");
		    }
	       }
	       else result = CombExprNew(heapDesc, CC_PRIMITIVE, RES_ID1, 0, NULL);
	       break;
	     case CT_PAIR:
	       result = CombExprPair(heapDesc,
				     _ctTranslate(heapDesc, var_base, expr->info.pair.l),
				     _ctTranslate(heapDesc, var_base, expr->info.pair.r));
		    break;
	     case CT_APP:
	       result = _translateApp(heapDesc, var_base, expr);
	       break;

	     default:
	       printMsg(FATAL_MSG, "ctTranslate - Unable to translate term logic");
	       break;
	  }
     } else 
	  printMsg(FATAL_MSG, "_ctTranslate:  invalid expression");

     assert(result);
     return(result);
}

/**************************
 *                        *
 *    ctTranslate         *
 *                        *
 **************************/
COMB_EXPR
*ctTranslate(char *funName, CT_VAR_BASE *var_base, CT_EXPR *expr)
{
     COMB_EXPR *result = NULL;

     functionName   = funName;
     result         = _ctTranslate(tmpHD, var_base, expr);
     functionName   = NULL; 
#if DEBUG
     printf("%s -> ", funName); CombExprPrint(result); printf("\n"); */
#endif
     return(result);
}


/**************************
 *                        *
 *     _transBuiltin      *
 *                        *
 **************************/

/* [BI] TRANSLATE THE BUILTIN VALUES: */

static
COMB_EXPR *
_transBuiltin (MEMORY      heapDesc,
	       CT_BUILTIN *builtin)
{
  COMB_EXPR *result = NULL;

  assert (builtin);

  switch (builtin->tag)
    {
    case CT_INT:
      result = CombInt (heapDesc, builtin->info.i);

      break;

    case CT_CHAR:
      result = CombChar (heapDesc, builtin->info.c);

      break;

    default:
      assert (BFALSE);
    }

  return result;
}


/**************************
 *                        *
 *     CombInt            *
 *                        *
 **************************/

/* [BI] BUILD AN INTEGER COMBINATOR: */

COMB_EXPR *
CombInt (MEMORY heapDesc,
	 int    i)
{
  COMB_EXPR *result = (COMB_EXPR *)MemHeapAlloc (heapDesc, 1, sizeof (COMB_EXPR));

  assert (result);

  result->tag                        = CTT_COMBINATOR;
  result->info.combinator.class      = CC_BUILTIN_INT;
  result->info.combinator.name       = NULL;
  result->info.combinator.parentName = NULL;
  result->info.combinator.numParams  = 0;
  result->info.combinator.param      = NULL;
  result->info.combinator.i          = i;

  return result;
}


/**************************
 *                        *
 *     CombChar           *
 *                        *
 **************************/

/* [BI] BUILD A CHARACTER COMBINATOR: */

COMB_EXPR *
CombChar (MEMORY heapDesc,
	  char   c)
{
  COMB_EXPR *result = (COMB_EXPR *)MemHeapAlloc (heapDesc, 1, sizeof (COMB_EXPR));

  assert (result);

  result->tag                        = CTT_COMBINATOR;
  result->info.combinator.class      = CC_BUILTIN_CHAR;
  result->info.combinator.name       = NULL;
  result->info.combinator.parentName = NULL;
  result->info.combinator.numParams  = 0;
  result->info.combinator.param      = NULL;
  result->info.combinator.c          = c;

  return result;
}


/**************************
 *                        *
 *     _CombDup           *
 *                        *
 **************************/

static
COMB_EXPR *
_CombDup (MEMORY     heapDesc,
	  COMB_EXPR *expr)
{
  COMB_EXPR  *result = NULL;
  COMB_PHR  **parms  = NULL;
  int         count  = 0;

  assert (expr);

  /* [H-O] ALTERED parms TYPE TO BE COMB_PHR (SEE codetab.h): */

  if (expr->info.combinator.numParams)
    {
      parms = (COMB_PHR **)MemHeapAlloc (heapDesc,
					 expr->info.combinator.numParams + 1,
					 sizeof (COMB_PHR *));

      assert (parms);

      for (count = 0; count < expr->info.combinator.numParams; count++)
	{
	  parms[count] = (COMB_PHR *)MemHeapAlloc (heapDesc,
						   1,
						   sizeof (COMB_PHR));

	  if (expr->info.combinator.param[count]->positive)
	    parms[count]->positive =
	      CombExprDuplicate (heapDesc,
				 expr->info.combinator.param[count]->positive);
	  else
	    parms[count]->positive = NULL;

	  if (expr->info.combinator.param[count]->negative)
	    parms[count]->negative =
	      CombExprDuplicate (heapDesc,
				 expr->info.combinator.param[count]->negative);
	  else
	    parms[count]->negative = NULL;
	}
    }

  result = CombExprNew (heapDesc,
			expr->info.combinator.class,
			expr->info.combinator.name,
			expr->info.combinator.numParams,
			parms);

  result->info.combinator.parentName = expr->info.combinator.parentName;
  result->info.combinator.parameter  = expr->info.combinator.parameter;
  result->info.combinator.i          = expr->info.combinator.i;
  result->info.combinator.c          = expr->info.combinator.c;
  result->info.combinator.self       = expr->info.combinator.self;

  return (result);
}


/**************************
 *                        *
 *    CombExprDuplicate   *
 *                        *
 **************************/
COMB_EXPR
*CombExprDuplicate(MEMORY heapDesc, COMB_EXPR *expr)
{
     COMB_EXPR *result = NULL;
     assert(expr);

     switch (expr->tag) {
        case CTT_COMPOSITION:
          result = CombExprComposition(heapDesc,
				       CombExprDuplicate(heapDesc, expr->info.composition.l), 
				       CombExprDuplicate(heapDesc, expr->info.composition.r));
          break;
        case CTT_COMBINATOR:
          result = _CombDup(heapDesc, expr);
          break;
        default:
          printMsg(FATAL_MSG, "CompileExprDuplicate: Internal error");
          break;
     }
     return(result);
}


/*********************************************************************************/


/**************************
 *                        *
 *    _CombParmsPrint     *
 *                        *
 **************************/

/* [H-O] ALTERED TO HANDLE COMBINATOR PHRASES PROPERLY (SEE codetab.h): */

static
void
_CombParmsPrint (int        num,
		 COMB_PHR **parms)
{
  int count = 0;

  for (count = 0; count < num; count++)
    {
      if (count > 0)
	printf (", ");

      if (parms[count]->positive)
	if (parms[count]->negative)
	  {
	    _CombExprPrint (parms[count]->positive);
	    printf (" & ");
	    _CombExprPrint (parms[count]->negative);
	  }
	else
	  _CombExprPrint (parms[count]->positive);
      else
	if (parms[count]->negative)
	  _CombExprPrint (parms[count]->negative);
	else
	  printf ("_");
    }
}


/**************************
 *                        *
 *    _CombinatorPrint    *
 *                        *
 **************************/

/* [H-O] ALTERED TO HANDLE COMBINATOR PHRASES PROPERLY (SEE codetab.h): */

static
void
_CombinatorPrint (COMB_EXPR *expr)
{
     assert(expr);
     assert(expr->tag == CTT_COMBINATOR);

     switch (expr->info.combinator.class) {
	case CC_PRIMITIVE:
	  if (strcmp(expr->info.combinator.name, RES_PAIR) == 0) {
	       printf("<");
	       _CombExprPrint(expr->info.combinator.param[0]->positive);
	       printf(", ");
	       _CombExprPrint(expr->info.combinator.param[1]->positive);
	       printf(">");
	  }
	  else if (strcmp(expr->info.combinator.name, MAP_PROD) == 0) {
	       printf("map_prod(");
	       _CombExprPrint(expr->info.combinator.param[0]->positive);
	       printf(", ");
	       _CombExprPrint(expr->info.combinator.param[1]->positive);
	       printf(")");
	  }
	  else {
	       printf("%s", expr->info.combinator.name);
	  }
	  break;
	case CC_CONSTRUCTOR:
	case CC_DESTRUCTOR:
	  printf("%s", expr->info.combinator.name);
	  break;
	case CC_FOLD:
	case CC_CASE:
	case CC_MAP_I:
	case CC_UNFOLD:
	case CC_RECORD:
	case CC_MAP_C:
	  printf("%s(", expr->info.combinator.name);
	  _CombParmsPrint(expr->info.combinator.numParams, 
			  expr->info.combinator.param);
	  printf(")");
	  break;
	case CC_FUNCTION:
	  printf("FUNCTION(%s){", expr->info.combinator.name);
	  _CombParmsPrint(expr->info.combinator.numParams, 
			  expr->info.combinator.param);
	  printf("}");
	  break;
	case CC_MACRO:
	  printf("PARM(%s, %d)", expr->info.combinator.name,
		 expr->info.combinator.parameter);
	  break;

	case CC_BUILTIN_INT:
	  printf ("INT(%d)", expr->info.combinator.i);           /* [BI] PRINT BULITIN INTEGERS   */

	  break;

	case CC_BUILTIN_CHAR:
	  printf ("CHAR(%d)", (int)expr->info.combinator.c);     /* [BI] PRINT BUILTIN CHARACTERS */

	  break;

	default:
	  printMsg(FATAL_MSG, "_CombinatorPrint: Unrecognized command");
	  break;
     }
}

/**************************
 *                        *
 *    _CombExprPrint      *
 *                        *
 **************************/
static
void
_CombExprPrint(COMB_EXPR *expr)
{
     assert(expr);
     
     switch (expr->tag) {
	case CTT_COMPOSITION:
	  _CombExprPrint(expr->info.composition.l);
	  printf(";");
	  _CombExprPrint(expr->info.composition.r);
	  break;
	case CTT_COMBINATOR:	  
	  _CombinatorPrint(expr);
	  break;
	case CTT_CLOSURE:
	  printf("CLOSURE");
	  break;
	default:
	  printMsg(FATAL_MSG, "CombExprPrint: Unrecognized combinator.");
	  break;
     }
}


/**************************
 *                        *
 *    CombExprPrint       *
 *                        *
 **************************/
void
CombExprPrint(COMB_EXPR *expr)
{
     _CombExprPrint(expr);
}


/**************************
 *                        *
 *    _makeMap1           *
 *                        *
 **************************/

static
COMB_EXPR *
_makeMap1 (MEMORY heapDesc)
{
  COMB_EXPR *result = CombExprAlloc (heapDesc);

  assert (result);

  result->tag                       = CTT_COMBINATOR;
  result->info.combinator.class     = CC_PRIMITIVE;
  result->info.combinator.name      = "map_1";
  result->info.combinator.numParams = 0;
  result->info.combinator.param     = NULL;

  return result;
}


/**************************
 *                        *
 *    _makeMapProd        *
 *                        *
 **************************/

static
COMB_EXPR *
_makeMapProd (MEMORY     heapDesc,
	      COMB_EXPR *l,
	      COMB_EXPR *r)
{
  COMB_EXPR *result = CombExprAlloc (heapDesc);

  assert (l);
  assert (r);
  assert (result);

  result->tag                       = CTT_COMBINATOR;
  result->info.combinator.class     = CC_PRIMITIVE;
  result->info.combinator.name      = MAP_PROD;
  result->info.combinator.numParams = 2;

  /* [H-O] ALTERED param TYPE TO BE COMB_PHR (SEE codetab.h): */

  result->info.combinator.param =
    (COMB_PHR **)MemHeapAlloc (heapDesc, 3, sizeof (COMB_PHR *));

  assert (result->info.combinator.param);

  result->info.combinator.param[0] =
    (COMB_PHR *)MemHeapAlloc (heapDesc, 1, sizeof (COMB_PHR));
  result->info.combinator.param[1] =
    (COMB_PHR *)MemHeapAlloc (heapDesc, 1, sizeof (COMB_PHR));

  assert (result->info.combinator.param[0]);
  assert (result->info.combinator.param[1]);

  result->info.combinator.param[0]->positive = l;
  result->info.combinator.param[0]->negative = NULL;
  result->info.combinator.param[1]->positive = r;
  result->info.combinator.param[1]->negative = NULL;

  result->info.combinator.param[2] = NULL;

  return result;
}


/**************************
 *                        *
 *    _makeMapUserData    *
 *                        *
 **************************/

static
COMB_EXPR *
_makeMapUserData (MEMORY       heapDesc,
		  ST_TYPE     *type,
		  COMB_PHR   **parametric,
		  COMB_EXPR   *state,          /* MAY BE NULL */
		  V_VARIANCE   variance)
{
  COMB_EXPR  *result = CombExprAlloc (heapDesc);
  int         count  = 0;
  V_VARIANCE *varity = NULL;

  assert (result);

  result->tag = CTT_COMBINATOR;

  if (isInductiveType (type->info.user_data.name))
    result->info.combinator.class = CC_MAP_I;
  else
    result->info.combinator.class = CC_MAP_C;

  result->info.combinator.name =
    lb_BuildCombString (heapDesc,
			RES_MAP,
			type->info.user_data.name);

  result->info.combinator.parentName = type->info.user_data.name;
  result->info.combinator.numParams  = getNumParams (type->info.user_data.name);

  /* [H-O] ALTERED param TYPE TO BE COMB_PHR (SEE codetab.h): */

  result->info.combinator.param =
    (COMB_PHR **) MemHeapAlloc(heapDesc,
			       result->info.combinator.numParams + 1,
			       sizeof (COMB_PHR *));

  assert (result->info.combinator.param);

  varity = st_GetVarity (type->info.user_data.key);

  assert (varity);

  for (count = 0; count < result->info.combinator.numParams; count++)
    {
      result->info.combinator.param[count] =
	(COMB_PHR *)MemHeapAlloc (heapDesc,
				  1,
				  sizeof (COMB_PHR));

      assert (result->info.combinator.param[count]);

      switch (varity[count])
	{
	case V_NEITHER:
	  result->info.combinator.param[count]->positive = NULL;
	  result->info.combinator.param[count]->negative = NULL;

	  break;

	case V_POSITIVE:
	  result->info.combinator.param[count]->positive =
	    _mapEi (heapDesc,
		    type->info.user_data.args[count],
		    parametric,
		    state,
		    variance);

	  result->info.combinator.param[count]->negative = NULL;

	  break;

	case V_NEGATIVE:
	  result->info.combinator.param[count]->positive = NULL;

	  result->info.combinator.param[count]->negative =
	    _mapEi (heapDesc,
		    type->info.user_data.args[count],
		    parametric,
		    state,
		    Flip (variance));

	  break;

	case V_BOTH:
	  result->info.combinator.param[count]->positive =
	    _mapEi (heapDesc,
		    type->info.user_data.args[count],
		    parametric,
		    state,
		    variance);

	  result->info.combinator.param[count]->negative =
	    _mapEi (heapDesc,
		    type->info.user_data.args[count],
		    parametric,
		    state,
		    Flip (variance));

	  break;
	}
    }

  result->info.combinator.param[count] = NULL;

  return result;
}


/**************************
 *                        *
 *    _mapEi              *
 *                        *
 **************************/

/* [H-O] ALTERED THIS FUNCTION TO TRACK VARIANCE: */

static
COMB_EXPR *
_mapEi (MEMORY       heapDesc,
	ST_TYPE     *type,
	COMB_PHR   **parametric,
	COMB_EXPR   *state,          /* MAY BE NULL */
	V_VARIANCE   variance)
{
  COMB_EXPR *result = NULL;

  assert (type);
  assert (parametric);
  assert ((variance == V_POSITIVE) || (variance == V_NEGATIVE));

  switch (type->tag)
    {
    case TYPE_1:
      result = _makeMap1 (heapDesc);

      break;

    case TYPE_PROD:
      result = _makeMapProd (heapDesc,
			     _mapEi (heapDesc,
				     type->info.prod.l,
				     parametric,
				     state,
				     variance),
			     _mapEi (heapDesc,
				     type->info.prod.r,
				     parametric,
				     state,
				     variance));

      break;

    case TYPE_PARAMETRIC_VAR:
      switch (variance)
	{
	case V_POSITIVE:
	  result = parametric[type->info.parametric_var]->positive;

	  break;

	case V_NEGATIVE:
	  result = parametric[type->info.parametric_var]->negative;

	  break;
	}

      break;

    case TYPE_STATE_VAR:
      result = state;

      break;

    case TYPE_USER_DATA:
      result = _makeMapUserData (heapDesc, type, parametric, state, variance);

      break;

    case TYPE_BUILTIN_CHAR:     /* [BI] ADDED THIS CASE FOR MAPPING OVER INTEGERS/CHARACTERS */
    case TYPE_BUILTIN_INT:
      result = CombExprP0 (heapDesc);

      break;

    default:
      printMsg (FATAL_MSG, "_mapEi(): unknown type.");

      break;
    }

  assert (result);

  return result;
}


/**************************
 *                        *
 *    mapEi               *
 *                        *
 **************************/

/* [H-O] ALTERED THIS FUNCTION TO TRACK VARIANCE: */

COMB_EXPR *
mapEi (MEMORY      heapDesc,
       char       *structor,
       COMB_PHR  **parametric,
       COMB_EXPR  *state)
{
  ST_TYPE *type = NULL;

  assert (structor);
  assert (parametric); 
  assert (state);

  type = getStructorType (structor);

  assert (type);

  return _mapEi (heapDesc, type, parametric, state, V_POSITIVE);
}


/**************************
 *                        *
 *    mapEiHO             *
 *                        *
 **************************/

/* [H-O] ADDED THIS FUNCTION TO HANDLE H-O DESTRUCTORS: */

COMB_EXPR *
mapEiHO (MEMORY      heapDesc,
	 char       *structor,
	 COMB_PHR  **parametric)
{
  ST_TYPE_SIG *type = NULL;

  assert (structor);
  assert (parametric); 

  type = getStructorTypeSig (structor);

  assert (type);
  assert (type->domain);
  assert (type->domain->tag == TYPE_PROD);
  assert (type->domain->info.prod.l);

  return _mapEi (heapDesc,
		 type->domain->info.prod.l,
		 parametric,
		 NULL,
		 V_NEGATIVE);
}


/*****************************************************************************/

/**************************
 *                        *
 *    _showCT_expr        *
 *                        *
 **************************/
int
_showCT_expr(CT_EXPR *expr, int indent) {

     switch (expr->tag) {
	case CT_VAR:
	  appendBuff(expr->info.var); 
	  indent += strlen(expr->info.var);
	  break;
	case CT_PAIR:
	  appendBuff("(");     indent++;
	  indent = _showCT_expr(expr->info.pair.l, indent);
	  appendBuff(", ");    indent +=2;
	  indent = _showCT_expr(expr->info.pair.r, indent);
	  appendBuff(")");     indent++;
	  break;
	case CT_APP:
	  indent = _showCT_term(expr->info.app.term, indent);
          switch ( expr->info.app.expr->tag ) {
          case CT_APP:
          case CT_VAR:  
              appendBuff(" ");  indent++;
              /* break missing on purpose */
          case CT_PAIR:                   
              indent = _showCT_expr(expr->info.app.expr, indent);
              break;
          case CT_BANG:
              if ( expr->info.app.term->tag != CT_T_STRUCTOR &&
                   expr->info.app.term->tag != CT_T_RECORD )   
                  indent = _showCT_expr(expr->info.app.expr, indent);
              break;
          default:
	    printMsg(FATAL_MSG, "_showCT_expr - unknown expr (%d)", expr->tag);
	    break;
	  }   /*  hctiws  */
	  break;
	case CT_BANG:
	  appendBuff("()");     indent += 2;
	  break;
	default:
	  printMsg(FATAL_MSG, "_showCT_expr - unknown expr (%d)", expr->tag);
	  break;
     }

    return indent;

}


/**************************
 *                        *
 *    _showCT_term        *
 *                        *
 **************************/
static
int
_showCT_term(CT_TERM *term, int indent) {
     switch (term->tag) {
	case CT_T_STRUCTOR:
	  appendBuff(term->info.struct_name);
          indent += strlen(term->info.struct_name);
	  break;
	case CT_T_FUNCTION:
	  indent = _showCT_map((CT_MAP *)term->info.function, indent);
	  break;
	case CT_T_MACRO:
	  indent = _showCT_map((CT_MAP *)term->info.macro, indent);
	  break;
	case CT_T_MAP:
	  indent = _showCT_map(term->info.maps, indent);
	  break;
	case CT_T_CASE:
	  indent = _showCT_case(term->info.cases, indent);
	  break;
	case CT_T_ABS:
	  indent = _showCT_abs(term->info.abs, indent);
	  break;
	case CT_T_FOLD:
	  indent = _showCT_fold(term->info.folds, indent);
	  break;
	case CT_T_CATA:                                                 /* [#@] */
	  indent = _showCT_fold(term->info.cata->folds, indent);
	  break;
	case CT_T_ANA:                                                  /* [#@] */
	  indent = _showCT_unfold(term->info.ana->unfolds, indent);
	  break;
	case CT_T_SELF:                                                 /* [#@] */
	  appendBuff ("REC");
	  break;
	case CT_T_UNFOLD:
	  indent = _showCT_unfold(term->info.unfolds, indent);
	  break;
	case CT_T_RECORD:
	  indent = _showCT_record(term->info.records, indent);
	  break;

	case CT_T_BUILTIN:     /* [BI] ADDED TO DISPLAY BUILTINS */
	  {
	    char s[10];

	    switch (term->info.builtin->tag)
	      {
	      case CT_INT:
		sprintf (s, "%d", term->info.builtin->info.i);
		break;

	      case CT_CHAR:
		sprintf (s, "\\d%d", (int)term->info.builtin->info.c);
		break;

	      default:
		assert (BFALSE);
	      }

	    appendBuff (s);
	  }

	  break;

	default:
	  printMsg(FATAL_MSG, "_showCT_term - unknown term (%d)", term->tag);
	  break;
     }

    return indent;

}

/**************************
 *                        *
 *    _appendCharsToBuff  *
 *                        *
 **************************/
static
void _appendCharsToBuff(char *s, int iters) {

    while (iters--)   appendBuff(s);

}   /*  end _appendCharsToBuff  */


/**************************
 *                        *
 *    _showCT_map        *
 *                        *
 **************************/
static 
int  _showCT_map(CT_MAP *maps, int indent) {

     int count = 0;
     int oldIndent = indent;

     appendBuff(maps->type_name);
     oldIndent += strlen(maps->type_name);

     while (maps->phrases[count]) {
          indent = oldIndent;
	  if (count) {
              appendBuff(",\n");
              _appendCharsToBuff(" ", oldIndent);
	    }   /*  fi  */
          else {
              appendBuff("{");     indent++;   oldIndent++;
          }   /*  esle  */
	  indent += _showCT_VarBase(maps->phrases[count]->var_base, BFALSE);
	  appendBuff(" => ");       indent += 4;
	  indent = _showCT_expr(maps->phrases[count]->expr, indent);
	  count++;
     }
     if (count == 1)    {
         appendBuff("}");  indent++;
     }   /*  fi  */
     else if (count > 1) {
         appendBuff("\n");   _appendCharsToBuff(" ",oldIndent-1);
         appendBuff("}");
         indent = oldIndent;
     }   /*  esle fi  */

    return indent;

}

/**************************
 *                        *
 *    _showCT_fold       *
 *                        *
 **************************/
static
int _showCT_fold(CT_FOLD **folds, int indent) {
     int num = 0;
     int count = 0;
    int oldIndent = indent;

     num = getNumStructors(getStructorParent(folds[0]->constr));

     for (count = 0; count < num; count++) {
         indent = oldIndent;
	  if (count) { 
              appendBuff("\n");
              _appendCharsToBuff(" ", oldIndent);
              appendBuff(" | ");    indent += 3;
          }   /*  fi  */
          else {
              appendBuff("{| ");    indent += 3;
          }   /*  esle  */
	  appendBuff(folds[count]->constr);   
          indent += strlen(folds[count]->constr);
	  appendBuff(" : ");   indent += 3;
	  indent += _showCT_VarBase(folds[count]->var_base, BFALSE);
	  appendBuff(" => ");  indent += 4;
	  _showCT_expr(folds[count]->expr, indent);
     }   /*  rof  */

     appendBuff("\n");
     _appendCharsToBuff(" ", oldIndent);
     appendBuff(" |}");    oldIndent += 3;

    return oldIndent;
}

/**************************
 *                        *
 *    _showCT_record      *
 *                        *
 **************************/
static
int _showCT_record(CT_RECORD **records, int indent) {
     int num = 0;
     int count = 0;

     num = getNumStructors(getStructorParent(records[0]->destr));

     for (count = 0; count < num; count++) {
	  if (count) {
              appendBuff(", ");     indent += 2;
          }   /*  fi  */
          else {
              appendBuff("(");     indent++;
          }   /*  esle  */
	  appendBuff(records[count]->destr);
          indent += strlen(records[count]->destr);
	  appendBuff(": ");    indent += 2;

          if ( records[count]->var_base != NULL ) {
              indent += _showCT_VarBase(records[count]->var_base, BFALSE);
              appendBuff(" => ");     indent +=4;
          }   /*  fi  */

	  indent = _showCT_expr(records[count]->expr, indent);
     }

     appendBuff(")");    indent++;

     return indent;

}

/**************************
 *                        *
 *    _showCT_unfold      *
 *                        *
 **************************/
static
int _showCT_unfold(CT_UNFOLD **unfolds, int indent) {
     int num = 0;
     int count = 0;
     int vbLen = 0;
     int oldIndent = indent;

     num = getNumStructors(getStructorParent(unfolds[0]->destr));

     for (count = 0; count < num; count++) {
          indent = oldIndent;
	  if (count) {
              appendBuff("\n");
              _appendCharsToBuff(" ", oldIndent);
              appendBuff(" | ");    indent += 3;
              _appendCharsToBuff(" ", vbLen);   indent += vbLen;
          }   /*   fi  */
          else {
              appendBuff("(| ");    indent += 3;
              if (num) {
                  vbLen += _showCT_VarBase(unfolds[count]->var_base, BFALSE);
                  appendBuff(" => ");    vbLen += 4;
                  indent += vbLen;
              }   /*  fi  */
          }   /*  esle  */
	  appendBuff(unfolds[count]->destr);
          indent += strlen(unfolds[count]->destr);
	  appendBuff(" : ");     indent += 3;
	  _showCT_expr(unfolds[count]->expr, indent);
     }

     appendBuff("\n");
     _appendCharsToBuff(" ", oldIndent);
     appendBuff(" |)");     oldIndent += 3;

    return oldIndent;

}

/**************************
 *                        *
 *    _showCT_case        *
 *                        *
 **************************/
static
int
_showCT_case(CT_CASE **cases, int indent)
{
     int num = 0;
     int count = 0;
     int oldIndent = indent;

     num = getNumStructors(getStructorParent(cases[0]->constr));

     for (count = 0; count < num; count++) {
          indent = oldIndent;
	  if (count) {
              appendBuff("\n");
              _appendCharsToBuff(" ", oldIndent);
              appendBuff("| ");    indent +=2;
	    }   /*  fi  */
          else {
              appendBuff("{ ");   indent += 2;
	  }   /*  esle  */
	  appendBuff(cases[count]->constr);
          indent += strlen(cases[count]->constr);
	  indent += _showCT_VarBase(cases[count]->var_base, BTRUE);
	  appendBuff(" => ");     indent +=4;
	  _showCT_expr(cases[count]->expr, indent);
     }   /*  rof  */
     appendBuff("\n");   _appendCharsToBuff(" ", oldIndent);   
     appendBuff("}");    oldIndent++;

    return oldIndent;

}

/**************************
 *                        *
 *    _showCT_abs         *
 *                        *
 **************************/
static
int
_showCT_abs(CT_ABS *absn, int indent)
{
     appendBuff("{");    indent++;
     indent += _showCT_VarBase(absn->var_base, BFALSE);
     appendBuff(" => ");  indent += 4;
     indent = _showCT_expr(absn->expr, indent);
     appendBuff("}");    indent++;

    return indent;

}

/*********************************
 *                               *
 *    _showCT_VarBase            *
 *                               *
 *********************************/
int
_showCT_VarBase(CT_VAR_BASE *var_base, BBOOL spaceBeforeVar) {

    int length = 0;

     if (var_base) {
          switch (var_base->tag) {
             case CT_VB_BANG:
#if 0
               appendBuff("()");
               length += 2;
#endif
               break;
             case CT_VB_VAR:
               if ( spaceBeforeVar ) {
                   appendBuff(" ");
                   length++;
   	       }   /*  fi  */
               appendBuff(var_base->info.var);
               length = length + strlen(var_base->info.var);
               break;
             case CT_VB_PAIR:
               appendBuff("(");      length++;
               length += _showCT_VarBase(var_base->info.pair.l, BFALSE);
               appendBuff(", ");     length += 2;
               length += _showCT_VarBase(var_base->info.pair.r, BFALSE);
               appendBuff(")");      length++;
               break;
             default:
               printMsg(FATAL_MSG, "_showCT_VarBase - Unknown variable base (%d)", var_base->tag);
               break;
          }
     }

    return length;

}


/*********************************
 *                               *
 *    ctShowVarBase              *
 *                               *
 *********************************/
void
ctShowVarBase(CT_VAR_BASE *var_base) {

    if ( var_base )
        if ( var_base->tag == CT_VB_BANG )
            appendBuff("()");
        else
            _showCT_VarBase(var_base, BFALSE);


}   /*  end ctShowVarBase  */


/*********************************
 *                               *
 *    ctPreTranslate             *
 *                               *
 *********************************/

/* [#@] */

CT_EXPR *
ctPreTranslate (CT_EXPR *expr)     /* MAY BE NULL */
{
  if (expr)
    switch (expr->tag)
      {
      case CT_APP:
	expr->info.app.term = ctPreTranslateTerm (expr->info.app.term);
	expr->info.app.expr = ctPreTranslate     (expr->info.app.expr);

	break;

      case CT_PAIR:
	expr->info.pair.l = ctPreTranslate (expr->info.pair.l);
	expr->info.pair.r = ctPreTranslate (expr->info.pair.r);

	break;
      }

  return expr;
}


/*********************************
 *                               *
 *    ctPreTranslateTerm         *
 *                               *
 *********************************/

/* [#@] */

static
CT_TERM *
ctPreTranslateTerm (CT_TERM *term)
{
  char *parentType = NULL;
  int   numParams  = 0;
  int   index      = 0;

  assert (term);

  switch (term->tag)
    {
    case CT_T_FUNCTION:
      {
	CT_PHRASE **phrase = term->info.function->macros;

	while (*phrase)
	  {
	    (*phrase)->expr = ctPreTranslate ((*phrase)->expr);
	    phrase++;
	  }
      }

      break;

    case CT_T_MACRO:
      {
	CT_PHRASE **phrase = term->info.macro->macros;

	while (*phrase)
	  {
	    (*phrase)->expr = ctPreTranslate ((*phrase)->expr);
	    phrase++;
	  }
      }

      break;

    case CT_T_MAP:
      {
	CT_MAP_PHRASE **phrase = term->info.maps->phrases;

	while (*phrase)
	  {
	    (*phrase)->expr     = ctPreTranslate ((*phrase)->expr);

	    if ((*phrase)->neg_expr)
	      (*phrase)->neg_expr = ctPreTranslate ((*phrase)->neg_expr);

	    phrase++;
	  }
      }

      break;

    case CT_T_CASE:
      {
	CT_CASE **cse = term->info.cases;

	while (*cse)
	  {
	    (*cse)->expr = ctPreTranslate ((*cse)->expr);
	    cse++;
	  }
      }

      break;

    case CT_T_RECORD:
      {
	CT_RECORD **record = term->info.records;

	while (*record)
	  {
	    (*record)->expr = ctPreTranslate ((*record)->expr);
	    record++;
	  }
      }

      break;

    case CT_T_ABS:
      term->info.abs->expr = ctPreTranslate (term->info.abs->expr);

      break;

    case CT_T_FOLD:
      {
	CT_CATA *cata = (CT_CATA *)MHA (tmpHD, 1, sizeof (CT_CATA));

	assert (cata);

	parentType = getStructorParent (term->info.folds[0]->constr);
	numParams  = getNumStructors (parentType);

	cata->self    = getUniqueInt ();
	cata->context = (CT_EXPR *)MHA (tmpHD, 1, sizeof (CT_EXPR));
	cata->folds   = (CT_FOLD **)MHA (tmpHD, numParams + 1, sizeof (CT_FOLD *));

	assert (cata->context);
	assert (cata->folds);

	for (index = 0; index < numParams; index++)
	  cata->folds[index] = ctPreTranslateFold (term->info.folds[index], cata->self, cata->context);

	cata->folds[index] = NULL;

	term->tag       = CT_T_CATA;
	term->info.cata = cata;
      }

      break;

    case CT_T_UNFOLD:
      {
	CT_ANA *ana = (CT_ANA *)MHA (tmpHD, 1, sizeof (CT_ANA));

	assert (ana);

	parentType = getStructorParent (term->info.unfolds[0]->destr);
	numParams  = getNumStructors (parentType);

	ana->self    = getUniqueInt ();
	ana->context = (CT_EXPR *)MHA (tmpHD, 1, sizeof (CT_EXPR));
	ana->unfolds = (CT_UNFOLD **)MHA (tmpHD, numParams + 1, sizeof (CT_UNFOLD *));

	assert (ana->context);
	assert (ana->unfolds);

	for (index = 0; index < numParams; index++)
	  ana->unfolds[index] = ctPreTranslateUnfold (term->info.unfolds[index], ana->self, ana->context);

	ana->unfolds[index] = NULL;

	term->tag      = CT_T_ANA;
	term->info.ana = ana;
      }

      break;

    case CT_T_CATA:
    case CT_T_ANA:
    case CT_T_SELF:
/*      assert (BFALSE);      PATTERN MATCHER GENERATES DAGS, NOT TREES! */

      break;
    }

  return term;
}


/*********************************
 *                               *
 *    ctPreTranslateFold         *
 *                               *
 *********************************/

/* [#@] */

static
CT_FOLD *
ctPreTranslateFold (CT_FOLD *fold,
		    int      self,
		    CT_EXPR *context)
{
  CT_FOLD *newFold = (CT_FOLD *)MHA (tmpHD, 1, sizeof (CT_FOLD));

  assert (fold);
  assert (context);
  assert (newFold);

  newFold->constr   = fold->constr;
  newFold->var_base = _makeVarBase (tmpHD, HASH_NAME);
  newFold->expr     = _makeAppExpr (tmpHD,
				    _makeAbsTerm (tmpHD, fold->var_base, fold->expr),
				    _makeAppExpr (tmpHD,
						  ct_mapEi (getStructorType (fold->constr), self, context),
						  _makeVarExpr (tmpHD, HASH_NAME)));

  return newFold;
}


/*********************************
 *                               *
 *    ctPreTranslateUnfold       *
 *                               *
 *********************************/

/* [#@] */

static
CT_UNFOLD *
ctPreTranslateUnfold (CT_UNFOLD *unfold,
		      int        self,
		      CT_EXPR   *context)
{
  CT_UNFOLD *newUnfold = (CT_UNFOLD *)MHA (tmpHD, 1, sizeof (CT_UNFOLD));

  assert (unfold);
  assert (context);
  assert (newUnfold);

  newUnfold->destr     = unfold->destr;
  newUnfold->var_base  = unfold->var_base;
  newUnfold->var_base2 = unfold->var_base2;
  newUnfold->expr      = _makeAppExpr (tmpHD,
				       ct_mapEi (getStructorType (unfold->destr), self, context),
				       unfold->expr);

  return newUnfold;
}


/*********************************
 *                               *
 *    ct_map1                    *
 *                               *
 *********************************/

/* [#@] */

static
CT_TERM *
ct_map1 (void)
{
  return _makeAbsTerm (tmpHD,
		       _make0tupleBase (tmpHD),
		       _make0tupleExpr (tmpHD));
}


/*********************************
 *                               *
 *    ct_mapProd                 *
 *                               *
 *********************************/

/* [#@] */

static
CT_TERM *
ct_mapProd (CT_TERM *l,
	    CT_TERM *r)
{
  char *x = makeNewRsrvdVar (tmpHD);
  char *y = makeNewRsrvdVar (tmpHD);

  return _makeAbsTerm (tmpHD,
		       _make2tupleBase (tmpHD,
					_makeVarBase (tmpHD, x),
					_makeVarBase (tmpHD, y)),
		       _make2tupleExpr (tmpHD,
					_makeAppExpr (tmpHD, l, _makeVarExpr (tmpHD, x)),
					_makeAppExpr (tmpHD, r, _makeVarExpr (tmpHD, y))));
}


/*********************************
 *                               *
 *    ct_mapUserData             *
 *                               *
 *********************************/

/* [#@] */

static
CT_TERM *
ct_mapUserData (ST_TYPE *type,
		int      self,
		CT_EXPR *context)
{
  CT_TERM    *result    = (CT_TERM *)MHA (tmpHD, 1, sizeof (CT_TERM));
  int         index     = 0;
  int         numParams = 0;
  V_VARIANCE *varity    = NULL;

  assert (type);
  assert (context);
  assert (result);

  result->tag       = CT_T_MAP;
  result->info.maps = (CT_MAP *)MHA (tmpHD, 1, sizeof (CT_MAP));

  assert (result->info.maps);

  result->info.maps->type_name = st_KeyToName (type->info.user_data.key);

  assert (result->info.maps->type_name);

  numParams = st_GetNumParams (type->info.user_data.key);
  varity    = st_GetVarity (type->info.user_data.key);

  result->info.maps->phrases = (CT_MAP_PHRASE **)MHA (tmpHD, numParams + 1, sizeof (CT_MAP_PHRASE *));

  assert (result->info.maps->phrases);

  for (index = 0; index < numParams; index++)
    {
      result->info.maps->phrases[index] = (CT_MAP_PHRASE *)MHA (tmpHD, 1, sizeof (CT_MAP_PHRASE));

      assert (result->info.maps->phrases[index]);

      switch (varity[index])
	{
	case V_NEITHER:
	  result->info.maps->phrases[index]->var_base = NULL;
	  result->info.maps->phrases[index]->expr     = NULL;

	  result->info.maps->phrases[index]->neg_var_base = NULL;
	  result->info.maps->phrases[index]->neg_expr     = NULL;

	  break;

	case V_POSITIVE:
	  {
	    char *x = makeNewRsrvdVar (tmpHD);

	    assert (x);

	    result->info.maps->phrases[index]->var_base = _makeVarBase (tmpHD, x);
	    result->info.maps->phrases[index]->expr     =
	      _makeAppExpr (tmpHD,
			    ct_mapEi (type->info.user_data.args[index], self, context),
			    _makeVarExpr (tmpHD, x));

	    result->info.maps->phrases[index]->neg_var_base = NULL;
	    result->info.maps->phrases[index]->neg_expr     = NULL;
	  }

	  break;

	case V_NEGATIVE:
	  {
	    char *x = makeNewRsrvdVar (tmpHD);

	    assert (x);

	    result->info.maps->phrases[index]->var_base = NULL;
	    result->info.maps->phrases[index]->expr     = NULL;

	    result->info.maps->phrases[index]->neg_var_base = _makeVarBase (tmpHD, x);
	    result->info.maps->phrases[index]->neg_expr     =
	      _makeAppExpr (tmpHD,
			    ct_mapEi (type->info.user_data.args[index], self, context),
			    _makeVarExpr (tmpHD, x));
	  }

	  break;

	case V_BOTH:
	  {
	    char *x = makeNewRsrvdVar (tmpHD);

	    assert (x);

	    result->info.maps->phrases[index]->var_base = _makeVarBase (tmpHD, x);
	    result->info.maps->phrases[index]->expr     =
	      _makeAppExpr (tmpHD,
			    ct_mapEi (type->info.user_data.args[index], self, context),
			    _makeVarExpr (tmpHD, x));

	    result->info.maps->phrases[index]->neg_var_base = _makeVarBase (tmpHD, x);
	    result->info.maps->phrases[index]->neg_expr     =
	      _makeAppExpr (tmpHD,
			    ct_mapEi (type->info.user_data.args[index], self, context),
			    _makeVarExpr (tmpHD, x));
	  }

	  break;

	default:
	  assert (BFALSE);
	}
    }

  result->info.maps->phrases[index] = NULL;

  return result;
}


/*********************************
 *                               *
 *    ct_mapEi                   *
 *                               *
 *********************************/

/* [#@] GENERALIZE THIS FUNCTION---CURRENTLY HANDLES MAPS FOR FOLDS AND UNFOLDS ONLY! */

static
CT_TERM *
ct_mapEi (ST_TYPE *type,
	  int      self,
	  CT_EXPR *context)
{
  CT_TERM *result = NULL;

  assert (type);
  assert (context);

  switch (type->tag)
    {
    case TYPE_1:
      result = ct_map1 ();

      break;

    case TYPE_PROD:
      result = ct_mapProd (ct_mapEi (type->info.prod.l, self, context),
			   ct_mapEi (type->info.prod.r, self, context));

      break;

    case TYPE_USER_DATA:
      result = ct_mapUserData (type, self, context);

      break;

    case TYPE_STATE_VAR:
      {
	CT_TERM *selfTerm = (CT_TERM *)MHA (tmpHD, 1, sizeof (CT_TERM));
	char    *x        = makeNewRsrvdVar (tmpHD);

	assert (selfTerm);

	selfTerm->tag       = CT_T_SELF;
	selfTerm->info.self = self;

	result = _makeAbsTerm (tmpHD,
			       _makeVarBase (tmpHD,
					     x),
			       _makeAppExpr (tmpHD,
					     selfTerm,
					     _make2tupleExpr (tmpHD,
							      _makeVarExpr (tmpHD, x),
							      context)));
      }

      break;

    case TYPE_PARAMETRIC_VAR:
    case TYPE_BUILTIN_CHAR:
    case TYPE_BUILTIN_INT:
      result = _makeId (tmpHD);

      break;

    default:
      assert (BFALSE);
    }

  assert (result);

  return result;
}
