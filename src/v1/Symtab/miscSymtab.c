#include <stdarg.h>
#include "symtab.h"
#include "symtabI.h"
#include "ioChar.h"
#include "codetab.h"
#include "lib.h"
#include "machine.h"     /* [BI] ADDED THIS INCLUDE */

int prodNestedDepth = -1;       /* used in showDomain to track nesting level */
int sumNestedDepth = -1;        /* used in showDomain to track nesting level */
ST_KEY               PROD_KEY;
ST_KEY               TERM_KEY;

ST_KEY CHAR_KEY;     /* [BI] ADDED (SEE addSymtab.c) */
ST_KEY INT_KEY;

/****************************************************************
 *                                                              *
 * Prototypes For Private Symbol Table Misc Functions           *
 *                                                              *
 ****************************************************************/

static void      LoadBool               (void);     /* [BI] ADDED (SEE BELOW) */
static void      LoadChar               (void);
static void      LoadInt                (void);
static void      LoadList               (void);
static ST_ENTRY *LoadBuiltIn            (char        *name);
static ST_ENTRY *LoadBuiltInConstructor (char        *name);
static void      LoadBuiltinFunctions   (void);
static void      LoadBuiltinFunction    (char        *name,
					 ST_TYPE_SIG *typeSig,
					 M_INSTR_TAG  instr);

static ST_ENTRY   *st_LoadBaseStructor(char *sName, ST_ENTRY *parent,int posn);
static ST_ENTRY   *st_LoadBaseDatatype(char *tName,
				   ST_DT_KIND class,
				   int numParams, 
				   int numStructs);
static void        loadProdDatatype(void);
static void        loadTerminalDatatype(void);
static void        loadIdentityCombinator(void);
static void        loadIdentity1Combinator(void);
static void        loadSumDatatype(void);
static void        loadBIDatatype(char *typeName, char *constrName);

static void        st_ShowDatatype(ST_KEY typeKey);
static void        st_ShowAlias(ST_ENTRY *entry);
static void        st_ShowStructor(ST_KEY structorKey);
static void        st_ShowStructDomain(ST_TYPE *domain, 
				       ST_TYPE  *parentTypeSig);
static void        st_ShowFunction(ST_KEY funKey);
static void        st_ShowCombinator(ST_KEY key);
static void        st_ShowMacro(ST_KEY macroKey);
static void        st_ShowVar(ST_KEY varKey);

static void      st_ShowParamRep(ST_PVAR parameter); /* converts parameter to ascii */
static void      st_ShowDomain(ST_TYPE *domain);
static void      st_ShowTypeParams(ST_TYPE_SIG **params);
static void      st_ShowTypeSigByKey(ST_KEY key);

/* print functions  (in miscSymtab.c) */
static void        st_PrintStructor(ST_KEY structorKey);
static void        st_PrintDatatype(ST_KEY typeKey);
static void        st_PrintAlias(ST_ENTRY *entry);
static void        st_PrintFunction(ST_KEY funKey);
static void        st_PrintCombinator(ST_KEY key);
static void        st_PrintMacro(ST_KEY macroKey);
static void        st_PrintVar(ST_KEY varKey);

/* probably can improve these: */
static void st_ShowTypeParamsLessContext(ST_TYPE_SIG **params);
static void st_ShowSigLessContext(ST_TYPE_SIG *sig);
static void st_ShowDomainLessContext(ST_TYPE *st_type);

/**************************************************************************** 
 *                                                                          *
 *    Globally Acting Function Definitions                                  *
 *                                                                          *
 ****************************************************************************/


/*********************************
 *                               *
 *    loadBaseTypes              *
 *                               *
 *********************************/

void
loadBaseTypes (void)
{
  loadProdDatatype        ();
  loadTerminalDatatype    ();
  loadIdentityCombinator  ();
  loadIdentity1Combinator ();

  LoadBool ();                 /* [BI] LOAD BUILTIN TYPES     INTO THE SYMBOL TABLE */
  LoadChar ();
  LoadInt  ();
  LoadList ();

  LoadBuiltinFunctions ();     /* [BI] LOAD BUILTIN FUNCTIONS INTO THE SYMBOL TABLE */

/*
  loadBIDatatype (STRING_TYPENAME, STRING_CONSTRUCTOR);
  loadBIDatatype (INT_TYPENAME,    INT_CONSTRUCTOR);
  loadBIDatatype (CHAR_TYPENAME,   CHAR_CONSTRUCTOR);
*/
}


/*********************************
 *                               *
 *    LoadBool                   *
 *                               *
 *********************************/

/* [BI] LOAD bool INTO THE SYMBOL TABLE: */

static
void
LoadBool (void)
{
  ST_ENTRY *boolType = (ST_ENTRY *)MHA (symTab->scopeHD, 1, sizeof (ST_ENTRY));
  ST_ENTRY *false    = (ST_ENTRY *)MHA (symTab->scopeHD, 1, sizeof (ST_ENTRY));
  ST_ENTRY *true     = (ST_ENTRY *)MHA (symTab->scopeHD, 1, sizeof (ST_ENTRY));

  ST_TYPE *unit = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));
  ST_TYPE *bool = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));

  ST_TYPE_SIG *unitToBool = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));

  assert (boolType);
  assert (false);
  assert (true);
  assert (unit);
  assert (bool);
  assert (unitToBool);

  boolType->tag  = ST_DATATYPE;
  boolType->name = libStrdup (symTab->scopeHD, BOOL_TYPENAME);
  boolType->key  = st_MakeKey ();

  boolType->info.datatype.class            = DT_INDUCTIVE;
  boolType->info.datatype.numParams        = 0;
  boolType->info.datatype.varity           = NULL;
  boolType->info.datatype.numStructors     = 2;
  boolType->info.datatype.structorNames    = (char **)MHA (symTab->scopeHD, 3, sizeof (char *));
  boolType->info.datatype.structorKeys     = (ST_KEY *)MHA (symTab->scopeHD, 3, sizeof (ST_KEY));
  boolType->info.datatype.opCombKeys       = (ST_KEY *)MHA (symTab->scopeHD, 4, sizeof (ST_KEY));
  boolType->info.datatype.genericStateType = st_MakeGenericStateType (boolType);

  boolType->info.datatype.structorNames[2] = NULL;
  boolType->info.datatype.structorKeys[2]  = NULL;
  boolType->info.datatype.opCombKeys[3]    = NULL;

  unit->tag = TYPE_1;

  bool->tag                 = TYPE_STATE_VAR;

  unitToBool->domain        = unit;
  unitToBool->codomain      = bool;
  unitToBool->negDomain     = NULL;
  unitToBool->negCodomain   = NULL;
  unitToBool->params        = NULL;
  unitToBool->userspecified = BFALSE;

  boolType->info.datatype.structorNames[0] = libStrdup (symTab->scopeHD, FALSE_CONSTRUCTORNAME);
  boolType->info.datatype.structorKeys[0]  = st_MakeKey ();

  false->tag  = ST_STRUCTOR;
  false->name = libStrdup (symTab->scopeHD, FALSE_CONSTRUCTORNAME);
  false->key  = boolType->info.datatype.structorKeys[0];

  false->info.structor.posn     = 0;
  false->info.structor.parent   = boolType;
  false->info.structor.type_sig = unitToBool;
  false->info.structor.isHO     = BFALSE;

  addEntryToSymTab (false);

  boolType->info.datatype.structorNames[1] = libStrdup (symTab->scopeHD, TRUE_CONSTRUCTORNAME);
  boolType->info.datatype.structorKeys[1]  = st_MakeKey ();

  true->tag  = ST_STRUCTOR;
  true->name = libStrdup (symTab->scopeHD, TRUE_CONSTRUCTORNAME);
  true->key  = boolType->info.datatype.structorKeys[1];

  true->info.structor.posn     = 1;
  true->info.structor.parent   = boolType;
  true->info.structor.type_sig = unitToBool;
  true->info.structor.isHO     = BFALSE;

  addEntryToSymTab (true);

  addEntryToSymTab (boolType);

  st_AddOpCombs (boolType->key);
}


/*********************************
 *                               *
 *    LoadList                   *
 *                               *
 *********************************/

/* [BI] LOAD bool INTO THE SYMBOL TABLE: */

static
void
LoadList (void)
{
  ST_ENTRY *listType = (ST_ENTRY *)MHA (symTab->scopeHD, 1, sizeof (ST_ENTRY));
  ST_ENTRY *nil      = (ST_ENTRY *)MHA (symTab->scopeHD, 1, sizeof (ST_ENTRY));
  ST_ENTRY *cons     = (ST_ENTRY *)MHA (symTab->scopeHD, 1, sizeof (ST_ENTRY));

  ST_TYPE *unit          = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));
  ST_TYPE *typeVar       = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));
  ST_TYPE *list          = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));
  ST_TYPE *typeVarByList = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));

  ST_TYPE_SIG *unitToList          = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));
  ST_TYPE_SIG *typeVarByListToList = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));

  assert (listType);
  assert (nil);
  assert (cons);
  assert (unit);
  assert (typeVar);
  assert (list);
  assert (typeVarByList);
  assert (unitToList);
  assert (typeVarByListToList);

  listType->tag  = ST_DATATYPE;
  listType->name = libStrdup (symTab->scopeHD, LIST_TYPENAME);
  listType->key  = st_MakeKey ();

  listType->info.datatype.class            = DT_INDUCTIVE;
  listType->info.datatype.numParams        = 1;
  listType->info.datatype.varity           = (V_VARIANCE *)MHA (symTab->scopeHD, 1, sizeof (V_VARIANCE));
  listType->info.datatype.numStructors     = 2;
  listType->info.datatype.structorNames    = (char **)MHA (symTab->scopeHD, 3, sizeof (char *));
  listType->info.datatype.structorKeys     = (ST_KEY *)MHA (symTab->scopeHD, 3, sizeof (ST_KEY));
  listType->info.datatype.opCombKeys       = (ST_KEY *)MHA (symTab->scopeHD, 4, sizeof (ST_KEY));
  listType->info.datatype.genericStateType = st_MakeGenericStateType (listType);

  listType->info.datatype.varity[0] = V_POSITIVE;

  listType->info.datatype.structorNames[2] = NULL;
  listType->info.datatype.structorKeys[2]  = NULL;
  listType->info.datatype.opCombKeys[3]    = NULL;

  unit->tag = TYPE_1;

  typeVar->tag            = TYPE_PARAMETRIC_VAR;
  typeVar->info.param_var = 0;

  list->tag                 = TYPE_STATE_VAR;

  unitToList->domain        = unit;
  unitToList->codomain      = list;
  unitToList->negDomain     = NULL;
  unitToList->negCodomain   = NULL;
  unitToList->params        = NULL;
  unitToList->userspecified = BFALSE;

  typeVarByList->tag           = TYPE_PROD;
  typeVarByList->info.prod.key = PROD_KEY;
  typeVarByList->info.prod.l   = typeVar;
  typeVarByList->info.prod.r   = list;

  typeVarByListToList->domain        = typeVarByList;
  typeVarByListToList->codomain      = list;
  typeVarByListToList->negDomain     = NULL;
  typeVarByListToList->negCodomain   = NULL;
  typeVarByListToList->params        = NULL;
  typeVarByListToList->userspecified = BFALSE;

  listType->info.datatype.structorNames[0] = libStrdup (symTab->scopeHD, NIL_CONSTRUCTORNAME);
  listType->info.datatype.structorKeys[0]  = st_MakeKey ();

  nil->tag  = ST_STRUCTOR;
  nil->name = libStrdup (symTab->scopeHD, NIL_CONSTRUCTORNAME);
  nil->key  = listType->info.datatype.structorKeys[0];

  nil->info.structor.posn     = 0;
  nil->info.structor.parent   = listType;
  nil->info.structor.type_sig = unitToList;
  nil->info.structor.isHO     = BFALSE;

  addEntryToSymTab (nil);

  listType->info.datatype.structorNames[1] = libStrdup (symTab->scopeHD, CONS_CONSTRUCTORNAME);
  listType->info.datatype.structorKeys[1]  = st_MakeKey ();

  cons->tag  = ST_STRUCTOR;
  cons->name = libStrdup (symTab->scopeHD, CONS_CONSTRUCTORNAME);
  cons->key  = listType->info.datatype.structorKeys[1];

  cons->info.structor.posn     = 1;
  cons->info.structor.parent   = listType;
  cons->info.structor.type_sig = typeVarByListToList;
  cons->info.structor.isHO     = BFALSE;

  addEntryToSymTab (cons);

  addEntryToSymTab (listType);

  st_AddOpCombs (listType->key);
}


/*********************************
 *                               *
 *    LoadChar                   *
 *                               *
 *********************************/

/* [BI] LOAD char INTO THE SYMBOL TABLE: */

static
void
LoadChar (void)
{
  ST_ENTRY *charType        = NULL;
  ST_ENTRY *charConstructor = NULL;

  charType        = LoadBuiltIn (CHAR_TYPENAME);
  charConstructor = LoadBuiltInConstructor (CHAR_CONSTRUCTOR);

  charType->info.datatype.structorNames[0] = charConstructor->name;
  charType->info.datatype.structorKeys [0] = charConstructor->key;
  charConstructor->info.structor.parent    = charType;

  CHAR_KEY = charType->key;
}


/*********************************
 *                               *
 *    LoadInt                    *
 *                               *
 *********************************/

/* [BI] LOAD int INTO THE SYMBOL TABLE: */

static
void
LoadInt (void)
{
  ST_ENTRY *intType        = NULL;
  ST_ENTRY *intConstructor = NULL;

  intType        = LoadBuiltIn (INT_TYPENAME);
  intConstructor = LoadBuiltInConstructor (INT_CONSTRUCTOR);

  intType->info.datatype.structorNames[0] = intConstructor->name;
  intType->info.datatype.structorKeys [0] = intConstructor->key;
  intConstructor->info.structor.parent    = intType;

  INT_KEY = intType->key;
}


/*********************************
 *                               *
 *    LoadBuiltIn                *
 *                               *
 *********************************/

/* [BI] LOAD BUILTINS INTO THE SYMBOL TABLE: */

static
ST_ENTRY *
LoadBuiltIn (char *name)
{
  ST_ENTRY *newType = NULL;

  assert (name);

  newType = (ST_ENTRY *)MHA (symTab->scopeHD, 1, sizeof (ST_ENTRY));

  assert (newType);

  newType->tag  = ST_DATATYPE;
  newType->name = libStrdup (symTab->scopeHD, name);
  newType->key  = st_MakeKey ();

  newType->info.datatype.class            = DT_INDUCTIVE;
  newType->info.datatype.numParams        = 0;
  newType->info.datatype.varity           = NULL;
  newType->info.datatype.numStructors     = 1;
  newType->info.datatype.structorKeys     = (ST_KEY *)MHA (symTab->scopeHD, 2, sizeof (ST_KEY));
  newType->info.datatype.structorNames    = (char **)MHA (symTab->scopeHD, 2, sizeof (char *));
  newType->info.datatype.opCombKeys       = NULL;
  newType->info.datatype.genericStateType = NULL;

  newType->info.datatype.structorKeys [1] = NULL;
  newType->info.datatype.structorNames[1] = NULL;

  addEntryToSymTab (newType);

  return newType;
}


/*********************************
 *                               *
 *    LoadBuiltInConstructor     *
 *                               *
 *********************************/

/*
 * [BI] LOAD BUILTIN "CONSTRUCTORS" INTO THE SYMBOL TABLE
 *
 *      (THESE ARE NOT TRUE CONSTRUCTORS, BUT ARE NEEDED BY
 *       THE PATTERN MATCHER)
 *
 */

static
ST_ENTRY *
LoadBuiltInConstructor (char *name)
{
  ST_ENTRY *constructor = NULL;

  assert (name);

  constructor = (ST_ENTRY *)MHA (symTab->scopeHD, 1, sizeof (ST_ENTRY));

  assert (constructor);

  constructor->tag  = ST_STRUCTOR;
  constructor->name = libStrdup (symTab->scopeHD, name);
  constructor->key  = st_MakeKey ();

  constructor->info.structor.posn     = 0;
  constructor->info.structor.parent   = NULL;
  constructor->info.structor.type_sig = NULL;
  constructor->info.structor.isHO     = BFALSE;

  addEntryToSymTab (constructor);

  return constructor;
}


/*********************************
 *                               *
 *    LoadBuiltinFunctions       *
 *                               *
 *********************************/

/* [BI] LOAD BUILTIN FUNCTIONS INTO THE SYMBOL TABLE: */

static
void
LoadBuiltinFunctions (void)
{
  ST_TYPE *typeInt        = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));
  ST_TYPE *typeChar       = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));
  ST_TYPE *typeBool       = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));
  ST_TYPE *typeIntByInt   = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));
  ST_TYPE *typeCharByChar = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));
  ST_TYPE *typeBoolByBool = (ST_TYPE *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE));

  ST_TYPE_SIG *typeIntByIntToInt    = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));
  ST_TYPE_SIG *typeIntByIntToBool   = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));
  ST_TYPE_SIG *typeCharByCharToBool = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));
  ST_TYPE_SIG *typeBoolByBoolToBool = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));
  ST_TYPE_SIG *typeCharToInt        = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));
  ST_TYPE_SIG *typeIntToChar        = (ST_TYPE_SIG *)MHA (symTab->scopeHD, 1, sizeof (ST_TYPE_SIG));

  assert (typeInt);
  assert (typeChar);
  assert (typeBool);
  assert (typeIntByInt);
  assert (typeCharByChar);
  assert (typeBoolByBool);
  assert (typeIntByIntToInt);
  assert (typeIntByIntToBool);
  assert (typeCharByCharToBool);
  assert (typeBoolByBoolToBool);
  assert (typeCharToInt);
  assert (typeIntToChar);

  typeInt->tag = TYPE_BUILTIN_INT;

  typeChar->tag = TYPE_BUILTIN_CHAR;

  typeBool->tag                 = TYPE_USER_DATA;
  typeBool->info.user_data.key  = st_NameToKey (BOOL_TYPENAME);
  typeBool->info.user_data.args = NULL;

  assert (typeBool->info.user_data.key);

  typeIntByInt->tag           = TYPE_PROD;
  typeIntByInt->info.prod.key = PROD_KEY;
  typeIntByInt->info.prod.l   = typeInt;
  typeIntByInt->info.prod.r   = typeInt;

  typeCharByChar->tag           = TYPE_PROD;
  typeCharByChar->info.prod.key = PROD_KEY;
  typeCharByChar->info.prod.l   = typeChar;
  typeCharByChar->info.prod.r   = typeChar;

  typeBoolByBool->tag           = TYPE_PROD;
  typeBoolByBool->info.prod.key = PROD_KEY;
  typeBoolByBool->info.prod.l   = typeBool;
  typeBoolByBool->info.prod.r   = typeBool;

  typeIntByIntToInt->domain        = typeIntByInt;
  typeIntByIntToInt->codomain      = typeInt;
  typeIntByIntToInt->negDomain     = NULL;
  typeIntByIntToInt->negCodomain   = NULL;
  typeIntByIntToInt->params        = NULL;
  typeIntByIntToInt->userspecified = BFALSE;

  typeIntByIntToBool->domain        = typeIntByInt;
  typeIntByIntToBool->codomain      = typeBool;
  typeIntByIntToBool->negDomain     = NULL;
  typeIntByIntToBool->negCodomain   = NULL;
  typeIntByIntToBool->params        = NULL;
  typeIntByIntToBool->userspecified = BFALSE;

  typeCharByCharToBool->domain        = typeCharByChar;
  typeCharByCharToBool->codomain      = typeBool;
  typeCharByCharToBool->negDomain     = NULL;
  typeCharByCharToBool->negCodomain   = NULL;
  typeCharByCharToBool->params        = NULL;
  typeCharByCharToBool->userspecified = BFALSE;

  typeBoolByBoolToBool->domain        = typeBoolByBool;
  typeBoolByBoolToBool->codomain      = typeBool;
  typeBoolByBoolToBool->negDomain     = NULL;
  typeBoolByBoolToBool->negCodomain   = NULL;
  typeBoolByBoolToBool->params        = NULL;
  typeBoolByBoolToBool->userspecified = BFALSE;

  typeCharToInt->domain        = typeChar;
  typeCharToInt->codomain      = typeInt;
  typeCharToInt->negDomain     = NULL;
  typeCharToInt->negCodomain   = NULL;
  typeCharToInt->params        = NULL;
  typeCharToInt->userspecified = BFALSE;

  typeIntToChar->domain        = typeInt;
  typeIntToChar->codomain      = typeChar;
  typeIntToChar->negDomain     = NULL;
  typeIntToChar->negCodomain   = NULL;
  typeIntToChar->params        = NULL;
  typeIntToChar->userspecified = BFALSE;

  LoadBuiltinFunction (ADD_INT, typeIntByIntToInt, MCadd);
  LoadBuiltinFunction (SUB_INT, typeIntByIntToInt, MCsub);
  LoadBuiltinFunction (MUL_INT, typeIntByIntToInt, MCmul);
  LoadBuiltinFunction (DIV_INT, typeIntByIntToInt, MCdiv);
  LoadBuiltinFunction (MOD_INT, typeIntByIntToInt, MCmod);

  LoadBuiltinFunction (LT_INT, typeIntByIntToBool, MClt_int);
  LoadBuiltinFunction (LE_INT, typeIntByIntToBool, MCle_int);
  LoadBuiltinFunction (GT_INT, typeIntByIntToBool, MCgt_int);
  LoadBuiltinFunction (GE_INT, typeIntByIntToBool, MCge_int);
  LoadBuiltinFunction (EQ_INT, typeIntByIntToBool, MCeq_int);

  LoadBuiltinFunction (LT_CHAR, typeCharByCharToBool, MClt_char);
  LoadBuiltinFunction (LE_CHAR, typeCharByCharToBool, MCle_char);
  LoadBuiltinFunction (GT_CHAR, typeCharByCharToBool, MCgt_char);
  LoadBuiltinFunction (GE_CHAR, typeCharByCharToBool, MCge_char);
  LoadBuiltinFunction (EQ_CHAR, typeCharByCharToBool, MCeq_char);

  LoadBuiltinFunction (CODE,   typeCharToInt, MCcode);
  LoadBuiltinFunction (DECODE, typeIntToChar, MCdecode);
}


/*********************************
 *                               *
 *    LoadBuiltinFunction        *
 *                               *
 *********************************/

/* [BI] LOAD A GIVEN BUILTIN FUNCTION INTO THE SYMBOL TABLE: */

static
void
LoadBuiltinFunction (char        *name,
		     ST_TYPE_SIG *typeSig,
		     M_INSTR_TAG  instr)
{
  ST_ENTRY *newFunction = NULL;

  assert (name);
  assert (typeSig);

  newFunction = (ST_ENTRY *)MHA (symTab->scopeHD, 1, sizeof (ST_ENTRY));

  assert (newFunction);

  newFunction->tag  = ST_FUNCTION;
  newFunction->name = libStrdup (symTab->scopeHD, name);
  newFunction->key  = st_MakeKey ();

  newFunction->info.function.macroKeys = NULL;
  newFunction->info.function.numMacros = 0;
  newFunction->info.function.type_sig  = typeSig;
  newFunction->info.function.instr     = instr;

  addEntryToSymTab (newFunction);
}


/*********************************
 *                               *
 *    st_LoadBaseStructor        *
 *                               *
 *********************************/
ST_ENTRY *
st_LoadBaseStructor(char *sName, ST_ENTRY *parent, int posn) {
/* doesn't actually load up any type signature. Just allocates memory */

  ST_ENTRY *structor = (ST_ENTRY *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_ENTRY));

  structor->tag = ST_STRUCTOR;
  structor->name = (char *)MemHeapAlloc(symTab->scopeHD, strlen(sName)+1, sizeof(char));
  strcpy(structor->name, sName);
  structor->key = st_MakeKey();

  structor->info.structor.parent = parent;
  structor->info.structor.posn   = posn;

  structor->info.structor.type_sig = 
    (ST_TYPE_SIG *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE_SIG));
  structor->info.structor.type_sig->params = NULL;
  structor->info.structor.type_sig->domain = 
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  structor->info.structor.type_sig->codomain = 
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));

  addEntryToSymTab(structor);

  return(structor);

}


/*********************************
 *                               *
 *    st_LoadBaseDatatype        *
 *                               *
 *********************************/
ST_ENTRY *
st_LoadBaseDatatype(char *tName,ST_DT_KIND class,int numParams,int numStructs){

  ST_ENTRY   *dType  = (ST_ENTRY *)MHA(symTab->scopeHD, 1, sizeof(ST_ENTRY));
  int         i;
  char       *structName;

  dType->tag = ST_DATATYPE;
  dType->name = libStrdup(symTab->scopeHD, tName);
  dType->key = st_MakeKey();
  
  dType->info.datatype.class = class;
  dType->info.datatype.numParams = numParams;
  dType->info.datatype.numStructors = numStructs;
  dType->info.datatype.structorNames = 
    (char **)MemHeapAlloc(symTab->scopeHD, numStructs+1, sizeof(char *));
  dType->info.datatype.structorKeys = 
    (ST_KEY *)MemHeapAlloc(symTab->scopeHD, numStructs, sizeof(ST_KEY));
  dType->info.datatype.opCombKeys =   /* always 3 of these */
    (ST_KEY *)MemHeapAlloc(symTab->scopeHD, 3, sizeof(ST_KEY));
  dType->info.datatype.genericStateType = st_MakeGenericStateType(dType);

  addEntryToSymTab(dType);

  return(dType);

}   /*  end loadDatatype()  */


/*********************************
 *                               *
 *    loadProdDatatype           *
 *                               *
 *********************************/
static void
loadProdDatatype(void) {

  ST_ENTRY *prodType,
           *destrType,
           *prodComb,
           *mapProdComb;
  ST_TYPE  *st_type,
           *genStateType,
           *domain,
           *codomain;
  int       i, j;

  /* add PROD_TYPE data type */
  prodType = st_LoadBaseDatatype(PROD_TYPE, DT_COINDUCTIVE, 2, 2);
  PROD_KEY = prodType->key;

  /* add PROD0 destructor */
  destrType = 
    st_LoadBaseStructor(PROD0, prodType, 0);
  destrType->info.structor.type_sig->domain->tag = TYPE_STATE_VAR;
  destrType->info.structor.type_sig->codomain->tag = TYPE_PARAMETRIC_VAR;
  destrType->info.structor.type_sig->codomain->info.parametric_var = 0;

  prodType->info.datatype.structorNames[0] = destrType->name;
  prodType->info.datatype.structorKeys[0] = destrType->key;
  
  /* add PROD1 destructor */
  destrType = 
    st_LoadBaseStructor(PROD1, prodType, 1);
  destrType->info.structor.type_sig->domain->tag = TYPE_STATE_VAR;
  destrType->info.structor.type_sig->codomain->tag = TYPE_PARAMETRIC_VAR;
  destrType->info.structor.type_sig->codomain->info.parametric_var = 1;

  prodType->info.datatype.structorNames[1] = destrType->name;
  prodType->info.datatype.structorKeys[1] = destrType->key;

  /* add "pair" (AKA prodrecord) combinator */
  prodComb =  (ST_ENTRY *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_ENTRY));
  prodComb->tag = ST_OPCOMB;
  prodComb->name = (char *)MemHeapAlloc(symTab->scopeHD,strlen(PRIM_PAIR)+1,sizeof(char));
  strcpy(prodComb->name, PRIM_PAIR);
  prodComb->key = st_MakeKey();
  
  prodComb->info.opcomb.numParams = 2;
  prodComb->info.opcomb.type_sig = 
    (ST_TYPE_SIG *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE_SIG));

  prodComb->info.opcomb.type_sig->params = 
    (ST_TYPE_SIG **)MemHeapAlloc(symTab->scopeHD, 3, sizeof(ST_TYPE_SIG *));
  for (i=0; i<2; i++) {
    prodComb->info.opcomb.type_sig->params[i] = 
      (ST_TYPE_SIG *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE_SIG));
    prodComb->info.opcomb.type_sig->params[i]->params = NULL;

    prodComb->info.opcomb.type_sig->params[i]->domain =
      (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
    prodComb->info.opcomb.type_sig->params[i]->domain->tag = TYPE_STATE_VAR;
    prodComb->info.opcomb.type_sig->params[i]->codomain =
      (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
    prodComb->info.opcomb.type_sig->params[i]->codomain->tag = 
      TYPE_PARAMETRIC_VAR;
    prodComb->info.opcomb.type_sig->params[i]->codomain->info.param_var = i;
  }   /*  rof  */

  prodComb->info.opcomb.type_sig->domain =
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  prodComb->info.opcomb.type_sig->domain->tag = TYPE_STATE_VAR;

  prodComb->info.opcomb.type_sig->codomain =
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  prodComb->info.opcomb.type_sig->codomain->tag = TYPE_PROD;
  prodComb->info.opcomb.type_sig->codomain->info.prod.key = PROD_KEY;
  prodComb->info.opcomb.type_sig->codomain->info.prod.l =
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  prodComb->info.opcomb.type_sig->codomain->info.prod.l->tag = 
    TYPE_PARAMETRIC_VAR;
  prodComb->info.opcomb.type_sig->codomain->info.prod.l->info.param_var = 0;

  prodComb->info.opcomb.type_sig->codomain->info.prod.r =
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  prodComb->info.opcomb.type_sig->codomain->info.prod.r->tag = 
    TYPE_PARAMETRIC_VAR;
  prodComb->info.opcomb.type_sig->codomain->info.prod.r->info.param_var = 1;

  addEntryToSymTab(prodComb);

  /* add "map_prod" combinator and type */
  /* map_prod{A * X -> A', B * X -> B'} : prod(A,B) * X -> prod(A', B') */
  mapProdComb =  (ST_ENTRY *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_ENTRY));
  mapProdComb->tag = ST_OPCOMB;
  mapProdComb->name = lb_BuildCombString(symTab->scopeHD, "map", PROD_TYPE);
  mapProdComb->key = st_MakeKey();
  
  mapProdComb->info.opcomb.numParams = 2;
  mapProdComb->info.opcomb.parent    = prodType;
  mapProdComb->info.opcomb.type_sig = 
    (ST_TYPE_SIG *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE_SIG));

  mapProdComb->info.opcomb.type_sig->params = 
    (ST_TYPE_SIG **)MemHeapAlloc(symTab->scopeHD, 3, sizeof(ST_TYPE_SIG *));
  j = 2;
  for (i=0; i<2; i++) {
    mapProdComb->info.opcomb.type_sig->params[i] = 
      (ST_TYPE_SIG *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE_SIG));
    mapProdComb->info.opcomb.type_sig->params[i]->params = NULL;

    mapProdComb->info.opcomb.type_sig->params[i]->domain =
      (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));


    /* A * X -> A' */
    /* domain */
    mapProdComb->info.opcomb.type_sig->params[i]->domain->tag = TYPE_PROD;
    mapProdComb->info.opcomb.type_sig->params[i]->domain->info.prod.key = PROD_KEY;
    
    mapProdComb->info.opcomb.type_sig->params[i]->domain->info.prod.l =
	 (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));

    mapProdComb->info.opcomb.type_sig->params[i]->domain->info.prod.l->tag = 
	 TYPE_PARAMETRIC_VAR;
    mapProdComb->info.opcomb.type_sig->params[i]->domain->info.prod.l->info.param_var =     
	 i;

    mapProdComb->info.opcomb.type_sig->params[i]->domain->info.prod.r =
	 (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));

    mapProdComb->info.opcomb.type_sig->params[i]->domain->info.prod.r->tag = 
	 TYPE_PARAMETRIC_VAR;
    mapProdComb->info.opcomb.type_sig->params[i]->domain->info.prod.r->info.param_var =     
	 4;  /* hard coded */

    /* codomain */
    mapProdComb->info.opcomb.type_sig->params[i]->codomain =
      (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
    mapProdComb->info.opcomb.type_sig->params[i]->codomain->tag = 
      TYPE_PARAMETRIC_VAR;
    mapProdComb->info.opcomb.type_sig->params[i]->codomain->info.param_var = j++;
  }   /*  rof  */

  /* prod(A,B) * X -> prod(A', B') */
  mapProdComb->info.opcomb.type_sig->domain =
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  mapProdComb->info.opcomb.type_sig->domain->tag = TYPE_PROD;
  mapProdComb->info.opcomb.type_sig->domain->info.prod.key = PROD_KEY;

  /* prod(A,B) */
  mapProdComb->info.opcomb.type_sig->domain->info.prod.l =
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  mapProdComb->info.opcomb.type_sig->domain->info.prod.l->tag = 
    TYPE_PROD;
  mapProdComb->info.opcomb.type_sig->domain->info.prod.l->info.prod.key = PROD_KEY;
  mapProdComb->info.opcomb.type_sig->domain->info.prod.l->info.prod.l =
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  mapProdComb->info.opcomb.type_sig->domain->info.prod.l->info.prod.l->tag =
       TYPE_PARAMETRIC_VAR;
  mapProdComb->info.opcomb.type_sig->domain->info.prod.l->info.prod.l->info.param_var =
       0;

  mapProdComb->info.opcomb.type_sig->domain->info.prod.l->info.prod.r =
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  mapProdComb->info.opcomb.type_sig->domain->info.prod.l->info.prod.r->tag =
       TYPE_PARAMETRIC_VAR;
  mapProdComb->info.opcomb.type_sig->domain->info.prod.l->info.prod.r->info.param_var =
       1;
  
  /* X */
  mapProdComb->info.opcomb.type_sig->domain->info.prod.r =
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  mapProdComb->info.opcomb.type_sig->domain->info.prod.r->tag = 
    TYPE_PARAMETRIC_VAR;
  mapProdComb->info.opcomb.type_sig->domain->info.prod.r->info.param_var = 4;

  /* prod(A', B') */
  mapProdComb->info.opcomb.type_sig->codomain =
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  mapProdComb->info.opcomb.type_sig->codomain->tag = TYPE_PROD;
  mapProdComb->info.opcomb.type_sig->codomain->info.prod.key = PROD_KEY;

  /* A' */
  mapProdComb->info.opcomb.type_sig->codomain->info.prod.l =
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  mapProdComb->info.opcomb.type_sig->codomain->info.prod.l->tag = 
    TYPE_PARAMETRIC_VAR;
  mapProdComb->info.opcomb.type_sig->codomain->info.prod.l->info.param_var = 2;
  
  /* B' */
  mapProdComb->info.opcomb.type_sig->codomain->info.prod.r =
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  mapProdComb->info.opcomb.type_sig->codomain->info.prod.r->tag = 
    TYPE_PARAMETRIC_VAR;
  mapProdComb->info.opcomb.type_sig->codomain->info.prod.r->info.param_var = 3;

  addEntryToSymTab(mapProdComb);

}   /*  end loadProdDatatype  */


/*********************************
 *                               *
 *    loadTerminalDatatype       *
 *                               *
 *********************************/
static void
loadTerminalDatatype(void) {

  ST_ENTRY *termType;
  ST_ENTRY *termComb;

  /* add TERMINAL_TYPE data type */
  termType = st_LoadBaseDatatype(TERMINAL_TYPE, DT_INDUCTIVE, 0, 0);
  TERM_KEY = termType->key;
  
  /* add TERMINAL_TYPE combinator */
  termComb = (ST_ENTRY *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_ENTRY));
  termComb->tag = ST_OPCOMB;
  termComb->name = 
    (char *)MemHeapAlloc(symTab->scopeHD,strlen(PRIM_BANG)+1,sizeof(char));
  strcpy(termComb->name, PRIM_BANG);
  termComb->key = st_MakeKey();

  termComb->info.opcomb.numParams = 0;
  termComb->info.structor.type_sig = 
    (ST_TYPE_SIG *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE_SIG));
  termComb->info.structor.type_sig->domain = 
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  termComb->info.structor.type_sig->domain->tag = TYPE_PARAMETRIC_VAR;
  termComb->info.structor.type_sig->domain->info.param_var = 0;
  termComb->info.structor.type_sig->codomain = 
    (ST_TYPE *)MemHeapAlloc(symTab->scopeHD, 1, sizeof(ST_TYPE));
  termComb->info.structor.type_sig->codomain->tag = TYPE_1;

  addEntryToSymTab(termComb);

}


/****************************
 *                          *
 *  loadIdentityCombinator  *
 *                          *
 ****************************/
static void
loadIdentityCombinator(void) {

  ST_ENTRY *idcomb;
  ST_TYPE_SIG *type_sig;

  idcomb = (ST_ENTRY *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_ENTRY));
  idcomb->tag = ST_OPCOMB;
  idcomb->name = RES_ID;
  idcomb->key = st_MakeKey();
  idcomb->info.opcomb.numParams = 0;
  idcomb->info.opcomb.parent = NULL;

  /* id combinator has 0 params and type sig C->C */

  type_sig = (ST_TYPE_SIG *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE_SIG));
  type_sig->params = NULL;
  type_sig->domain = (ST_TYPE *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE));
  type_sig->domain->tag = TYPE_STATE_VAR;
  type_sig->codomain = (ST_TYPE *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE));
  type_sig->codomain->tag = TYPE_STATE_VAR;

  idcomb->info.opcomb.type_sig = type_sig;

  addEntryToSymTab(idcomb);
}

/****************************
 *                          *
 *  loadIdentity1Combinator *
 *                          *
 ****************************/
static void
loadIdentity1Combinator(void) {

  ST_ENTRY *idcomb;
  ST_TYPE_SIG *type_sig;

  idcomb = (ST_ENTRY *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_ENTRY));
  idcomb->tag = ST_OPCOMB;
  idcomb->name = RES_ID1;
  idcomb->key = st_MakeKey();
  idcomb->info.opcomb.numParams = 0;
  idcomb->info.opcomb.parent = NULL;

  /* id combinator has 0 params and type sig C->C */

  type_sig = (ST_TYPE_SIG *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE_SIG));
  type_sig->params = NULL;
  type_sig->domain = (ST_TYPE *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE));
  type_sig->domain->tag = TYPE_1;
  type_sig->codomain = (ST_TYPE *) MemHeapAlloc(symTab->scopeHD,1,sizeof(ST_TYPE));
  type_sig->codomain->tag = TYPE_1;

  idcomb->info.opcomb.type_sig = type_sig;

  addEntryToSymTab(idcomb);
}


/*********************************
 *                               *
 *    loadSumDatatype            *
 *                               *
 *********************************/
static void
loadSumDatatype(void) {

  ST_ENTRY *sumType,
           *constrType;

  /* add SUM_TYPE data type */
  sumType = st_LoadBaseDatatype(SUM_TYPE, DT_INDUCTIVE, 2, 2);

  /* add SUM0 constructor */
  constrType = st_LoadBaseStructor(SUM0, sumType, 0);
  constrType->info.structor.type_sig->domain->tag = TYPE_PARAMETRIC_VAR;
  constrType->info.structor.type_sig->domain->info.parametric_var = 0;
  constrType->info.structor.type_sig->codomain->tag = TYPE_STATE_VAR;

  sumType->info.datatype.structorNames[0] = SUM0;
  sumType->info.datatype.structorKeys[0] = constrType->key;

  /* add SUM1 constructor */
  constrType = st_LoadBaseStructor(SUM1, sumType, 1);
  constrType->info.structor.type_sig->domain->tag = TYPE_PARAMETRIC_VAR;
  constrType->info.structor.type_sig->domain->info.parametric_var = 1;
  constrType->info.structor.type_sig->codomain->tag = TYPE_STATE_VAR;

  sumType->info.datatype.structorNames[1] = SUM1;
  sumType->info.datatype.structorKeys[1] = constrType->key;

  /* add SUM1 combinators */
  st_AddOpCombs(sumType->key);

}   /*  end loadSumDatatype  */


/*********************************
 *                               *
 *    loadBIDatatype             *
 *                               *
 *********************************/
static void
loadBIDatatype(char *typeName, char *constrName) {

  ST_ENTRY *biType,
           *constrType;

  biType = st_LoadBaseDatatype(typeName, DT_INDUCTIVE, 0, 1);

  /* add constructor */
  constrType = st_LoadBaseStructor(constrName, biType, 0);
  constrType->info.structor.type_sig->domain->tag = TYPE_1;
  constrType->info.structor.type_sig->codomain->tag = TYPE_STATE_VAR;

  biType->info.datatype.structorNames[0]=libStrdup(symTab->scopeHD,constrName);
  biType->info.datatype.structorKeys[0] = constrType->key;

  st_AddOpCombs(biType->key);

}   /*  end loadBIDatatype  */


/**************************************************************************** 
 *                                                                          *
 *    Print and Show Function Definitions                                   *
 *                                                                          *
 ****************************************************************************/

/*****************
 *               *
 *  st_ShowType  *
 *               *
 *****************/
void st_ShowType(ST_TYPE *type) {

    st_ShowDomain(type);

}

/*********************************
 *                               *
 *    st_ShowParamRep            *
 *                               *
 *********************************/
void
st_ShowParamRep(ST_PVAR parameter) {

  char    param[2];

  param[0] = parameter+'A';
  param[1] = '\0';
  appendBuff(param);

}


/*********************************
 *                               *
 *   st_ShowDomain               *
 *                               *
 *********************************/
void
st_ShowDomain(ST_TYPE *domain) {

  int         i,
              numParams;
  char       *typeName;

  switch (domain->tag) {
    case TYPE_1:
      appendBuff("1");
      break;

    case TYPE_PROD:
      prodNestedDepth++;
      if (prodNestedDepth)  appendBuff("("); 
      st_ShowDomain(domain->info.prod.l);
      appendBuff(" ");      appendBuff(PROD_TYPE);      appendBuff(" ");
      st_ShowDomain(domain->info.prod.r); 
      if (prodNestedDepth) appendBuff(")");
      prodNestedDepth--;
      break;

    case TYPE_PARAMETRIC_VAR:
      st_ShowParamRep(domain->info.param_var);
      break;

    case TYPE_STATE_VAR:
      appendBuff(STATE_VAR_REP);
      break;

    case TYPE_USER_DATA:
      typeName = st_KeyToName(domain->info.user_data.key);
      if (strcmp(typeName, SUM_TYPE)==0) {
	sumNestedDepth++;

	/* [H-O] ALTERED CONDITIONAL TO FIX A +/* PRECEDENCE BUG: */

	if ((sumNestedDepth > 0) || (prodNestedDepth >= 0))  appendBuff("("); 
	st_ShowDomain(domain->info.user_data.args[0]);
	appendBuff(" ");      appendBuff(SUM_TYPE_INFIX);      appendBuff(" ");
	st_ShowDomain(domain->info.user_data.args[1]); 

	/* [H-O] ALTERED CONDITIONAL TO FIX A +/* PRECEDENCE BUG: */

	if ((sumNestedDepth > 0) || (prodNestedDepth >= 0)) appendBuff(")");
	sumNestedDepth--;
      }   /*  fi  */
      else {
	appendBuff(typeName);
	numParams = st_GetNumParams(domain->info.user_data.key);
	if (numParams > 0) {
	    appendBuff("(");
	    for (i = 0; i < numParams; i++) {
		 st_ShowDomain(domain->info.user_data.args[i]);
		 if (i < numParams-1) appendBuff(", ");
	    } /* rof */
	    appendBuff(")");
        } /* fi */
      }   /*  esle  */
      break;

    case TYPE_BUILTIN_INT:      /* [BI] PRINT THE BUILTIN int  TYPE */
      appendBuff ("int");
      break;

    case TYPE_BUILTIN_CHAR:     /* [BI] PRINT THE BUILTIN char TYPE */
      appendBuff ("char");
      break;

    default:
       printMsg(FATAL_MSG, "st_ShowDomain - %d is not a valid tag", domain->tag);
   }

}   /*  end showDomain  */


/*********************************
 *                               *
 *   st_ShowStructDomain         *
 *                               *
 *********************************/
void
st_ShowStructDomain(ST_TYPE *domain, ST_TYPE *parentStateSig) {
/* this is a kludge. The symbol table should be changed to reflect
   the different type of a structor. But this will affect the type
   checking module. */
  int         i,
              numParams;
  char       *typeName;

  switch (domain->tag) {
    case TYPE_1:
      appendBuff("1");
      break;

    case TYPE_BUILTIN_CHAR:     /* [BI] PRINT THE BUILTIN char TYPE */
      appendBuff ("char");
      break;

    case TYPE_BUILTIN_INT:      /* [BI] PRINT THE BUILTIN int  TYPE */
      appendBuff ("int");
      break;

    case TYPE_PROD:
      prodNestedDepth++;
      if (prodNestedDepth)  appendBuff("("); 
      st_ShowStructDomain(domain->info.prod.l, parentStateSig);
      appendBuff(" ");      appendBuff(PROD_TYPE);      appendBuff(" ");
      st_ShowStructDomain(domain->info.prod.r, parentStateSig); 
      if (prodNestedDepth) appendBuff(")");
      prodNestedDepth--;
      break;

    case TYPE_PARAMETRIC_VAR:
      st_ShowParamRep(domain->info.param_var);
      break;

    case TYPE_STATE_VAR:
      st_ShowStructDomain(parentStateSig, parentStateSig);
      break;
    case TYPE_USER_DATA:
      typeName = st_KeyToName(domain->info.user_data.key);
      if (strcmp(typeName, SUM_TYPE)==0) {
	sumNestedDepth++;
	if (sumNestedDepth)  appendBuff("("); 
	st_ShowStructDomain(domain->info.user_data.args[0], parentStateSig);
	appendBuff(" ");      appendBuff(SUM_TYPE_INFIX);      appendBuff(" ");
	st_ShowStructDomain(domain->info.user_data.args[1], parentStateSig); 
	if (sumNestedDepth) appendBuff(")");
	sumNestedDepth--;
      }   /*  fi  */
      else {
	appendBuff(typeName);
	numParams = st_GetNumParams(domain->info.user_data.key);
	if (numParams > 0) {
	    appendBuff("(");
	    for (i = 0; i < numParams; i++) {
	      st_ShowStructDomain(domain->info.user_data.args[i],
				  parentStateSig);
	      if (i < numParams-1) appendBuff(", ");
	    } /* rof */
	    appendBuff(")");
        } /* fi */
      }   /*  esle  */
      break;

    default:
       printMsg(FATAL_MSG, "st_ShowStructDomain - %d is not a valid tag", domain->tag);
   }

}   /*  end showStructDomain  */


void st_ShowDomainLessContext(ST_TYPE *st_type) {
/* assuming st_type is a product with context to the right */

  if (st_type->tag == TYPE_PROD) {
    st_ShowDomain(st_type->info.prod.l);
  }
}


/*********************************
 *                               *
 *    st_ShowTypeParams          *
 *                               *
 *********************************/
void
st_ShowTypeParams(ST_TYPE_SIG **params) {

  int i=0;

  if (params) {
    appendBuff(" {");
    while (params[i]) {
      st_ShowSig(params[i++]);
      if (params[i])   appendBuff(",");
      else             appendBuff("}");
    }
  }
}


void st_ShowTypeParamsLessContext(ST_TYPE_SIG **params) {

  int i=0;

  if (params) {
    appendBuff(" {");
    while (params[i]) {
      st_ShowSigLessContext(params[i++]);
      if (params[i])   appendBuff(",");
      else             appendBuff("}");
    }
  }
}


/*********************************
 *                               *
 *    st_ShowTypeSigByKey        *
 *                               *
 *********************************/
void
st_ShowTypeSigByKey(ST_KEY key) {

  st_ShowTypeSig(st_GetTypeSig(key));

}


/*********************************
 *                               *
 *    st_ShowTypeSig             *
 *                               *
 *********************************/
void
st_ShowTypeSig(ST_TYPE_SIG *typeSig) {
  if (typeSig)
    {
      if (typeSig->params) st_ShowTypeParams(typeSig->params);
      appendBuff(" : ");

      st_ShowDomain(typeSig->domain);
      appendBuff(" -> ");
      st_ShowDomain(typeSig->codomain);
    }

}


void st_ShowTypeSigLessContext(ST_TYPE_SIG *sig) {

  if (sig->params) st_ShowTypeParamsLessContext(sig->params);
  appendBuff(" : ");

  if (sig->params) st_ShowDomainLessContext(sig->domain);
  else st_ShowDomain(sig->domain);
  appendBuff(" -> ");
  st_ShowDomain(sig->codomain);

}


/*********************************
 *                               *
 *    st_ShowSig                 *
 *                               *
 *********************************/
void
st_ShowSig(ST_TYPE_SIG *typeSig) {

  st_ShowDomain(typeSig->domain);
  appendBuff(" -> ");
  st_ShowDomain(typeSig->codomain);

}


void st_ShowSigLessContext(ST_TYPE_SIG *sig) {

  st_ShowDomainLessContext(sig->domain);
  appendBuff(" -> ");
  st_ShowDomain(sig->codomain);

}


/********************************
 *                              *
 * st_PrintEntryInfo            *
 *                              *
 ********************************/

void
st_PrintEntryInfo (ST_KEY key)
{
  ST_ENTRY *entry = st_GetEntry (key, NULL);

  if (entry)
    switch (entry->tag)
      {
      case ST_FUNCTION:
	st_PrintFunction (key);
	break;

      case ST_MACRO:
	st_PrintMacro (key);
	break;

      case ST_STRUCTOR:
	st_PrintStructor (key);
	break;

      case ST_DATATYPE:
	st_PrintDatatype (key);
	break;

      case ST_ALIAS:
	st_PrintAlias (entry);
	break;

      case ST_OPCOMB:
	st_PrintCombinator (key);
	break;

      case ST_VAR:
          st_PrintVar(key);
          break;

      default:
	printMsg (FATAL_MSG,
		  "st_PrintEntryInfo() - invalid tag %d",
		  entry->tag);
      }
  else
    printMsg (ERROR_MSG, "Name not in symbol table");
}


/*********************************
 *                               *
 *    st_ShowCombinator          *
 *                               *
 *********************************/
void
st_ShowCombinator(ST_KEY combKey) {
/* Format for a combinator is as follows:
    COMB $$_case_list : { 1 * 'x -> 'b,
                          ('a) * list('a) -> 'b } : list('a) * 'x -> 'b
*/
  char *combName = st_KeyToName(combKey);
  int resPrefixLen = strlen(RES_PREFIX);

  if (strncmp(RES_PREFIX, combName, resPrefixLen) == 0)
    combName = combName + resPrefixLen;
  appendBuff("COMB ");
  appendBuff(combName);   appendBuff(" : ");
  st_ShowTypeSigByKey(combKey);             appendBuff("\n");

}


/*********************************
 *                               *
 *    st_PrintCombinator         *
 *                               *
 *********************************/
void
st_PrintCombinator(ST_KEY combKey) {

  clearBuff();  st_ShowCombinator(combKey);  outputBuff(stdout);

}


/*********************************
 *                               *
 *    st_ShowVar                 *
 *                               *
 *********************************/
static void
st_ShowVar(ST_KEY varKey) {

    ST_ENTRY   *entry = st_GetEntry(varKey, NULL);

    appendBuff("VAR "); appendBuff(entry->name);
    appendBuff(" | ");   appendBuff(entry->info.var.uniqueVar);
    appendBuff("\n");

}


/*********************************
 *                               *
 *    st_PrintVar                *
 *                               *
 *********************************/
static void
st_PrintVar(ST_KEY varKey) {

  clearBuff();  st_ShowVar(varKey);  outputBuff(stdout);

}


/*********************************
 *                               *
 *    st_ShowStructor            *
 *                               *
 *********************************/
void
st_ShowStructor(ST_KEY structKey) {

  ST_TYPE_SIG *typeSig        = st_GetTypeSig(structKey);
  ST_TYPE     *parentStateSig = st_GetGenericStateType(structKey);

  if (st_IsConstructor(structKey)) appendBuff("CONSTR ");
  else                             appendBuff("DESTR ");
  appendBuff(st_KeyToName(structKey));

  if (typeSig)
    {
      appendBuff(" : ");
      st_ShowStructDomain(typeSig->domain, parentStateSig);
      appendBuff(" -> ");
      st_ShowStructDomain(typeSig->codomain, parentStateSig);
    }

  appendBuff("\n");
}


/********************************
 *                              *
 *     st_PrintStructor         *
 *                              *
 ********************************/
void
st_PrintStructor(ST_KEY structKey) {

clearBuff();  st_ShowStructor(structKey);  outputBuff(stdout);

}


/********************************
 *                              *
 *     st_ShowAlias             *
 *                              *
 ********************************/

static
void
st_ShowAlias (ST_ENTRY *entry)
{
  int i = 0;

  assert (entry);
  assert (entry->tag == ST_ALIAS);
  assert (entry->name);

  appendBuff ("TYPE ALIAS ");
  appendBuff (entry->name);

  if (entry->info.alias.numParams)
    {
      appendBuff ("(");

      for (i = 0; i < entry->info.alias.numParams; i++)
	{
	  st_ShowParamRep (i);

	  if (i < entry->info.alias.numParams - 1)
	    appendBuff (", ");
	}

      appendBuff (")");
    }

  appendBuff (" = ");
  st_ShowType (entry->info.alias.expansion);
  appendBuff ("\n");
}


/********************************
 *                              *
 *     st_ShowDatatype          *
 *                              *
 ********************************/
/* loads print buffer with a string formatted like:
    TYPE list('A) = 
           nil : 1 -> list('A)
	   cons : 'A x list('A)  -> list('A)
*/
void
st_ShowDatatype(ST_KEY typeKey) {

  ST_KEY   *structorKeys;
  char     *typeName = st_KeyToName(typeKey);
  int i,
      params,
      structors;

  params = st_GetNumParams(typeKey);

  appendBuff("TYPE "); 

  if (strcmp(typeName, PROD_TYPE)==0) {
    appendBuff(STATE_VAR_REP " -> " );
    st_ShowParamRep(0); 
    appendBuff(" " PROD_TYPE " ");
    st_ShowParamRep(1);
    appendBuff(" =\n");
  }
/*  else if (strcmp(typeName, SUM_TYPE)==0) {
    st_ShowParamRep(0); 
    appendBuff(" " SUM_TYPE_INFIX " ");
    st_ShowParamRep(1);
    appendBuff(" -> " STATE_VAR_REP " =\n");
  }
*/  else {
    if (st_IsCoinductiveType(typeKey))
      appendBuff(STATE_VAR_REP " -> " );

    appendBuff(typeName); 
    if (params>0)     appendBuff("(");
    for (i=0; i<params; i++) {
      st_ShowParamRep(i);
      if (i == params-1) appendBuff(")");
      else            appendBuff(", ");
    }

    if (st_IsInductiveType(typeKey))
      appendBuff(" -> " STATE_VAR_REP " =\n");
    else
      appendBuff(" =\n");
  }   /*  esle  */

  structors = st_GetNumStructors(typeKey);
  structorKeys = st_GetStructorKeys(typeKey);
  for (i=0; i<structors; i++) {
    appendBuff("       ");
    appendBuff(st_KeyToName(structorKeys[i]));
    st_ShowTypeSigByKey(structorKeys[i]);
    appendBuff("\n");
  }

}  /*  end showDataType  */


/********************************
 *                              *
 *    st_PrintDatatype          *
 *                              *
 ********************************/
void
st_PrintDatatype(ST_KEY typeKey) {

clearBuff();  st_ShowDatatype(typeKey);  outputBuff(stdout);

}


/********************************
 *                              *
 *    st_PrintAlias             *
 *                              *
 ********************************/

static
void
st_PrintAlias (ST_ENTRY *entry)
{
  clearBuff ();
  st_ShowAlias (entry);
  outputBuff (stdout);
}


/********************************
 *                              *
 *    st_ShowFunction           *
 *                              *
 ********************************/
/* loads print buffer with a string formatted like:
   foo {a : 'A -> 'B, b : 'C -> bool} : 'D -> 'E
*/
void
st_ShowFunction(ST_KEY funKey) {

  printMsg(MSG,"FUNCTION %s%U",st_KeyToName(funKey),st_GetTypeSig(funKey));
   /* "%U" means to show type sigs without context vars */
}  /* end showFunctionType */


/********************************
 *                              *
 *    st_PrintFunction          *
 *                              *
 ********************************/
void
st_PrintFunction(ST_KEY funKey) {

clearBuff();  st_ShowFunction(funKey);  outputBuff(stdout);

}


/********************************
 *                              *
 *    st_ShowMacro              *
 *                              *
 ********************************/
/* loads print buffer with a string formatted like:
   foo : 'D -> 'E
*/
void
st_ShowMacro(ST_KEY macroKey) {

  char    *macroName = st_KeyToName(macroKey);

  appendBuff(macroName);    appendBuff(" : ");
  st_ShowTypeSigByKey(macroKey);  appendBuff("\n");

}  /* end st_ShowMacro */


/********************************
 *                              *
 *    st_PrintMacro             *
 *                              *
 ********************************/
void
st_PrintMacro(ST_KEY macroKey) {

clearBuff();  st_ShowMacro(macroKey);  outputBuff(stdout);

}


/*********************************
 *                               *
 *    st_KeysEqual               *
 *                               *
 *********************************/

BBOOL
st_KeysEqual (ST_KEY k1, ST_KEY k2)
{
  if ((unsigned long int)k1 == (unsigned long int)k2)
    return BTRUE;
  else
    return BFALSE;
}


/*********************************
 *                               *
 *    st_ParamsEqual             *
 *                               *
 *********************************/
BBOOL   
st_ParamsEqual(ST_PVAR p1, ST_PVAR p2) {

  if ((int)p1 == (int)p2)
    return BTRUE;
  else
    return BFALSE;

}


