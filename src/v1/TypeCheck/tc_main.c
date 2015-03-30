/******************************************************************************
 *                                                                            *
 *   tc_main.c                                                                *
 *                                                                            *
 *   COPYRIGHT (c) 1997 by Charity Development Group.   All rights reserved.  *
 *                                                                            *
 *   contact: charity@cpsc.ucalgary.ca                                        *
 *                                                                            *
 *****************************************************************************/

#include "typecheckI.h"
#include <string.h>

MEMORY tc_memory;                                       /* all local memory usage */
TC_TYPE_VARIABLE next_new_var;                          /* for renaming type vars */
TYPE_ASMT_LIST *SubstList;

static int typechecking_a_def;                          /* 1 for def, 0 otherwise */
static ST_KEY _fnkey;

static TC_TYPE_CONSTRUCTOR product_con;
static TYPE_EXPR *terminal_type, *int_type, *char_type;
static TC_TYPE_CONSTRUCTOR next_AT_con;                 /* for @ type constructors */

static PE_PATT *convert_PE_VAR_BASE_to_PE_PATT(PE_VAR_BASE *vb);
static TYPED_PATT *typed_patt(PE_PATT *patt, TYPE_EXPR **type);
static void typecheck_PE_LIST_T_PHRASE(TYPED_PATT_LIST *context, PE_LIST_T_PHRASE *phrase_list, TYPE_EXPR *domain, TYPE_EXPR *codomain);
static TYPED_PATT_LIST *add_patt_to_context(TYPED_PATT_LIST *context, PE_PATT *patt, TYPE_EXPR **patt_type);
static void typecheck_structor(char *name, TYPE_EXPR *domain, TYPE_EXPR *codomain);
static void typecheck_PE_FUNCTION(TYPED_PATT_LIST *context, PE_FUNCTION *fn, TYPE_EXPR *domain, TYPE_EXPR *codomain);
static void typecheck_PE_MAP(TYPED_PATT_LIST *context, PE_MAP *map, TYPE_EXPR *domain, TYPE_EXPR *codomain);
static void typecheck_fold(TYPED_PATT_LIST *context, PE_FOLD **phrases, TYPE_EXPR *domain, TYPE_EXPR *codomain);
static void typecheck_unfold(TYPED_PATT_LIST *context, PE_UNFOLD **phrases, TYPE_EXPR *domain, TYPE_EXPR *codomain);
static void typecheck_record(TYPED_PATT_LIST *context, PE_RECORD **phrases, TYPE_EXPR *domain, TYPE_EXPR *codomain);
static void typecheck_PE_TERM(TYPED_PATT_LIST *context, PE_TERM *term, TYPE_EXPR *domain, TYPE_EXPR *codomain);
static TYPE_EXPR **find_var_in_context(TYPED_PATT_LIST *context, char *var);
static void typecheck_PE_EXPR(TYPED_PATT_LIST *context, PE_EXPR *expr, TYPE_EXPR *type);
static TYPE_EXPR *type_var_expr(TC_TYPE_VARIABLE var);
static TYPE_EXPR *type_con_expr(TC_TYPE_CONSTRUCTOR con, TYPE_EXPR **params);
static TYPE_EXPR *product_type_expr(TYPE_EXPR *left, TYPE_EXPR *right);

#define DEBUG 0
#if DEBUG
#include "tc_debug.c"
#endif

/*************************/
/*                       */
/*  tc_open_typechecker  */
/*                       */
/*************************/
void tc_open_typechecker(void)
/*
 *  call this BEFORE calling tc_typecheck_PE_EXPR or tc_typecheck_PE_DEF
 */
{
  tc_memory = MemAlloc("typecheck",50000,1);    /* enough for now? */
  next_new_var = 0;
  typechecking_a_def = 0;
  SubstList = 0;

  product_con = st_NameToKey(PROD_TYPE);
  terminal_type = type_con_expr(st_NameToKey(TERMINAL_TYPE), 0);
  int_type = type_con_expr(st_NameToKey(INT_TYPENAME), 0);
  char_type = type_con_expr(st_NameToKey(CHAR_TYPENAME), 0);
  next_AT_con = -1;
}

/**************************/
/*                        */
/*  tc_close_typechecker  */
/*                        */
/**************************/
void tc_close_typechecker(int kill_a_def)
/*
 *  call this AFTER the ST_TYPE returned by tc_typecheck_PE_EXPR is no longer needed
 *   OR immediately prior to a non-fatal error message bail out
 */
{
  if ( typechecking_a_def || kill_a_def )
      st_RemoveEntry(_fnkey);
  /* a def is only wiped out if close is called DURING typechecking */

  MemDealloc(tc_memory);
}

/**************************/
/*                        */
/*  tc_typecheck_PE_EXPR  */
/*                        */
/**************************/
ST_TYPE *tc_typecheck_PE_EXPR(PE_EXPR *expr)
{
  TYPE_EXPR *type;

  type = type_var_expr(new_type_var());
  typecheck_PE_EXPR(0, expr, type);

  /* if this point reached then typechecking was successful */
  /* to get the result type, apply SubstList to type */
  type = subst_all_in_TE(SubstList, type);

  /* normalize type vars */
  next_new_var = 0;
  type = rename_vars_in_TE(assign_new_vars(collect_vars_in_TE(type)), type);

  return convert_TE_to_ST_TYPE(type);
}

/*************************/
/*                       */
/*  tc_typecheck_PE_DEF  */
/*                       */
/*************************/
void tc_typecheck_PE_DEF(PE_DEF *def, ST_KEY fnkey)
{
  int num_macros, i, j;
  ST_KEY *macro_keys;
  TYPE_EXPR **types, **ptype;
  ST_TYPE_SIG *st_type_sig;
  TYPED_PATT_LIST *context;
  PE_LIST_MACRO *pe_macros;
  PE_MACRO *pe_macro;
  PE_PATT *patt;

  _fnkey = fnkey;       /* make it global (used in tc_close_typechecker) */
  typechecking_a_def = 1;
  num_macros = st_GetNumMacros(fnkey);
  macro_keys = st_GetMacroKeys(fnkey);
  pe_macros = def->macros;
  if (MacroListLen(pe_macros) != num_macros) {
    printMsg(FATAL_MSG, "typecheck_PE_DEF: macro list length inequality");
  }

  /* get existing type sigs from st (user may have given some) */
  /* also initialize context with all macros as function variables */

  types = (TYPE_EXPR **) MHA(tc_memory, (num_macros*2)+3, sizeof(TYPE_EXPR *));
  st_type_sig = st_GetTypeSig(fnkey);
  types[0] = convert_ST_TYPE_to_TE(st_type_sig->domain);
  types[1] = convert_ST_TYPE_to_TE(st_type_sig->codomain);
  if (st_type_sig->userspecified) {
    types[0] = type_vars_to_user_vars(types[0]);
    types[1] = type_vars_to_user_vars(types[1]);
  }

  context = 0;
  for (i=0; i<num_macros; i++) {
    j=i*2;
    st_type_sig = st_GetTypeSig(macro_keys[i]);
    types[j+2] = convert_ST_TYPE_to_TE(st_type_sig->domain);
    types[j+3] = convert_ST_TYPE_to_TE(st_type_sig->codomain);
    if (st_type_sig->userspecified) {
      types[j+2] = type_vars_to_user_vars(types[j+2]);
      types[j+3] = type_vars_to_user_vars(types[j+3]);
    }
    pe_macro = MacroListHead(pe_macros);
    patt = (PE_PATT *) MHA(tc_memory, 1, sizeof(PE_PATT));
    patt->tag = P_HOVAR;
    patt->info.hovar.hovar = pe_macro->ident;
    ptype = (TYPE_EXPR **) MHA(tc_memory, 3, sizeof(TYPE_EXPR *));
    ptype[0] = types[j+2];
    ptype[1] = types[j+3];
    ptype[2] = 0;
    context = add_patt_to_context(context, patt, ptype);
    pe_macros = MacroListTail(pe_macros);
  }
  types[(num_macros*2)+2] = 0;

  /* normalize type vars to avoid clashes with new type vars later */
  next_new_var = 0;
  types = rename_vars_in_TEL(assign_new_vars(collect_vars_in_TEL(types)), types);

  /* add def->var_base to context */
  ptype = (TYPE_EXPR **) MHA(tc_memory, 2, sizeof(TYPE_EXPR *));
  ptype[0] = types[0];
  ptype[1] = 0;
  context = add_patt_to_context(context,convert_PE_VAR_BASE_to_PE_PATT(def->var_base),ptype);

  /* typecheck def->expr in context */
  typecheck_PE_EXPR(context, def->expr, types[1]);

  /* typecheck successful if this point reached */

  /* instantiate all function and macro type sigs */
  types = subst_all_in_TEL(SubstList, types);

  /* normalize type vars in the result types */
  next_new_var = 0;
  types = rename_vars_in_TEL(assign_new_vars(collect_vars_in_TEL(types)), types);

  /* finally, copy result types to the st */
  st_type_sig = (ST_TYPE_SIG *) MHA(tc_memory, 1, sizeof(ST_TYPE_SIG));

  /* 1. copy the macro type sigs to st */
  st_type_sig->params = 0;
  for (i=0; i<num_macros; i++) {
    j=i*2;
    st_type_sig->domain = convert_TE_to_ST_TYPE(types[j+2]);
    st_type_sig->codomain = convert_TE_to_ST_TYPE(types[j+3]);
    st_UpdateTypeSig(macro_keys[i], st_type_sig);
  }
  st_LinkMacroTypeSigsToFunction(fnkey);

  /* 2. copy the fn type sig to st */
  st_type_sig->domain = convert_TE_to_ST_TYPE(types[0]);
  st_type_sig->codomain = convert_TE_to_ST_TYPE(types[1]);
  st_type_sig->params = st_GetMacroTypeSigs(fnkey);
  st_UpdateTypeSig(fnkey, st_type_sig);

  typechecking_a_def = 0;
}

/************************************/
/*                                  */
/*  convert_PE_VAR_BASE_to_PE_PATT  */
/*                                  */
/************************************/
static PE_PATT *convert_PE_VAR_BASE_to_PE_PATT(PE_VAR_BASE *vb)
{
  PE_PATT *patt;

  if (!vb) return 0;
  patt = (PE_PATT *) MHA(tc_memory, 1, sizeof(PE_PATT));
  switch (vb->tag)
  {
    case VB_BANG:
      patt->tag = P_BANG;
      break;
    case VB_VAR:
      patt->tag = P_VAR;
      patt->info.var = vb->info.var;
      break;
    case VB_PAIR:
      patt->tag = P_PAIR;
      patt->info.ppair.l = convert_PE_VAR_BASE_to_PE_PATT(vb->info.vbpair.l);
      patt->info.ppair.r = convert_PE_VAR_BASE_to_PE_PATT(vb->info.vbpair.r);
      break;
    default:
      printMsg(FATAL_MSG,"(typecheck) convert_PE_VAR_BASE_to_PE_PATT: unknown tag");
  }
  return patt;
}

/****************/
/*              */
/*  typed_patt  */
/*              */
/****************/
static TYPED_PATT *typed_patt(PE_PATT *patt, TYPE_EXPR **type)
{
  TYPED_PATT *tp;
  tp = (TYPED_PATT *) MHA(tc_memory, 1, sizeof(TYPED_PATT));
  tp->patt = patt;
  tp->type = type;
  return tp;
}

/********************************/
/*                              */
/*  typecheck_PE_LIST_T_PHRASE  */
/*                              */
/********************************/
static void typecheck_PE_LIST_T_PHRASE(TYPED_PATT_LIST *context, PE_LIST_T_PHRASE *phrase_list, TYPE_EXPR *domain, TYPE_EXPR *codomain)
{
  PE_T_PHRASE *phrase;
  TYPE_EXPR **patt_type;

  while (phrase_list) {
    phrase = (PE_T_PHRASE *) phrase_list->item;
    if (phrase->expr) {                 /* first order */
      patt_type = (TYPE_EXPR **) MHA(tc_memory, 2, sizeof(TYPE_EXPR *));
      patt_type[0] = domain;
      patt_type[1] = 0;
      typecheck_PE_EXPR(add_patt_to_context(context,phrase->patt,patt_type), phrase->expr, codomain);
    }
    else if (phrase->cases) {           /* higher order */
      if (domain && (domain->tag == TYPE_CON) && (domain->id.con == product_con)) {
        patt_type = (TYPE_EXPR **) MHA(tc_memory, 2, sizeof(TYPE_EXPR *));
        patt_type[0] = domain->params[1];
        patt_type[1] = 0;
        typecheck_PE_TERM(add_patt_to_context(context,phrase->patt,patt_type), phrase->cases, domain->params[0], codomain);
      }
      else printMsg(FATAL_MSG,"typecheck_PE_LIST_T_PHRASE: failed on h.o. phrase");
    }
    else printMsg(FATAL_MSG,"typecheck_PE_LIST_T_PHRASE: empty phrase");
    phrase_list = phrase_list->next;
  }
}

/*************************/
/*                       */
/*  add_patt_to_context  */
/*                       */
/*************************/
TYPED_PATT_LIST *add_patt_to_context(TYPED_PATT_LIST *context, PE_PATT *patt, TYPE_EXPR **patt_type)
/*
 *  given a pattern (patt) and its type (patt_type), decompose the pattern and
 *   add any variables it contains to the context while also updating SubstList
 *  return the extended context
 */
{
  TYPE_EXPR **types;
  P_STRUCTOR **structors;
  ST_KEY structor_key;
  ST_TYPE_SIG *st_type_sig;
  int num_destrs, i;
  TYPE_EXPR *parent_type;
  TYPE_ASMT_LIST *alist;
  TYPE_ASMT *asmt;

  if (!patt) printMsg(FATAL_MSG,"add_patt_to_context: patt is NULL");
  if (patt->tag == P_HOVAR) {
    if (TEL_length(patt_type) != 2) {
      printMsg(FATAL_MSG,"add_patt_to_context: illegal type for higher-order pattern");
    }
  }
  else {
    if (TEL_length(patt_type) != 1) {
      printMsg(FATAL_MSG,"add_patt_to_context: illegal type for first-order pattern");
    }
  }
  switch (patt->tag) {
    case P_VAR:
    case P_HOVAR:
      context = TPL_cons(typed_patt(patt, patt_type), context);
      break;

    case P_RECORD:
      structors = patt->info.record;  /* should be nt array with > 0 entries */
      if (!structors  ||  !(structors[0])) {
        printMsg(FATAL_MSG,"add_patt_to_context: illegal record pattern");
      }
      structor_key = st_NameToKey(structors[0]->id);
      num_destrs = st_GetNumStructors(st_GetStructorParent(structor_key));
      parent_type = convert_ST_TYPE_to_TE(st_GetGenericStateType(structor_key));
      alist = assign_new_vars(collect_vars_in_TE(parent_type));
      asmt = type_asmt(-1, parent_type);

      for (i=0; i<num_destrs; i++) {
        structor_key = st_NameToKey(structors[i]->id);
        st_type_sig = st_GetTypeSig(structor_key);
        if (st_IsHO(structor_key)) {
          types = (TYPE_EXPR **) MHA(tc_memory, 3, sizeof(TYPE_EXPR *));
          types[0] = convert_ST_TYPE_to_TE(st_type_sig->domain);    /* Ei(A)*C */
          if (types[0]->tag == TYPE_CON  &&  types[0]->id.con == product_con) {
            types[0] = types[0]->params[0];           /* Ei(A) */
          }
          else printMsg(FATAL_MSG,"add_patt_to_context: failed on h.o. destructor pattern");
          types[1] = convert_ST_TYPE_to_TE(st_type_sig->codomain);  /* Fi(A,C) */
          types[2] = 0;
        }
        else {
          types = (TYPE_EXPR **) MHA(tc_memory, 2, sizeof(TYPE_EXPR *));
          types[0] = convert_ST_TYPE_to_TE(st_type_sig->codomain);  /* Fi(A,C) */
          types[1] = 0;
        }
        types = subst_in_TEL(asmt, types);          /* C:=R(A) */
        types = rename_vars_in_TEL(alist, types);   /* rename A's */

        context = add_patt_to_context(context, structors[i]->arg, types);
      }
      parent_type = rename_vars_in_TE(alist, parent_type);  /* rename A's */
      add_equation_to_SubstList(parent_type, patt_type[0]);
      break;

    case P_PAIR:
      types = (TYPE_EXPR **) MHA(tc_memory, 4, sizeof(TYPE_EXPR *));
      types[0] = type_var_expr(new_type_var());
      types[1] = 0;
      types[2] = type_var_expr(new_type_var());
      types[3] = 0;

      context = add_patt_to_context(context, patt->info.ppair.l, types);
      context = add_patt_to_context(context, patt->info.ppair.r, types+2);
      add_equation_to_SubstList(product_type_expr(types[0], types[2]), patt_type[0]);
      break;

    case P_CONSTR:
      structor_key = st_NameToKey(patt->info.constr->id);
      parent_type = convert_ST_TYPE_to_TE(st_GetGenericStateType(structor_key));
      alist = assign_new_vars(collect_vars_in_TE(parent_type));

      st_type_sig = st_GetTypeSig(structor_key);
      types = (TYPE_EXPR **) MHA(tc_memory, 2, sizeof(TYPE_EXPR *));
      types[0] = convert_ST_TYPE_to_TE(st_type_sig->domain);          /* Ei(A,C) */
      types[0] = subst_in_TE(type_asmt(-1,parent_type), types[0]);    /* C:=R(A) */
      types[0] = rename_vars_in_TE(alist, types[0]);                  /* rename A's */
      types[1] = 0;

      context = add_patt_to_context(context, patt->info.constr->arg, types);
      parent_type = rename_vars_in_TE(alist, parent_type);            /* rename A's */
      add_equation_to_SubstList(parent_type, patt_type[0]);
      break;

    case P_BANG:
      add_equation_to_SubstList(terminal_type, patt_type[0]);
      break;
    case P_INT:
      add_equation_to_SubstList(int_type, patt_type[0]);
      break;
    case P_CHAR:
      add_equation_to_SubstList(char_type, patt_type[0]);
      break;
    default:
      printMsg(FATAL_MSG,"add_patt_to_context: unknown tag");
  }
  return context;
}

/************************/
/*                      */
/*  typecheck_structor  */
/*                      */
/************************/
static void typecheck_structor(char *name, TYPE_EXPR *domain, TYPE_EXPR *codomain)
{
  TYPE_EXPR **types;
  TYPE_EXPR *parent;
  ST_KEY st_key;
  ST_TYPE_SIG *st_type_sig;

  st_key = st_NameToKey(name);
  if (st_key == 0) printMsg(FATAL_MSG,"typecheck_structor: unknown structor \"%s\"", name);
  else {
    st_type_sig = st_GetTypeSig(st_key);
    types = (TYPE_EXPR **) MHA(tc_memory, 3, sizeof(TYPE_EXPR *));
    types[0] = convert_ST_TYPE_to_TE(st_type_sig->domain);
    types[1] = convert_ST_TYPE_to_TE(st_type_sig->codomain);
    types[2] = 0;

    parent = convert_ST_TYPE_to_TE(st_GetGenericStateType(st_key));     /* L(A) or R(A) */
    /* replace C (-1) by parent */
    types = subst_in_TEL(type_asmt(-1, parent), types);
    /* rename the A's */
    types = rename_vars_in_TEL(assign_new_vars(collect_vars_in_TE(parent)), types);

    add_equation_to_SubstList(domain, types[0]);
    add_equation_to_SubstList(types[1], codomain);
  }
}

/***************************/
/*                         */
/*  typecheck_PE_FUNCTION  */
/*                         */
/***************************/
static void typecheck_PE_FUNCTION(TYPED_PATT_LIST *context, PE_FUNCTION *fn, TYPE_EXPR *domain, TYPE_EXPR *codomain)
{
  TYPE_EXPR **types;
  ST_KEY st_key;
  ST_TYPE_SIG *st_type_sig;
  int num_macros, i, j;
  PE_LIST_T_PHRASE **params;

  types = find_var_in_context(context, fn->fun_name);    /* look in context first */
  if (types) {
    /* must be a higher-order variable */
    if (TEL_length(types) != 2) printMsg(FATAL_MSG,"typecheck_PE_FUNCTION: illegal type for h.o. variable");
    add_equation_to_SubstList(domain, types[0]);
    add_equation_to_SubstList(types[1], codomain);
  }
  else {
    /* must be a previously defined function */
    st_key = st_NameToKey(fn->fun_name);
    if (st_key == 0) printMsg(FATAL_MSG,"typecheck_PE_FUNCTION: unknown function \"%s\"", fn->fun_name);
    st_type_sig = st_GetTypeSig(st_key);
    num_macros = st_GetNumMacros(st_key);

    /* get function and macro types from st and rename all type vars */
    types = (TYPE_EXPR **) MHA(tc_memory, (num_macros*2)+3, sizeof(TYPE_EXPR *));
    types[0] = convert_ST_TYPE_to_TE(st_type_sig->domain);
    types[1] = convert_ST_TYPE_to_TE(st_type_sig->codomain);
    for (i=0; i<num_macros; i++) {
      j=(i*2);
      types[j+2] = convert_ST_TYPE_to_TE(st_type_sig->params[i]->domain);
      types[j+3] = convert_ST_TYPE_to_TE(st_type_sig->params[i]->codomain);
    }
    types[(num_macros*2)+2] = 0;
    types = rename_vars_in_TEL(assign_new_vars(collect_vars_in_TEL(types)), types);

    /* typecheck all the macro args passed to the function */
    params = fn->macros;
    for (i=0; i<num_macros; i++) {
      j=i*2;
      if (params[i]) {
        typecheck_PE_LIST_T_PHRASE(context, params[i], types[j+2], types[j+3]);
      }
      else {
        tc_close_typechecker(0);
        printMsg(ERROR_MSG, "too few macro arguments to function %s", fn->fun_name);
      }
    }
    if (params  &&  (params[i])) {
      tc_close_typechecker(0);
      printMsg(ERROR_MSG, "too many macro arguments to function %s", fn->fun_name);
    }

    add_equation_to_SubstList(domain, types[0]);
    add_equation_to_SubstList(types[1], codomain);
  }
}

/**********************/
/*                    */
/*  typecheck_PE_MAP  */
/*                    */
/**********************/
static void typecheck_PE_MAP(TYPED_PATT_LIST *context, PE_MAP *map, TYPE_EXPR *domain, TYPE_EXPR *codomain)
{
  ST_KEY st_key;
  int numParams, i;
  TYPE_EXPR **A_types, **B_types;
  PE_MAP_PHRASE *map_phrase;

  st_key = st_NameToKey(map->type_name);
  numParams = st_GetNumParams(st_key);
  A_types = (TYPE_EXPR **) MHA(tc_memory, numParams+1, sizeof(TYPE_EXPR *));
  B_types = (TYPE_EXPR **) MHA(tc_memory, numParams+1, sizeof(TYPE_EXPR *));
  for (i=0; i<numParams; i++) {
    A_types[i] = type_var_expr(new_type_var());         /* Ai */
    B_types[i] = type_var_expr(new_type_var());         /* Bi */

    map_phrase = (map->phrases)+i;
    if (map_phrase->positive) {
      if (map_phrase->negative) {
        /* bivariant (*) phrase (p:Ai->Bi & n:Bi->Ai) */
        typecheck_PE_LIST_T_PHRASE(context, map_phrase->positive, A_types[i], B_types[i]);
        typecheck_PE_LIST_T_PHRASE(context, map_phrase->negative, B_types[i], A_types[i]);
      }
      else {
        /* covariant (+) phrase (p:Ai->Bi) */
        typecheck_PE_LIST_T_PHRASE(context, map_phrase->positive, A_types[i], B_types[i]);
      }
    }
    else {
      if (map_phrase->negative) {
        /* contra-variant (-) phrase (n:Bi->Ai) */
        typecheck_PE_LIST_T_PHRASE(context, map_phrase->negative, B_types[i], A_types[i]);
      }
      else {
        /* non-variant (?) phrase ( _ ) */
        /* do nothing */
      }
    }
  } /* for */
  A_types[numParams] = 0;
  B_types[numParams] = 0;

  add_equation_to_SubstList(domain, type_con_expr(st_key, A_types));   /* dom=T(As) */
  add_equation_to_SubstList(type_con_expr(st_key, B_types), codomain); /* cod=T(Bs) */
}

/********************/
/*                  */
/*  typecheck_fold  */
/*                  */
/********************/
static void typecheck_fold(TYPED_PATT_LIST *context, PE_FOLD **phrases, TYPE_EXPR *domain, TYPE_EXPR *codomain)
{
  ST_KEY constr_key;
  int num_phrases, i;
  TYPE_EXPR **types, **hash_types;
  ST_TYPE_SIG *st_type_sig;
  TYPE_ASMT_LIST *alist;
  PE_PATT patt;
  TYPE_EXPR *ptype[2];

  if (!phrases || !(phrases[0])) printMsg(FATAL_MSG,"typecheck_fold: phrases is empty");
  constr_key = st_NameToKey(phrases[0]->constr);
  num_phrases = st_GetNumStructors(st_GetStructorParent(constr_key));
  types = (TYPE_EXPR **) MHA(tc_memory, num_phrases+2, sizeof(TYPE_EXPR *));
  hash_types = (TYPE_EXPR **) MHA(tc_memory, num_phrases+1, sizeof(TYPE_EXPR *));
  types[0] = convert_ST_TYPE_to_TE(st_GetGenericStateType(constr_key));         /* L(A) */

  for (i=0; i<num_phrases; i++) {
    st_type_sig = st_GetTypeSig(st_NameToKey(phrases[i]->constr));
    types[i+1] = convert_ST_TYPE_to_TE(st_type_sig->domain);            /* Ei(A,C) */
    hash_types[i] = copy_TE(types[i+1]);
  }
  types[num_phrases+1] = 0;
  hash_types[num_phrases] = 0;
  /* rename the A's */
  alist = assign_new_vars(collect_vars_in_TE(types[0]));
  types = rename_vars_in_TEL(alist, types);
  hash_types = rename_vars_in_TEL(alist, hash_types);

  /* replace C (-1) by codomain */
  types = subst_in_TEL(type_asmt(-1,codomain), types);

  /* replace C (-1) by L(A) in hash types */
  hash_types = subst_in_TEL(type_asmt(-1,types[0]), hash_types);

  ptype[1] = 0;
  patt.tag = P_VAR;
  patt.info.var = HASH_NAME;
  for (i=0; i<num_phrases; i++) {
    ptype[0] = hash_types[i];
    typecheck_PE_LIST_T_PHRASE(add_patt_to_context(context, &patt, ptype), phrases[i]->phrases, types[i+1], codomain);
/*
    typecheck_PE_LIST_T_PHRASE(context, phrases[i]->phrases, types[i+1], codomain);
*/
  }
  add_equation_to_SubstList(domain, types[0]);
}

/**********************/
/*                    */
/*  typecheck_unfold  */
/*                    */
/**********************/
static void typecheck_unfold(TYPED_PATT_LIST *context, PE_UNFOLD **phrases, TYPE_EXPR *domain, TYPE_EXPR *codomain)
{
  ST_KEY destr_key;
  int num_phrases, i, j;
  TYPE_EXPR **types;
  ST_TYPE_SIG *st_type_sig;
  PE_PATT patt;
  TYPE_EXPR *ptype[3];
  TYPE_ASMT *asmt1, *asmt2;

  if (!phrases || !(phrases[0])) printMsg(FATAL_MSG,"typecheck_unfold: phrases is empty");
  destr_key = st_NameToKey(phrases[0]->destr);
  num_phrases = st_GetNumStructors(st_GetStructorParent(destr_key));
  types = (TYPE_EXPR **) MHA(tc_memory, (num_phrases*2)+2, sizeof(TYPE_EXPR *));
  types[0] = convert_ST_TYPE_to_TE(st_GetGenericStateType(destr_key));          /* R(A) */

  for (i=0; i<num_phrases; i++) {
    j=i*2;
    st_type_sig = st_GetTypeSig(st_NameToKey(phrases[i]->destr));
    types[j+1] = convert_ST_TYPE_to_TE(st_type_sig->domain);
    types[j+2] = convert_ST_TYPE_to_TE(st_type_sig->codomain);
  }
  types[(num_phrases*2)+1] = 0;
  /* rename the A's */
  types = rename_vars_in_TEL(assign_new_vars(collect_vars_in_TE(types[0])), types);


  /* @ : R(A) -> (C @ R(A)) */
  ptype[0] = types[0];                                  /* R(A) */
  ptype[1] = product_type_expr(domain, types[0]);       /* dom @ R(A) */
  next_AT_con--;
  ptype[1]->id.con = next_AT_con;       /* new @ type constructor (-2 and lower) */
  ptype[2] = 0;
  patt.tag = P_HOVAR;
  patt.info.var = AT_NAME;
  context = add_patt_to_context(context, &patt, ptype);


  asmt1 = type_asmt(-1,domain);         /* C := dom */
  asmt2 = type_asmt(-1,ptype[1]);       /* C := dom @ R(A) */
  for (i=0; i<num_phrases; i++) {
    j=i*2;
    types[j+1] = subst_in_TE(asmt1,types[j+1]); /* replace C by dom in phrase domain */
    types[j+2] = subst_in_TE(asmt2,types[j+2]); /* replace C by dom@R(A) in Fi(A,C) */
    typecheck_PE_LIST_T_PHRASE(context, phrases[i]->phrases, types[j+1], types[j+2]);
  }
  add_equation_to_SubstList(types[0], codomain);


  /* apply SubstList to codomain and check that it contains no @ from present level */
  if (type_con_occurs_in_TE(next_AT_con, subst_all_in_TE(SubstList,codomain))) {
    tc_close_typechecker(0);
    printMsg(ERROR_MSG,"@ type occurs in unfold codomain");
  }

  /* remove all @ occurrences (present level only) from SubstList */
  SubstList = eliminate_AT_in_TAL(next_AT_con, SubstList);
}

/**********************/
/*                    */
/*  typecheck_record  */
/*                    */
/**********************/
static void typecheck_record(TYPED_PATT_LIST *context, PE_RECORD **phrases, TYPE_EXPR *domain, TYPE_EXPR *codomain)
{
  ST_KEY destr_key;
  int num_phrases, i, j;
  TYPE_EXPR **types;
  ST_TYPE_SIG *st_type_sig;

  if (!phrases || !(phrases[0])) printMsg(FATAL_MSG,"typecheck_record: phrases is empty");
  destr_key = st_NameToKey(phrases[0]->destr);
  num_phrases = st_GetNumStructors(st_GetStructorParent(destr_key));
  types = (TYPE_EXPR **) MHA(tc_memory, (num_phrases*2)+2, sizeof(TYPE_EXPR *));
  types[0] = convert_ST_TYPE_to_TE(st_GetGenericStateType(destr_key));          /* R(A) */

  for (i=0; i<num_phrases; i++) {
    j=i*2;
    st_type_sig = st_GetTypeSig(st_NameToKey(phrases[i]->destr));
    types[j+1] = convert_ST_TYPE_to_TE(st_type_sig->domain);
    types[j+2] = convert_ST_TYPE_to_TE(st_type_sig->codomain);
  }
  types[(num_phrases*2)+1] = 0;
  /* rename the A's */
  types = rename_vars_in_TEL(assign_new_vars(collect_vars_in_TE(types[0])), types);
  /* replace C (-1) by R(A) */
  types = subst_in_TEL(type_asmt(-1,types[0]), types);

  for (i=0; i<num_phrases; i++) {
    destr_key = st_NameToKey(phrases[i]->destr);
    j=i*2;
    if (st_IsHO(destr_key)) {   /* higher order di:Ei(A)*C->Fi(A,C) */
      if (types[j+1]->tag == TYPE_CON  &&  types[j+1]->id.con == product_con) {
        typecheck_PE_TERM(context, phrases[i]->cases, types[j+1]->params[0], types[j+2]);
      }
      else printMsg(FATAL_MSG, "typecheck_record: h.o. destr has illegal domain");
    }
    else {                      /* first order di:C->Fi(A,C) */
      typecheck_PE_EXPR(context, phrases[i]->expr, types[j+2]);
    }
  }

  add_equation_to_SubstList(domain, terminal_type);
  add_equation_to_SubstList(types[0], codomain);
}

/***********************/
/*                     */
/*  typecheck_PE_TERM  */
/*                     */
/***********************/
static void typecheck_PE_TERM(TYPED_PATT_LIST *context, PE_TERM *term, TYPE_EXPR *domain, TYPE_EXPR *codomain)
{
  TYPE_EXPR **types;

#if DEBUG
disp_PE_TERM_sequent(context, term, domain, codomain);
#endif

  if (!term) printMsg(FATAL_MSG,"typecheck_PE_TERM: term is NULL");
  switch (term->tag) {
    case T_STRUCTOR:
      typecheck_structor(term->info.struct_name, domain, codomain);
      break;
    case T_FUNCTION:
      typecheck_PE_FUNCTION(context, term->info.function, domain, codomain);
      break;
    case T_MACRO:
      /* it should be in the context */
      types = find_var_in_context(context, term->info.macro->macro_name);
      if (types) {
        if (TEL_length(types) != 2) printMsg(FATAL_MSG,"typecheck_PE_TERM: illegal type for macro");
        add_equation_to_SubstList(domain, types[0]);
        add_equation_to_SubstList(types[1], codomain);
      }
      else printMsg(FATAL_MSG,"typecheck_PE_TERM: macro not found in context");
      break;
    case T_MAP:
      typecheck_PE_MAP(context, term->info.maps, domain, codomain);
      break;
    case T_FOLD:
      typecheck_fold(context, term->info.folds, domain, codomain);
      break;
    case T_COMPLETE_CASE:
    case T_CASE:
      typecheck_PE_LIST_T_PHRASE(context, term->info.cases, domain, codomain);
      break;
    case T_UNFOLD:
      typecheck_unfold(context, term->info.unfolds, domain, codomain);
      break;
    case T_RECORD:
      typecheck_record(context, term->info.records, domain, codomain);
      break;
    case T_BUILTIN:
      if (term->info.builtin) {
        switch (term->info.builtin->tag) {
          case BI_INT:
            add_equation_to_SubstList(int_type, codomain);
            break;
          case BI_CHAR:
            add_equation_to_SubstList(char_type, codomain);
            break;
          default:
            printMsg(FATAL_MSG, "typecheck_PE_TERM: unknown builtin tag");
        }
        add_equation_to_SubstList(domain, terminal_type);
      }
      else {
        printMsg(FATAL_MSG,"typecheck_PE_TERM: builtin term is NULL");
      }
      break;
    default:
      printMsg(FATAL_MSG,"typecheck_PE_TERM: unknown tag");
  }
}

/*************************/
/*                       */
/*  find_var_in_context  */
/*                       */
/*************************/
static TYPE_EXPR **find_var_in_context(TYPED_PATT_LIST *context, char *var)
{
  TYPED_PATT *tp;
  PE_PATT *patt;

  if (!var) printMsg(FATAL_MSG,"find_var_in_context: var is NULL");
  while (context) {
    tp = TPL_head(context);
    patt = tp->patt;
    switch (patt->tag) {
      case P_VAR:
        if (strcmp(patt->info.var,var)==0) return tp->type;
        break;
      case P_HOVAR:
        if (strcmp(patt->info.hovar.hovar,var)==0) return tp->type;
        break;
      default:
        printMsg(FATAL_MSG,"find_var_in_context: pattern in context is not a variable");
    }
    context = TPL_tail(context);
  }
  return 0;     /* not found */
}

/***********************/
/*                     */
/*  typecheck_PE_EXPR  */
/*                     */
/***********************/
static void typecheck_PE_EXPR(TYPED_PATT_LIST *context, PE_EXPR *expr, TYPE_EXPR *type)
{
  TYPE_EXPR **types;

#if DEBUG
disp_PE_EXPR_sequent(context, expr, type);
#endif

  if (!expr) printMsg(FATAL_MSG,"typecheck_PE_EXPR: expr is NULL");
  switch (expr->tag) {
    case E_VAR:
      types = find_var_in_context(context, expr->info.var);
      if (types) {
        if (TEL_length(types) == 1) add_equation_to_SubstList(types[0], type);
        else printMsg(FATAL_MSG, "typecheck_PE_EXPR: E_VAR has illegal type");
      }
      else printMsg(FATAL_MSG,"typecheck_PE_EXPR: E_VAR nowhere in context");
      break;
    case E_PAIR:
      types = (TYPE_EXPR **) MHA(tc_memory, 3, sizeof(TYPE_EXPR *));
      types[0] = type_var_expr(new_type_var());
      types[1] = type_var_expr(new_type_var());
      types[2] = 0;
      typecheck_PE_EXPR(context, expr->info.epair.l, types[0]);
      typecheck_PE_EXPR(context, expr->info.epair.r, types[1]);
      add_equation_to_SubstList(type_con_expr(product_con, types), type);
      break;
    case E_APP:
      types = (TYPE_EXPR **) MHA(tc_memory, 1, sizeof(TYPE_EXPR *));
      types[0] = type_var_expr(new_type_var());
      typecheck_PE_EXPR(context, expr->info.app.expr, types[0]);
      typecheck_PE_TERM(context, expr->info.app.term, types[0], type);
      break;
    case E_BANG:
      add_equation_to_SubstList(terminal_type, type);
      break;
    default:
      printMsg(FATAL_MSG,"typecheck_PE_EXPR: unknown tag");
  }
}

/*******************/
/*                 */
/*  type_var_expr  */
/*                 */
/*******************/
static TYPE_EXPR *type_var_expr(TC_TYPE_VARIABLE var)
{
  TYPE_EXPR *type;
  type = (TYPE_EXPR *) MHA(tc_memory, 1, sizeof(TYPE_EXPR));
  type->tag = TYPE_VAR;
  type->id.var = var;
  return type;
}

/*******************/
/*                 */
/*  type_con_expr  */
/*                 */
/*******************/
static TYPE_EXPR *type_con_expr(TC_TYPE_CONSTRUCTOR con, TYPE_EXPR **params)
{
  TYPE_EXPR *type;
  type = (TYPE_EXPR *) MHA(tc_memory, 1, sizeof(TYPE_EXPR));
  type->tag = TYPE_CON;
  type->id.con = con;
  type->params = params;
  return type;
}

/***********************/
/*                     */
/*  product_type_expr  */
/*                     */
/***********************/
static TYPE_EXPR *product_type_expr(TYPE_EXPR *left, TYPE_EXPR *right)
{
  TYPE_EXPR **params;
  params = (TYPE_EXPR **) MHA(tc_memory, 3, sizeof(TYPE_EXPR *));
  params[0] = left;
  params[1] = right;
  params[2] = 0;
  return type_con_expr(product_con, params);
}
