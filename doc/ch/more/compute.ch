(******************************************************************************
**
** HIGHER-ORDER CHARITY EXAMPLES MOTIVATED BY THE BOOK:
**
**      Computational Category Theory
**      by
**         D. E. Rydeheard
**         R. M. Burstall
**
**      (Prentice Hall, 1988)
**
** AUTHOR:
**
**      Marc A. Schroeder
**
**      Formal Methods Group
**      Dept. of Computer Science
**      The University of Calgary,
**      CANADA
**
**      marc@cpsc.ucalgary.ca
**
** CREATED       : 4 Sept. 1995
** LAST MODIFIED : 4 Sept. 1995
**
** PURPOSE: Rydeheard and Burstall observe that a good deal of the theorems
**          in Category Theory are constructive (ie. their proofs involve the
** constructions of the objects they describe). In the above text, they
** develop basic Category Theory and each time a new categorical object or
** constructive proof is encountered, they represent it either as a new
** SML type or function (respectively). In a sense they "implement" the
** theory.
**
** These types and functions are redeveloped here, except they are encoded
** in the Charity programming language instead. This is for two reasons:
**
**      (1) they serve as a good examples of Higher-Order Charity, and
**
**      (2) as Charity is a categorical programming language, it is likely
**          the most suitable language for the task. In fact, being able to
**          encode (some of) Category Theory as a Charity script tells us
**          something of the nature of both Charity and the theory.
**
** METHOD: More specifically:
**
**      (1) Mathematical objects are encoded as types. We define a new
**          type for each new object we need. When one kind of object is
**          defined in terms of another, we make use of parametric
**          polymorphism.
**
**      (2) Constructions are encoded as functions over these types. These
**          functions will also be polymorphic, as we wish them to be as
**          general as possible.
**
**      (3) Objects are usually defined not only as some collection of
**          sets (or classes), but instead also have some structure imposed
**          upon those sets via typed operations. To encode this situation,
**          the types we define also need to make use of higher-order
**          functions.
**
** For a concrete example of the above points, consider the definition of a
** type for representing categories, below.
**
******************************************************************************)



(******************************************************************************
**
** MISC. TYPES
**
******************************************************************************)

data sf(A) -> C = ss : A -> C      % THE "SUCCESS-OR-FAIL" TYPE, USED BELOW
                | ff : 1 -> C.

data C -> exp(A, B) = fn : C -> A => B.     % THE EXPONENTIAL TYPE, USED BELOW



(******************************************************************************
**
** CATEGORIES
**
** TYPE: cat
**
** PURPOSE: Represents category objects.
**
******************************************************************************)

data C -> cat(Obj, Mor) = domain   : C -> Obj^Mor     % EXTEND SYNTAX?
                        | codomain : C -> Obj^Mor
                        | id       : C -> Mor^Obj
                        | comp     : C -> sf(Mor)^(Mor * Mor).

(*
 * NOTE: Categories satisfy some properties which aren't embodied in the
 *       above type specification (ie. id must be 1-1, comp must work only
 * for composable morphisms, and the identity/associative laws must be
 * satisfied---as an aside, checking that morphisms are composable involves
 * an equality check of two objects (values), which can be a problem).
 *
 * Similar representational problems range throughout this endeavor, and
 * are essentially unavoidable. The onus is on the programmer to ensure that
 * when representing a category, or encoding any mathematical structure, it
 * has been appropriately modeled.
 *
 * NOTE: The type system is not powerful enough to handle such categories
 *       as the category of categories.
 *
 * NOTE: The above TYPE definition captures the THEORY of categories.
 *       VALUES of this type are MODELS of the theory (ie. examples of
 * categories, given that the type variables Obj and Mor are instantiated).
 *
 *)

% SHOULD CODE UP SOME EXAMPLES HERE



(******************************************************************************
**
** FUNCTORS
**
** TYPE: fun
**
** PURPOSE: Represents functor objects.
**
******************************************************************************)

data C -> fun(ObjA, MorA, ObjB, MorB) = fun_domain   : C -> cat(ObjA, MorA)
                                      | fun_codomain : C -> cat(ObjB, MorB)
                                      | obj_map      : C -> ObjB^ObjA
                                      | mor_map      : C -> MorB^MorA.

(*
 * NOTE: A functor is a kind of object, and so is represented as a type.
 *       However, a functor may not only be thought of as a morphism of
 * categories, but also as a CONSTRUCTION. (In fact, any special structure a
 * category possesses can, in general, but captured by functors to/from that
 * category). We can thus convert a value of this type to a function over
 * categories by destructing for obj_map and mor_map, then using those as
 * the parameters to the map_cat operation.
 *
 *)

% SHOULD CODE UP SOME EXAMPLES HERE



(******************************************************************************
**
** DUALITY
**
** FUNCTIONS: dual_cat
**            dual_fun
**
** PURPOSE: Construct dual categories and dual functors, respectively.
**
******************************************************************************)

def dual_cat(C) =
    ( domain   : codomain(C),
      codomain : domain(C),
      id       : id(C),
      comp     : (fn: (g, f) => comp(C) (f, g))     % EXTEND SYNTAX?
    ).

def dual_fun(F) =
    ( fun_domain   : dual_cat(fun_domain(F)),
      fun_codomain : dual_cat(fun_codomain(F)),
      obj_map      : obj_map(F),
      mor_map      : mor_map(F)
    ).
