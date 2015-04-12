// Contains code used to unify or match two types.

definition module unify

import typeDefs

:: Substitution :== (Int, Type)

// Unifies the two types, returning True if they unify
// and a list of variable substitutions that will make
// them equal.
Unify :: !Type !Type -> (Bool, [Substitution])
UnifyArgs :: ![Type] ![Type] -> (Bool, [Substitution])

// Makes variable substitutions in a type.
SubsIntoType :: ![Substitution] Type -> Type

// Returns True if the two types are both products, sums, etc.
SameConstructor :: !Type Type -> Bool

// Returns the type arguments to a type (e.g. the two types that
// make up a product).
Args :: !Type -> [Type]

// Matches two types.  Returns True if the first type is no more
// general than the second, with a list of variable substitutions that
// will make second type equal to the first.  It allows substitutions
// that replace a variable with an type that uses the variable - the
// two types are assumed to have independent variables.  It won't allow
// a variable to be substituted by more than one type.
MatchTypes :: Type !Type [Substitution] -> (Bool, [Substitution])
