// This module gives definitions for the Type and WireType types and some utility functions used with
// these types (for displaying types and checking equality of types).

definition module typeDefs

import StdEnv

// Var is for variables, UserVar1 is for variables given by a user when specifying a type on a wire, UserVar2 is
// for variables in "user-defined" types introduced by a rewrite, Const is for constants (also specified by the
// user), Then is for "implications" given by a lolly, and UserFunc is for type functions specified by the user.
:: Type = Var Int | UserVar1 String | UserVar2 Int | Const String | Product (Type, Type) | Sum (Type, Type) |
          Then (Type, Type) | Unit | Counit | UserFunc String [Type]

// User indicates that the type was specified by the user, Free means that the type was not specified (and should
// be as general as possible given the other types in the circuit).
:: WireType = User Type | Free Type

WireTypeString :: !WireType -> String
TypeString :: !Type -> String
EqualTypes :: !Type !Type -> Bool
