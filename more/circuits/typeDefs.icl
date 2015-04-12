// This module gives definitions for the Type and WireType types and some utility functions used with
// these types (for displaying types and checking equality of types).

implementation module typeDefs

import StdEnv

// Var is for variables, UserVar1 is for variables given by a user when specifying a type on a wire, UserVar2 is
// for variables in "user-defined" types introduced by a rewrite, Const is for constants (also specified by the
// user), Then is for "implications" given by a lolly, and UserFunc is for type functions specified by the user.
:: Type = Var Int | UserVar1 String | UserVar2 Int | Const String | Product (Type, Type) | Sum (Type, Type) |
          Then (Type, Type) | Unit | Counit | UserFunc String [Type]

// User indicates that the type was specified by the user, Free means that the type was not specified (and should
// be as general as possible given the other types in the circuit).
:: WireType = User Type | Free Type

// The following functions are used to display types.
/////////// WireTypeString
WireTypeString :: !WireType -> String
WireTypeString (Free type) = "Free: " +++ (TypeString type)
WireTypeString (User type) = "User: " +++ (TypeString type)

/////////// TypeString
TypeString :: !Type -> String
TypeString Unit = "Unit"
TypeString Counit = "Counit"
TypeString (Var a) = "'" +++ (VariableString a)
TypeString (UserVar1 a) = a
TypeString (UserVar2 a) = "*" +++ (VariableString a)
TypeString (Const a) = "\"" +++ a +++ "\""
TypeString (Product (type1, type2)) = "(" +++ (TypeString type1) +++ " (*) " +++ (TypeString type2) +++ ")"
TypeString (Sum (type1, type2)) = "(" +++ (TypeString type1) +++ " (+) " +++ (TypeString type2) +++ ")"
TypeString (Then (type1, type2)) = "(" +++ (TypeString type1) +++ " (-o) " +++ (TypeString type2) +++ ")"
TypeString (UserFunc name types) = name +++ "(" +++ (TypesString types) +++ ")"

/////////// TypesString
TypesString :: [Type] -> String
TypesString [type]         = (TypeString type)
TypesString [type : types] = (TypeString type) +++ ", " +++ (TypesString types)
TypesString []             = ""

// Converts an integer into a string, in the order {"A",..,"Z","AA",..,"AZ","BA",..}
/////////// VariableString
VariableString :: !Int -> String
VariableString 0 = ""
VariableString n = (VariableString ((n-1)/26)) +++ (toString (toChar (((n-1) mod 26) + (toInt 'A'))))

/////////// EqualTypes
EqualTypes :: !Type !Type -> Bool
EqualTypes (Var a) (Var b)
  = a==b
EqualTypes (UserVar1 a) (UserVar1 b)
  = a==b
EqualTypes (UserVar2 a) (UserVar2 b)
  = a==b
EqualTypes (Const a) (Const b)
  = a==b
EqualTypes (Product (type1A, type2A)) (Product (type1B, type2B))
  = (EqualTypes type1A type1B) && (EqualTypes type2A type2B)
EqualTypes (Sum (type1A, type2A)) (Sum (type1B, type2B))
  = (EqualTypes type1A type1B) && (EqualTypes type2A type2B)
EqualTypes (Then (type1A, type2A)) (Then (type1B, type2B))
  = (EqualTypes type1A type1B) && (EqualTypes type2A type2B)
EqualTypes (UserFunc name1 types1) (UserFunc name2 types2)
  = (name1==name2) && (EqualTypesLists types1 types2)
EqualTypes Unit Unit = True
EqualTypes Counit Counit = True
EqualTypes _ _ = False

/////////// EqualTypesLists
EqualTypesLists :: [Type] [Type] -> Bool
EqualTypesLists [type1 : types1] [type2 : types2] = (EqualTypes type1 type2) && (EqualTypesLists types1 types2)
EqualTypesLists [] [] = True
EqualTypesLists _  _  = False
