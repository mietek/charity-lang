// Contains the code used to parse a type from a string (used when the user specifies
// the type for a wire).

definition module parseType

import StdEnv
import typeDefs

ParseType :: !String -> (Bool, Type)
