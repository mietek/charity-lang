// This module contains code used when the user is first creating a generic component and when the user instantiates
// a generic component (i.e. puts it in a circuit).

definition module generics

import StdEnv
import circuitDefs

 // Returns the name of a generic (user-defined) component, given its CompSpecifics.
//*^*GetGenericName :: !CompSpecifics -> String
GetGenericName :: !(CompSpecifics Component) -> String

 // This returns temporary information about a new generic component, after the user
 // has entered its name and numbers of inputs and outputs, but before the user has
 // given the types on the wires (arbitrary variables are temporarily assigned to
 // the inputs and outputs).
//*^*MakeNewGeneric :: String Int Int -> (CompSpecifics, Int, Int)
MakeNewGeneric :: String Int Int -> (CompSpecifics Component, Int, Int)

 // This function is called after the user has set the types for a new generic component to
 // convert the user's variables (both UserVar and Var) to new variables starting at 0.
 // Variables normally start at 1, but starting at zero allows the component to be 
 // instantiated by adding the next free variable number to all connection variables.
//*^*SetGenericTypes :: !CompSpecifics -> CompSpecifics
SetGenericTypes :: !(CompSpecifics Component) -> (CompSpecifics Component)

 // This is called when putting a generic component in a circuit - it positions the component
 // and creates its connections using the types obtained by adding the next free variable
 // number to variables in the input and output types.  Returns the new component and the new
 // next variable number.
//*^*MakeGenericComp :: !(!CompSpecifics, Int, Int) !Point -> (Component, Int)
MakeGenericComp :: !(!(CompSpecifics Component), Int, Int) ComponentID Int !Point -> (Component, Int)

 // This saves a generic-component definition to a file at the file-pointer position.
 // The file pointer should be positioned at the end of the file.
//*^*SaveGeneric :: !(CompSpecifics, Int, Int) *File -> *File
SaveGeneric :: !(CompSpecifics Component, Int, Int) *File -> *File

 // This reads a generic-component definition from a file.  The file pointer should be 
 // positioned at the beginning of a rewrite (or at the end of the file).
 // Returns:
 //   - True if the end of the file has been reached, False otherwise,
 //   - True if it read a generic, False otherwise (always False if the first argument is True),
 //   - the CompSpecifics for the new component, read from the file-pointer position,
 //   - the new file.
//*^*ReadGeneric :: !*File -> (Bool, Bool, (CompSpecifics, Int, Int), *File)
ReadGeneric :: !*File -> (Bool, Bool, (CompSpecifics Component, Int, Int), *File)
