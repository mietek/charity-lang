// This module contains code used when the user is first creating a generic component and when the user instantiates
// a generic component (i.e. puts it in a circuit).

implementation module generics

import StdEnv
import circuitDefs, typeDefs, circuitSave, utilities

////////// GetGenericName
//*^*GetGenericName :: !CompSpecifics -> String
GetGenericName :: !(CompSpecifics Component) -> String
GetGenericName (Generic name _ _) = name

////////// MakeNewGeneric
// This returns temporary information about a new generic component, after the user
// has entered its name and numbers of inputs and outputs, but before the user has
// given the types on the wires (arbitrary variables are temporarily assigned to
// the inputs and outputs).

//*^*MakeNewGeneric :: String Int Int -> (CompSpecifics, Int, Int)
MakeNewGeneric :: String Int Int -> (CompSpecifics Component, Int, Int)
MakeNewGeneric compName inputCount outputCount
  = (Generic compName inputTypes outputTypes, compWidth, paddedLineHeight)
where
  inputTypes       = [Var x \\ x<-[1..inputCount]]
  outputTypes      = [Var x \\ x<-[(inputCount+1)..(inputCount+outputCount)]]
  compWidth        = maxList [terminalWidth * inputCount, terminalWidth * outputCount,
                              (FontStringWidth compName currentFont.font) + 2*currentFont.padding
                             ]
  terminalWidth    = 2*terminalRadius
  paddedLineHeight = currentFont.ascent + currentFont.descent + 2*currentFont.padding

////////// SetGenericTypes
// This function is called after the user has set the types for a new generic component to
// convert the user's variables (both UserVar and Var) to new variables starting at 0.
// Variables normally start at 1, but starting at zero allows the component to be
// instantiated by adding the next free variable number to all connection variables.

//*^*SetGenericTypes :: !CompSpecifics -> CompSpecifics
SetGenericTypes :: !(CompSpecifics Component) -> (CompSpecifics Component)
SetGenericTypes (Generic name inputTypes outputTypes)
  = Generic name newInputTypes newOutputTypes
where
  (newInputTypes, typeSubs, nextVar) = ResetGenericTypes inputTypes [] 0
  (newOutputTypes, _, _)             = ResetGenericTypes outputTypes typeSubs nextVar

////////// ResetGenericTypes
ResetGenericTypes :: ![Type] [(Type, Int)] Int -> ([Type], [(Type, Int)], Int)
ResetGenericTypes [type : types] subs1 nextVar1
  = ([newType : newTypes], subs3, nextVar3)
where
  (newType, subs2, nextVar2)  = ResetTypeVars type subs1 nextVar1
  (newTypes, subs3, nextVar3) = ResetGenericTypes types subs2 nextVar2

ResetGenericTypes [] subs nextVar = ([], subs, nextVar)

////////// ResetTypeVars
// Takes a type, a list of substitution pairs giving variable-number
// substitutions for variables and user variables already
// encountered, and the next free variable number, and returns the
// type with all variables and user variables replaced with new
// variables, a new list giving variable-number substitutions for
// variables and user variables (with any new ones added), and the
// new next free variable number.
ResetTypeVars :: !Type [(Type, Int)] Int -> (Type, [(Type, Int)], Int)

ResetTypeVars (Var x) subs nextVar
  | isEmpty backSubs = (Var nextVar, [(Var x, nextVar) : subs], nextVar+1)
  | otherwise        = (Var (snd (hd backSubs)), subs, nextVar)
where
  backSubs = dropWhile (not o (EqualTypes (Var x)) o fst) subs

ResetTypeVars (UserVar1 s) subs nextVar
  | isEmpty backSubs = (Var nextVar, [(UserVar1 s, nextVar) : subs], nextVar+1)
  | otherwise        = (Var (snd (hd backSubs)), subs, nextVar)
where
  backSubs = dropWhile (not o (EqualTypes (UserVar1 s)) o fst) subs

ResetTypeVars (UserVar2 x) subs nextVar
  | isEmpty backSubs = (Var nextVar, [(UserVar2 x, nextVar) : subs], nextVar+1)
  | otherwise        = (Var (snd (hd backSubs)), subs, nextVar)
where
  backSubs = dropWhile (not o (EqualTypes (UserVar2 x)) o fst) subs

ResetTypeVars (Product (type1a, type2a)) subs1 nextVar1
  = (Product (type1b, type2b), subs3, nextVar3)
where
  (type1b, subs2, nextVar2) = ResetTypeVars type1a subs1 nextVar1
  (type2b, subs3, nextVar3) = ResetTypeVars type2a subs2 nextVar2

ResetTypeVars (Sum (type1a, type2a)) subs1 nextVar1
  = (Sum (type1b, type2b), subs3, nextVar3)
where
  (type1b, subs2, nextVar2) = ResetTypeVars type1a subs1 nextVar1
  (type2b, subs3, nextVar3) = ResetTypeVars type2a subs2 nextVar2

ResetTypeVars (Then (type1a, type2a)) subs1 nextVar1
  = (Then (type1b, type2b), subs3, nextVar3)
where
  (type1b, subs2, nextVar2) = ResetTypeVars type1a subs1 nextVar1
  (type2b, subs3, nextVar3) = ResetTypeVars type2a subs2 nextVar2

ResetTypeVars (UserFunc name types1) subs1 nextVar1
  = (UserFunc name types2, subs2, nextVar2)
where
  (types2, subs2, nextVar2) = ResetGenericTypes types1 subs1 nextVar1

ResetTypeVars type subs nextVar = (type, subs, nextVar)

////////// MakeGenericComp
// This is called when putting a generic component in a circuit - it positions the component
// and creates its connections using the types obtained by adding the next free variable
// number to variables in the input and output types.  Returns the new component and the new
// next variable number.

//*^*MakeGenericComp :: !(!CompSpecifics, Int, Int) !Point -> (Component, Int)
MakeGenericComp :: !(!(CompSpecifics Component), Int, Int) ComponentID Int !Point -> (Component, Int)
MakeGenericComp (Generic name inputTypes1 outputTypes1, width, height) compID nextVar (centreX, centreY)
  = ({spec=Generic name inputTypes1 outputTypes1, id=compID, inputs=newInputs, outputs=newOutputs,
      pos=RCT ((centreX - halfWidth, centreY - halfHeight), (centreX + halfWidth, centreY + halfHeight))
     },
     maxVar2+1
    )
where
  newInputs  = [{cWireID=(-1), cWireType=Free type} \\ type <- inputTypes2]
  newOutputs = [{cWireID=(-1), cWireType=Free type} \\ type <- outputTypes2]
  (inputTypes2, maxVar1)  = AddToVarsInTypes nextVar inputTypes1 (nextVar-1)
  (outputTypes2, maxVar2) = AddToVarsInTypes nextVar outputTypes1 maxVar1
  halfWidth  = width/2
  halfHeight = height/2

////////// SaveGeneric
// This saves a generic-component definition to a file at the file-pointer position.
// The file pointer should be positioned at the end of the file.
//*^*SaveGeneric :: !(CompSpecifics, Int, Int) *File -> *File
SaveGeneric :: !(CompSpecifics Component, Int, Int) *File -> *File
SaveGeneric (Generic name inputTypes outputTypes, width, height) file
  = seq [fwrites (name +++ "\n" +++ startMark), WriteTypes inputTypes, fwrites (endMark +++ startMark),
         WriteTypes outputTypes, fwrites (endMark +++ (toString width) +++ "\n" +++ (toString height) +++ "\n")
        ] file

////////// ReadGeneric
// This reads a generic-component definition from a file.  The file pointer should be
// positioned at the beginning of a rewrite (or at the end of the file).
// Returns:
//   - True if the end of the file has been reached, False otherwise,
//   - True if it read a generic, False otherwise (always False if the first argument is True),
//   - the CompSpecifics for the new component, read from the file-pointer position,
//   - the new file.
//*^*ReadGeneric :: !*File -> (Bool, Bool, (CompSpecifics, Int, Int), *File)
ReadGeneric :: !*File -> (Bool, Bool, (CompSpecifics Component, Int, Int), *File)
ReadGeneric file1
  | nameSize < 2 = if endOfFile
                      (True, False, (StartTerminal, 0, 0), file2b)
                      (False, False, (StartTerminal, 0, 0), file2b)
  | not success1 = (False, False, (StartTerminal, 0, 0), file3)
  | not success2 = (False, False, (StartTerminal, 0, 0), file4)
  | not success3 = (False, False, (StartTerminal, 0, 0), file5)
  | not success4 = (False, False, (StartTerminal, 0, 0), file6)
  | not success5 = (False, False, (StartTerminal, 0, 0), file7)
  | not success6 = (False, False, (StartTerminal, 0, 0), file8)
  | otherwise    = (False, True, (Generic name2 inputTypes outputTypes, width, height), file8)
where
  (name1, file2)                 = freadline file1
  nameSize                       = size name1
  name2                          = name1 % (0, nameSize - 2)
  (endOfFile, file2b)            = fend file2
  (success1, file3)              = ReadStartMark file2
  (success2, inputTypes, file4)  = ReadTypes file3
  (success3, file5)              = ReadStartMark file4
  (success4, outputTypes, file6) = ReadTypes file5
  (success5, width, file7)       = ReadInt file6
  (success6, height, file8)      = ReadInt file7
