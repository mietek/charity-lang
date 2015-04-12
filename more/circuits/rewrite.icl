// Contains code used in creating new rewrite rules and in performing a rewrite in a circuit.

////////// Changed to add numbered user variables.


//*** DoRewrite1 does not make use of the type of the broken wire connection right now - could
//*** be eliminated.

//*** User variables in the right-side of a rule are made "independent" from the user variables
//*** in the circuit by appending an asterisk.  If a rewrite contained any user variables that
//*** did not appear in its inputs or outputs, they would remain in the final circuit, and
//*** independence would not be guaranteed in further rewrites.

//*** Could probably avoid having SetTypeOn... functions take two circuits as args

//*** Might be able to change DoRewrite2 to split the circuit as it searches, but would have to change MatchLeftSide, etc.
//*** to use the component with the live wire as an *input*.

implementation module rewrite

import StdEnv
import circuitDefs, rewriteDefs, utilities, insert, circuitSave, unify, circuitWireMatch, sequent

/////////// CreateRewrite
// Takes as arguments:
//   - the name of the new rewrite rule,
//   - the circuit for the left side of the rewrite rule,
//   - the circuit for the right side of the rewrite rule,
//   - the wire ID of the live wire (the wire that the user selects to indicate where in a circuit
//     the rewrite should take place) in the left side of the rewrite rule,
//   - the next unused wire ID for the left-side circuit (used to make sure that the only wire IDs
//     the two sides of the rule have in common are for their input and output wires).
// Returns:
//   - an error value indicating what error, if any, occurred,
//   - the new rewrite, if no error occurred.
//
// Steps:
//   - Checks that both sides are fully connected, and that neither side has any start or end
//     terminals in boxes, and that the left side is fully connected (necessary if the rule is to
//     be matched starting from a single wire).
//   - Splits the left-side circuit into four components:  the component from which the live wire
//     connects down (leftComp), the top of the circuit (all of the circuit that has a direct or
//     indirect connection down to leftComp), the bottom of the circuit (all of the circuit that
//     has a direct or indirect connection down from leftComp), and the middle of the circuit
//     (all of the circuit that neither connects down to nor down from leftComp).
//   - Checks that the live wire does not have either end in a box - the function that splits
//     the left side will return a dummy component for leftComp if so.  (The function that does
//     the rewrite assumes that this is true).
//   - Adds the next wire ID from the left side to all wire IDs in the right side to ensure that
//     they have no wire IDs in common.
//   - Get the input and output connections for both sides of the rule and verifies that their
//     numbers match.
//   - Calls CheckTypeMatch to verify that the types of the inputs and outputs of the two sides
//     unify.  This function also returns a list of substitutions needed to unify the two sides,
//     two lists of pairs giving the left-side input and output wire IDs with their matching
//     right-side connections (with variable substitutions), and two lists of input and output
//     wire IDs identifying wires in the right side that should now have user-specified types,
//     because they required substitutions to unify with the left side (this ensures that the
//     proper types will be held on these wires when a rewrite is done).
//   - Removes start and end terminals from the right side of the rule, and finds the enclosing rectangle
//     for the remaining circuit (this is ruleRect).
//   - If the rule is an expansion (i.e. the left side consists of a single wire connecting a start
//     terminal and an end terminal), it changes the right-side's input wire's ID to the live wire's
//     ID and makes variable substitutions in the right side (this isn't needed on the left side,
//     because the left side isn't used for type-matching when doing a rewrite), puts the right side's
//     single input connection in the inConnects list (with its type made free if it isn't already,
//     since an expansion rewrite rule never needs to introduce a user type into a circuit), and saves
//     the right-side's output connection as brokenWireConnect (with its type made free if it isn't
//     already).  It assumes that the user hasn't tried to create a rewrite rule that consists of a
//     single wire on both sides - i.e. the right side must "break" the wire by putting in some
//     components between the input and output wires.
//   - If the rule is not an expansion, it finds all the inputs that are connected to outputs
//     in the right side, and saves a list with the input wire ID (in both the left and right sides),
//     output wire ID (from the left side) and type for each of these in connectedPairs.  The
//     remaining inputs and outputs go into inConnects and outConnects, variable substitutions are
//     made into the right side, user-specified types are put in on the right where necessary
//     because of substitutions, and the wire IDs in the right side are changed to match the left side.
//     A dummy connection with a wire ID of -1 is put in for brokenWireConnect to indicate that the
//     rule is not an expansion.
//
//*^* NOTE:  Should probably replace all user variables with variables at this point (instead of when
//*^*        doing the rewrite).  It would allow variables of the rule to be made independent of the
//*^*        variables in the circuit just by adding the next variable number to them.

CreateRewrite :: String !Circuit Circuit WireID WireID -> (ErrorVal, Rewrite)
CreateRewrite name leftSide rightSide liveWireID nextLeftWireID
  | NotFullyWired leftSide
      = (LeftSideNotWired, dummyRewrite)
  | NotFullyWired rightSide
      = (RightSideNotWired, dummyRewrite)
  | TerminalsInBoxes leftSide
      = (BadLeftSideTerminals, dummyRewrite)
  | TerminalsInBoxes rightSide
      = (BadRightSideTerminals, dummyRewrite)
  | not (IsConnected leftSide)
      = (NotConnected, dummyRewrite)
  | IsDummy leftComp  // This means that the wire has one or both ends inside a box in the circuit.
      = (BadLiveWire, dummyRewrite)
  | (length leftInputs) <> (length rightInputs)
      = (InputCountMismatch, dummyRewrite)
  | (length leftOutputs) <> (length rightOutputs)
      = (OutputCountMismatch, dummyRewrite)
  | not goodTypeMatch
      = (TypeMismatch, dummyRewrite)
  | IsExpansion leftSide
      = (NoError,
         {rewriteName=name, liveWireID=liveWireID, leftComp=leftComp, leftTop=leftTop, leftMid=leftMid, leftBot=leftBot,
          rightSide=rightSide5A, oldRightSide=rightSide, ruleRect=rightSideRect,
          inConnects=[{cWireID=liveWireID, cWireType=inConnectType2}], outConnects=[], connectedPairs=[],
          brokenWireConnect=outConnect2
         }
        )
  | otherwise
      = (NoError,
         {rewriteName=name, liveWireID=liveWireID, leftComp=leftComp, leftTop=leftTop, leftMid=leftMid, leftBot=leftBot,
          rightSide=rightSide6B, oldRightSide=rightSide, ruleRect=rightSideRect, inConnects=rightInputs2,
          outConnects=rightOutputs2, connectedPairs=connectedPairs, brokenWireConnect={cWireID=(-1), cWireType=Free Unit}
         }
        )
where
  (leftComp, leftTop, leftMid, leftBot) = SplitCircuit liveWireID leftSide

  rightSide2                  = AddWireVal nextLeftWireID rightSide
  (leftInputs, leftOutputs)   = GetInputsAndOutputs leftSide
  (rightInputs, rightOutputs) = GetInputsAndOutputs rightSide2

  (goodTypeMatch, subs, inputPairs, outputPairs, newInputUserWires, newOutputUserWires)
    = CheckTypeMatch leftInputs leftOutputs rightInputs rightOutputs

  rightSide3    = RemoveTerminals rightSide2
  rightSideRect = foldl (flip RectangleUnion) dummyRectangle (map ComponentRectangle rightSide3)

  // If the rule is an expansion...
  rightSide3A = SubsIntoCircuit subs (MakeWireSubs rightSide3 [((hd rightInputs).cWireID, liveWireID)])
  inConnectType1 = (snd (hd inputPairs)).cWireType
  outConnect1    = snd (hd outputPairs)
  // An expansion doesn't need any user-specified types, since the type of the wire must be determined by the
  // circuit outside the rewrite.
  (inConnectType2, rightSide4A)
    = if (IsUserType inConnectType1)
         (MakeFree inConnectType1, snd (SetFreeTypeOnInput liveWireID rightSide3A))
         (inConnectType1, rightSide3A)
  (outConnect2, rightSide5A)
    = if (IsUserType outConnect1.cWireType)
         ({outConnect1 & cWireType=MakeFree outConnect1.cWireType},
          snd (SetFreeTypeOnOutput outConnect1.cWireID rightSide4A)
         )
         (outConnect1, rightSide4A)

  // If the rule is not an expansion...
  (rightInputs2, rightOutputs2, connectedPairs, rightWireSubs) = SplitOutConnectedPairs inputPairs outputPairs
  rightSide3B         = SubsIntoCircuit subs rightSide3
  (rightSide4B, _, _) = SetUserTypeOnInputs newInputUserWires rightSide3B [] []
  (rightSide5B, _, _) = SetUserTypeOnOutputs newOutputUserWires rightSide4B [] []
  rightSide6B         = MakeWireSubs rightSide5B rightWireSubs

/////////// TerminalsInBoxes
// Checks to see if there are any start or end terminals in boxes in the circuit.
TerminalsInBoxes :: !Circuit -> Bool
TerminalsInBoxes [{spec=Box boxCircuit} : circuit] = (TerminalsInBox boxCircuit) || (TerminalsInBoxes circuit)
TerminalsInBoxes [_ : circuit]                     = TerminalsInBoxes circuit
TerminalsInBoxes []                                = False

/////////// TerminalsInBox
// Checks to see if there are any start or end terminals in the circuit.
TerminalsInBox :: !Circuit -> Bool
TerminalsInBox [{spec=StartTerminal} : _]        = True
TerminalsInBox [{spec=EndTerminal} : _]          = True
TerminalsInBox [{spec=Box boxCircuit} : circuit] = (TerminalsInBox boxCircuit) || (TerminalsInBox circuit)
TerminalsInBox [_ : circuit]                     = TerminalsInBox circuit
TerminalsInBox []                                = False

/////////// AddWireVal
// Adds the first integer to every wire ID in the circuit.
AddWireVal :: Int !Circuit -> Circuit
AddWireVal incr [box=:{spec=Box boxCircuit, inputs, outputs} : circuit]
  = [{box & spec=Box newBoxCircuit, inputs=newInputs, outputs=newOutputs} : newCircuit]
where
  newInputs     = AddWireValToConnects incr inputs
  newOutputs    = AddWireValToConnects incr outputs
  newBoxCircuit = AddWireVal incr boxCircuit
  newCircuit    = AddWireVal incr circuit

AddWireVal incr [comp=:{inputs, outputs} : circuit]
  = [{comp & inputs=newInputs, outputs=newOutputs} : newCircuit]
where
  newInputs     = AddWireValToConnects incr inputs
  newOutputs    = AddWireValToConnects incr outputs
  newCircuit    = AddWireVal incr circuit

AddWireVal _ [] = []

/////////// AddWireValToConnects
// Adds the integer to every wire ID in the list of connections.
AddWireValToConnects :: Int ![Connection] -> [Connection]
AddWireValToConnects incr [connect=:{cWireID} : connects]
  = [{connect & cWireID=cWireID+incr} : newConnects]
where
  newConnects = AddWireValToConnects incr connects

AddWireValToConnects _ [] = []

/////////// GetInputsAndOutputs
// Finds all the input and output connections in the circuit (connected to start and end terminals
// respectively), sorted by their horizontal positions.
GetInputsAndOutputs :: !Circuit -> ([Connection], [Connection])
GetInputsAndOutputs circuit = (map snd (sort inputs), map snd (sort outputs))
where
  (inputs, outputs) = GetInputsAndOutputsAux circuit [] []

  GetInputsAndOutputsAux [comp=:{spec=StartTerminal, outputs=[outC], pos=PT (x,_)} : circuit] inputPairs outputPairs
    = GetInputsAndOutputsAux circuit [(x, outC) : inputPairs] outputPairs
  GetInputsAndOutputsAux [comp=:{spec=EndTerminal, inputs=[inC], pos=PT (x,_)} : circuit] inputPairs outputPairs
    = GetInputsAndOutputsAux circuit inputPairs [(x, inC) : outputPairs]
  GetInputsAndOutputsAux [comp : circuit] inputPairs outputPairs
    = GetInputsAndOutputsAux circuit inputPairs outputPairs
  GetInputsAndOutputsAux [] inputPairs outputPairs
    = (inputPairs, outputPairs)

/////////// IsConnected
// Checks whether a circuit is connected (not in separate pieces).
// As it goes down the circuit, it keeps a list of lists of wires
// for which the lower connection has not yet been found.  Each
// list contains wires from a part of the circuit that has been
// found to be connected already, and when a component is found,
// all the lists containing wires that connect down to it are
// combined, the connected wires are removed, and the component's
// output wires are added to the new list.  When it is done, it
// will have a single empty list of wires in its list if and only
// if the circuit is connected.
IsConnected :: !Circuit -> Bool
IsConnected circuit = IsConnectedAux circuit []
where
  IsConnectedAux [comp : circuit] topWireLists1
    = IsConnectedAux circuit [(outWireIDs ++ wireGroup) : topWireLists2]
  where
    inWireIDs  = GetInWireIDs comp
    outWireIDs = GetOutWireIDs comp
    (wireGroup, topWireLists2) = MakeWireGroup inWireIDs [] topWireLists1
  IsConnectedAux [] [[]] = True
  IsConnectedAux [] _    = False

/////////// MakeWireGroup
// Takes a list of input connection wire IDs, a list of wire IDs representing
// the wires in the group to which all input connections examined so far
// were connected, and a list of lists of the other output wire IDs.
// For each input wire ID, it checks to see if it connects into the current
// group.  If so, it removes the connecting wire ID, otherwise it finds the
// group in the list of groups to which the input does connect, removes the
// connecting wire, and combines the remaining contents with the current
// group.
// Returns a list of wire IDs representing the remaining wires in the group
// to which the input connections were connected (with the connected wires
// removed), and the remaining list of lists of wire IDs.
MakeWireGroup :: ![WireID] [WireID] [[WireID]] -> ([WireID], [[WireID]])
MakeWireGroup [wireID : wireIDs] wireGroup wireGroups1
  | isEmpty wireGroupBack
      = MakeWireGroup wireIDs (newGroup ++ wireGroup) wireGroups2
  | otherwise
      = MakeWireGroup wireIDs (wireGroupFront ++ (tl wireGroupBack)) wireGroups1
where
  (wireGroupFront, wireGroupBack) = span ((<>) wireID) wireGroup
  (newGroup, wireGroups2) = GetWireGroup wireID wireGroups1 []

MakeWireGroup [] wireGroup wireGroups = (wireGroup, wireGroups)

/////////// GetWireGroup
// Takes the wire ID of an input connection, a list of groups of wire IDs
// that hasn't been examined yet, and a list of groups of wire IDs that
// have already been examined, and returns the group into which the input
// wire connected (with the connected wire ID removed) and the remaining
// groups of wire IDs.
GetWireGroup :: WireID ![[WireID]] [[WireID]] -> ([WireID], [[WireID]])
GetWireGroup wireID [wireGroup : wireGroups] newWireGroups
  | isEmpty wireGroupBack
      = GetWireGroup wireID wireGroups [wireGroup : newWireGroups]
  | otherwise
      = (wireGroupFront ++ (tl wireGroupBack), wireGroups ++ newWireGroups)
where
  (wireGroupFront, wireGroupBack) = span ((<>) wireID) wireGroup

/////////// CheckTypeMatch
// Takes the left-side input connections, the left-side output connections,
// the right-side input connections and the right-side output connections,
// and returns True if their types unify with a list of variable substitutions
// to do the unification, two lists pairing input and output wire IDs from the
// left side with the matching input and output connections from the right side
// (with variable substitutions made), and two lists identifying input and
// output wires on the right side that will need user types because substitutions
// had to be made to unify them with the left side.
CheckTypeMatch :: ![Connection] [Connection] ![Connection] [Connection]
                   -> (Bool, [Substitution], [(WireID, Connection)], [(WireID, Connection)], [WireID], [WireID])
CheckTypeMatch leftInputs leftOutputs rightInputs rightOutputs
  | unifies
     = (True, subs, zip (map GetConnectWireID leftInputs, rightInputs2),
        zip (map GetConnectWireID leftOutputs, rightOutputs2), newInputUserWires, newOutputUserWires
       )
  | otherwise = (False, [], [], [], [], [])
where
  leftTypes1       = map (GetType o GetConnectWireType) (leftInputs ++ leftOutputs)
  rightTypes       = map (GetType o GetConnectWireType) (rightInputs ++ rightOutputs)
  //*^* VVV Maximum right-variable number could be passed as an argument to CreateRewrite.
  maxRightVar      = FindMaxVarInTypes rightTypes 0
  leftTypes2       = AddToVarsInTypes` maxRightVar leftTypes1  // Makes sure the variables are independent.
  (unifies, subs)  = UnifyArgs rightTypes leftTypes2

  (rightInputs2, newInputUserWires)   = SubsIntoRightSideConnects subs rightInputs
  (rightOutputs2, newOutputUserWires) = SubsIntoRightSideConnects subs rightOutputs

//////// AddToVarsInTypes`
// Adds the integer to all the variables and numeric user variables in the list of types.
AddToVarsInTypes` :: Int ![Type] -> [Type]
AddToVarsInTypes` n [type : types] = [newType : newTypes]
where
  newType  = AddToVarsInType` n type
  newTypes = AddToVarsInTypes` n types

AddToVarsInTypes` _ [] = []

//////// AddToVarsInType`
// Adds the integer to all the variables and numeric user variables in the type.
AddToVarsInType` :: Int !Type -> Type

AddToVarsInType` n (Var m)      = Var (n+m)
AddToVarsInType` _ (UserVar1 s) = UserVar1 s
AddToVarsInType` n (UserVar2 m) = UserVar2 (n+m)
AddToVarsInType` _ (Const s)    = Const s
AddToVarsInType` _ Unit         = Unit
AddToVarsInType` _ Counit       = Counit

AddToVarsInType` n (Product (type1, type2)) = Product (newType1, newType2)
where
  newType1 = AddToVarsInType` n type1
  newType2 = AddToVarsInType` n type2

AddToVarsInType` n (Sum (type1, type2)) = Sum (newType1, newType2)
where
  newType1 = AddToVarsInType` n type1
  newType2 = AddToVarsInType` n type2

AddToVarsInType` n (Then (type1, type2)) = Then (newType1, newType2)
where
  newType1 = AddToVarsInType` n type1
  newType2 = AddToVarsInType` n type2

AddToVarsInType` n (UserFunc name types) = UserFunc name (map (AddToVarsInType` n) types)

/////////// SubsIntoRightSideConnects
// Makes variable substitutions in the list of connections, returning a new list
// of connections and a list of wire IDs identifying wires that had substitutions
// made, and that should therefore become user-specified types.
SubsIntoRightSideConnects :: [Substitution] ![Connection] -> ([Connection], [WireID])
SubsIntoRightSideConnects subs [connect=:{cWireID, cWireType} : connects]
  | subMade   = ([{connect & cWireType=newWireType} : newConnects], [cWireID : subWireIDs])
  | otherwise = ([{connect & cWireType=newWireType} : newConnects], subWireIDs)
where
  (subMade, newWireType)    = SubsIntoRightSideWireType subs cWireType
  (newConnects, subWireIDs) = SubsIntoRightSideConnects subs connects

SubsIntoRightSideConnects _ [] = ([], [])

////////// SubsIntoRightSideWireType
// Makes variable substitutions in the wire type.  If a substitution was made,
// it returns True and changes the wire type to a user-specified type.
SubsIntoRightSideWireType :: [Substitution] !WireType -> (Bool, WireType)
SubsIntoRightSideWireType _ (User type)
  = (False, User type)
SubsIntoRightSideWireType subs (Free type)
  | subMade   = (True, User newType)
  | otherwise = (False, Free newType)
where
  (subMade, newType) = SubsIntoRightSideType subs type

///////// SubsIntoRightSideType
// Makes variable substitutions in the type, returning True if a
// substitution was made.
SubsIntoRightSideType :: ![Substitution] Type -> (Bool, Type)
SubsIntoRightSideType [sub : subs] type
  | subMade   = (True, SubsIntoType subs newType)
  | otherwise = SubsIntoRightSideType subs newType
where
  (subMade, newType) = SubIntoRightSideType sub type

SubsIntoRightSideType [] type = (False, type)

///////// SubIntoRightSideType
// Makes a variable substitution in the type, returning True if
// a substitution was made.
SubIntoRightSideType :: !Substitution !Type -> (Bool, Type)

SubIntoRightSideType (a, Var x) (Var b)
  | a==b      = (False, Var x)
  | otherwise = (False, Var b)

SubIntoRightSideType (a, UserVar1 x) (Var b)
  | a==b      = (False, UserVar1 x)
  | otherwise = (False, Var b)

SubIntoRightSideType (a, UserVar2 x) (Var b)
  | a==b      = (False, UserVar2 x)
  | otherwise = (False, Var b)

SubIntoRightSideType (a, subType) (Var b)
  | a==b      = (True, subType)
  | otherwise = (False, Var b)

SubIntoRightSideType sub (Product (type1, type2))
  = (subMade1 || subMade2, Product (newType1, newType2))
where
  (subMade1, newType1) = SubIntoRightSideType sub type1
  (subMade2, newType2) = SubIntoRightSideType sub type2

SubIntoRightSideType sub (Sum (type1, type2))
  = (subMade1 || subMade2, Sum (newType1, newType2))
where
  (subMade1, newType1) = SubIntoRightSideType sub type1
  (subMade2, newType2) = SubIntoRightSideType sub type2

SubIntoRightSideType sub (Then (type1, type2))
  = (subMade1 || subMade2, Then (newType1, newType2))
where
  (subMade1, newType1) = SubIntoRightSideType sub type1
  (subMade2, newType2) = SubIntoRightSideType sub type2

SubIntoRightSideType sub (UserFunc name types)
  = (subMade, UserFunc name newTypes)
where
  (subMade, newTypes) = SubIntoRightSideTypes sub types

SubIntoRightSideType _ type = (False, type)

/////////// SubIntoRightSideTypes
SubIntoRightSideTypes :: Substitution ![Type] -> (Bool, [Type])
SubIntoRightSideTypes sub [type : types]
  = (subMade1 || subMade2, [newType : newTypes])
where
  (subMade1, newType)  = SubIntoRightSideType sub type
  (subMade2, newTypes) = SubIntoRightSideTypes sub types

SubIntoRightSideTypes _ [] = (False, [])

/////////// RemoveTerminals
// Removes all StartTerminals and EndTerminals from a circuit (assuming that
// none are in boxes), returning the circuit otherwise unchanged.
RemoveTerminals :: !Circuit -> Circuit
RemoveTerminals [comp=:{spec=StartTerminal} : circuit] = RemoveTerminals circuit
RemoveTerminals [comp=:{spec=EndTerminal} : circuit] = RemoveTerminals circuit
RemoveTerminals [comp : circuit] = [comp : RemoveTerminals circuit]
RemoveTerminals [] = []

/////////// IsExpansion
// Returns true if the circuit (the left side of a rewrite rule)
// consists of just a single wire connecting a start terminal
// and an end terminal.
IsExpansion :: !Circuit -> Bool
IsExpansion [{spec=StartTerminal}, {spec=EndTerminal}] = True
IsExpansion _ = False

/////////// MakeWireSubs
// Takes a circuit and a list of wire ID substitutions, and changes the
// wire IDs in the circuit.
MakeWireSubs :: !Circuit ![WireSub] -> Circuit
MakeWireSubs circuit [wireSub : wireSubs] = MakeWireSubs newCircuit wireSubs
where
  (_, newCircuit) = MakeWireSub circuit wireSub
MakeWireSubs circuit [] = circuit

//
MakeWireSub :: !Circuit WireSub -> (Bool, Circuit)
MakeWireSub [box=:{spec=Box boxCircuit, inputs=[inC], outputs=[outC1,outC2]} : circuit] (oldID, newID)
  | outC2.cWireID==oldID
      = (True, [{box & outputs=[outC1, {outC2 & cWireID=newID}]} : circuit])
  | foundInBox
      = (True, [{box & spec=Box newBoxCirc} : circuit])
  | otherwise
      = (foundInCirc, [box : newCircuit])
where
  (foundInBox, newBoxCirc)  = MakeWireSub boxCircuit (oldID, newID)
  (foundInCirc, newCircuit) = MakeWireSub circuit (oldID, newID)

MakeWireSub [comp=:{inputs, outputs} : circuit] (oldID, newID)
  | isEmpty outputsBack = if (isEmpty inputsBack)
                             (foundInCirc, [comp : newCircuit])
                             (True, [{comp & inputs=newInputs} : circuit])
  | otherwise           = (True, [{comp & outputs=newOutputs} : circuit])
where
  (outputsFront, outputsBack) = span (((<>) oldID) o GetConnectWireID) outputs
  newOutputs                  = outputsFront ++ [{(hd outputsBack) & cWireID=newID} : tl outputsBack]

  (inputsFront, inputsBack) = span (((<>) oldID) o GetConnectWireID) inputs
  newInputs                 = inputsFront ++ [{(hd inputsBack) & cWireID=newID} : tl inputsBack]

  (foundInCirc, newCircuit) = MakeWireSub circuit (oldID, newID)

MakeWireSub [] _ = (False, [])

/////////// SplitOutConnectedPairs
// Takes two lists pairing left-side input and output wire IDs with right-side input and output
// connections, and returns two lists of right-side input and output connections with no wires in
// common, with their wire IDs changed to match the wire IDs of the corresponding left-side inputs
// and outputs, a list identifying inputs and outputs that belong to single wires, with the
// input wire ID (in both the left and right sides), the output wire ID (in the left side), and
// the type, and a list of wire ID substitutions identifying wires whose IDs must be changed in
// the right side to match the left side.
SplitOutConnectedPairs :: ![(WireID, Connection)] [(WireID, Connection)]
                            -> ([Connection], [Connection], [(WireID, WireID, WireType)], [WireSub])
SplitOutConnectedPairs inputPairs outputPairs
  = SplitAux inputPairs outputPairs [] [] []
where
  SplitAux [(leftWireID, rightConnect) : inputPairs] outputPairs inputs connectedPairs wireSubs
    | isEmpty outputPairsBack
        = SplitAux inputPairs outputPairs [{rightConnect & cWireID=leftWireID} : inputs] connectedPairs
                   [(rightConnect.cWireID, leftWireID) : wireSubs]
    | otherwise
        = SplitAux inputPairs (outputPairsFront ++ (tl outputPairsBack)) inputs
                   [(leftWireID, fst (hd outputPairsBack), rightConnect.cWireType) : connectedPairs]
                   [(rightConnect.cWireID, leftWireID) : wireSubs]
  where
    (outputPairsFront, outputPairsBack)
      = span (((<>) rightConnect.cWireID) o GetConnectWireID o snd) outputPairs

  SplitAux [] outputPairs inputs connectedPairs wireSubs1
    = (inputs, outputs, connectedPairs, wireSubs2)
  where
    (outputs, wireSubs2) = GetRightsideOutputs outputPairs wireSubs1

/////////// GetRightsideOutputs
// Takes a list pairing left-side output wire IDs with right-side output connections
// and the current list of wire ID substitutions, and returns the list of connections
// with their wire IDs changed to match the left-side wire IDs and the new list of
// wire ID substitutions that must be made in the right-side circuit.
GetRightsideOutputs :: ![(WireID, Connection)] [WireSub] -> ([Connection], [WireSub])
GetRightsideOutputs outputPairs wireSubs = GetOutputsAux outputPairs [] wireSubs
where
  GetOutputsAux [(leftWireID, rightConnect) : outputPairs] outputs wireSubs
    = GetOutputsAux outputPairs [{rightConnect & cWireID=leftWireID} : outputs]
                    [(rightConnect.cWireID, leftWireID) : wireSubs]
  GetOutputsAux [] outputs wireSubs = (outputs, wireSubs)

////////// SplitCircuit
// Takes a wire ID identifying the live wire in the left-side circuit and
// the left-side circuit, and returns the component that has the live
// wire as an output, the part of the left-side that directly or indirectly
// connects down to it, the part of the left-side that directly or
// indirectly connects down from it, and the part of the left-side that
// neither directly nor indirectly connects to it.
// FindRuleOutWire will return a dummy component for ruleComp if the live
// wire has either endpoint in a box (which is not allowed).
SplitCircuit :: WireID Circuit -> (Component, Circuit, Circuit, Circuit)
SplitCircuit wireID ruleCircuit
  = (ruleComp, ruleTop2, ReverseAppend ruleTopMiddle ruleBotMiddle, ruleBot2)
where
  (ruleTop1, ruleComp, ruleBot1) = FindRuleOutWire wireID ruleCircuit
  (ruleTop2, ruleTopMiddle)      = SplitTop (GetInWireIDs ruleComp) ruleTop1
  (ruleBot2, ruleBotMiddle)      = SplitBottom (GetOutWireIDs ruleComp) ruleBot1

////////// FindRuleOutWire
// Takes a wire ID identifying the live wire in the left-side circuit and
// the left-side circuit, and finds the component with the live wire as
// an output.  It doesn't search into boxes, and it checks that the lower
// end of the live wire doesn't connect into any boxes.  If neither end of
// the live wire is in a box, it returns the part of the circuit that is
// above the live wire's component (in reverse order), the component, and
// the part of the circuit that is below the live wire's component.  If an end of the live
// wire is in a box, it returns a dummy component.
FindRuleOutWire :: WireID !Circuit -> (Circuit, Component, Circuit)
FindRuleOutWire wireID circuit = FindRuleOutWireAux [] circuit
where
  FindRuleOutWireAux circuit1 [box=:{spec=Box _, outputs=[_, {cWireID}]} : circuit2]
    | cWireID==wireID = if (FoundInput wireID circuit2)
                           (circuit1, box, circuit2)
                           ([], dummyComponent, [])
    | otherwise       = FindRuleOutWireAux [box : circuit1] circuit2
  FindRuleOutWireAux circuit1 [comp=:{outputs} : circuit2]
    | any (((==) wireID) o GetConnectWireID) outputs
        = if (FoundInput wireID circuit2)
             (circuit1, comp, circuit2)
             ([], dummyComponent, [])
    | otherwise
        = FindRuleOutWireAux [comp : circuit1] circuit2
  FindRuleOutWireAux _ [] = ([], dummyComponent, [])

////////// FoundInput
// Returns true if it finds a component with the specified wire
// as an input, without recursing into any boxes.
FoundInput :: WireID !Circuit -> Bool
FoundInput wireID [{spec=Box _} : circuit]
  = FoundInput wireID circuit
FoundInput wireID [{inputs} : circuit]
  = (any (((==) wireID) o GetConnectWireID) inputs) || (FoundInput wireID circuit)
FoundInput _ [] = False

////////// SplitTop
// Takes a list of wire IDs and a reversed circuit, and returns the part of the
// circuit that directly or indirectly connects down to the wires identified
// by the wire ID list and the part of the circuit that doesn't, both still
// reversed.
SplitTop :: [WireID] !Circuit -> (Circuit, Circuit)
SplitTop wireIDs [comp : circuit]
  | isEmpty connects
      = (topCirc1, [comp : midCirc1])
  | otherwise
      = ([comp : topCirc2], midCirc2)
where
  (connects, newWireIDs) = filterInOut (MemberOf (GetOutWireIDs comp)) wireIDs
  (topCirc1, midCirc1)   = SplitTop wireIDs circuit
  (topCirc2, midCirc2)   = SplitTop ((GetInWireIDs comp) ++ newWireIDs) circuit

SplitTop _ [] = ([], [])

////////// SplitBottom
// Takes a list of wire IDs and a circuit, and returns the part of the
// circuit that directly or indirectly connects down from the wires identified
// by the wire ID list and the part of the circuit that doesn't.
SplitBottom :: [WireID] !Circuit -> (Circuit, Circuit)
SplitBottom wireIDs [comp : circuit]
  | isEmpty connects
      = (botCirc1, [comp : midCirc1])
  | otherwise
      = ([comp : botCirc2], midCirc2)
where
  (connects, newWireIDs) = filterInOut (MemberOf (GetInWireIDs comp)) wireIDs
  (botCirc1, midCirc1)   = SplitBottom wireIDs circuit
  (botCirc2, midCirc2)   = SplitBottom ((GetOutWireIDs comp) ++ newWireIDs) circuit

SplitBottom _ [] = ([], [])

////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////// DoRewrite
// Takes as arguments:
//   - the rewrite rule,
//   - the wire ID of the wire selected as the live wire in the circuit where the rewrite is to be done,
//   - the mouse position that identified the live wire in the circuit,
//   - the circuit,
//   - the circuit's list of wires,
//   - the next unused wire ID for the circuit,
//   - the next unused component ID for the circuit,
//   - the next unused variable number for the circuit,
//   - the list of sorted output position pairs giving the horizontal positions and the connections of
//     the circuit's outputs.
// Returns:
//   - an error value indicating what error, if any, occurred,
//   - the new circuit after the rewrite, if no error occurred,
//   - the new circuit's wires,
//   - the new next unused wire ID for the circuit,
//   - the new next unused component ID for the circuit,
//   - the new next unused variable number for the circuit,
//   - a list of drawing functions to be applied to the window picture,
//   - the new list of output position pairs,
DoRewrite :: !Rewrite WireID Point !Circuit [Wire] WireID ComponentID Int [(Int, Connection)]
              -> (ErrorVal, Circuit, [Wire], WireID, ComponentID, Int, [DrawFunction], [(Int, Connection)])

DoRewrite rewrite selectedWireID selectedPoint circuit1 wires1 nextWireID1 nextCompID1 nextVar1 outputPairs1
  | IsNotError returnCode
      = (returnCode, SubsIntoRuleCircuit newUserVarSubs circuit2, SubsIntoRuleWires newUserVarSubs wires2, nextWireID2,
         nextCompID2, nextVar2, redraws, outputPairs2
        )
  | otherwise
      = (returnCode, [], [], nextWireID1, nextCompID1, nextVar1, [], [])
where
  // newUserVarSubs are substitutions to turn variables into user variables.
  (returnCode, circuit2, wires2, nextWireID2, nextCompID2, nextVar2, redraws, outputPairs2, _, _, _, newUserVarSubs)
    = DoRewrite1 rewrite selectedWireID selectedPoint circuit1 wires1 nextWireID1 nextCompID1 nextVar1 outputPairs1

////////// DoRewrite1
// Takes as arguments:
//   - the rewrite rule,
//   - the wire ID of the wire selected as the live wire in the circuit where the rewrite is to be done,
//   - the mouse position that identified the live wire in the circuit,
//   - the circuit,
//   - the circuit's list of wires,
//   - the next unused wire ID for the circuit,
//   - the next unused component ID for the circuit,
//   - the next unused variable number for the circuit,
//   - the list of sorted output position pairs giving the horizontal positions and the connections of
//     the circuit's outputs.
// Returns:
//   - an error value indicating what error, if any, occurred,
//   - the new circuit after the rewrite, if no error occurred,
//   - the new circuit's wires,
//   - the new next unused wire ID for the circuit,
//   - the new next unused component ID for the circuit,
//   - the new next unused variable number for the circuit,
//   - a list of drawing functions to be applied to the window picture,
//   - the new list of output position pairs,
//   - a list of wire ID substitutions to connect or break wires as a result of the rewrite (only used after recursion
//     into boxes),
//   - a list of IDs identifying wires from the circuit into the rewrite that need to be changed in the
//     circuit to have user-specified types (only used after recursion into boxes).
//   - a list of IDs wires into the circuit from the rewrite that need to be changed in the circuit to
//     have user-specified types (only used after recursion into boxes),
//   - a list of variable substitutions to replace variables in new user-specified types with user variables.
DoRewrite1 :: !Rewrite WireID Point !Circuit [Wire] WireID ComponentID Int [(Int, Connection)]
               -> (ErrorVal, Circuit, [Wire], WireID, ComponentID, Int, [DrawFunction], [(Int, Connection)], [WireSub],
                   [WireID], [WireID], [(Type, Type)])

// brokenWireID=-1 means this is not an expansion
DoRewrite1 rewrite=:{liveWireID, leftComp, leftTop, leftMid, leftBot, rightSide, ruleRect, inConnects, outConnects,
                     connectedPairs, brokenWireConnect={cWireID=(-1)}
                    }
           selectedWireID selectedPoint circuit wires nextWireID nextCompID nextVar outputPairs
  | not (IsNotError returnCode)
      = (returnCode, [], [], nextWireID, nextCompID, nextVar, [], [], [], [], [], [])
  | done
      = (NoError, circuit3A, wires2A, nextWireID2A, nextCompID2A, nextVarA, redrawsA, outputPairs2A, [],
         userWiresFromCirc2A, [], newUserVarSubs1A
        )
  | not goodMatch
      = (BadMatch, [], [], nextWireID, nextCompID, nextVar, [], [], [], [], [], [])
  | not typesMatch
      = (TypeMismatch, [], [], nextWireID, nextCompID, nextVar, [], [], [], [], [], [])
  | any (((flip any) (map ComponentRectangle circTop)) o Overlaps) rightSideRects ||
    any (((flip any) (map ComponentRectangle circMid)) o Overlaps) rightSideRects ||
    any (((flip any) (map ComponentRectangle circBot)) o Overlaps) rightSideRects
      = (NewCompOverlaps, [], [], nextWireID, nextCompID, nextVar, [], [], [], [], [], [])
  | not ((isEmpty circuit2B) || sequentializes)
      = (BadSequent, [], [], nextWireID, nextCompID, nextVar, [], [], [], [], [], [])
  | otherwise
      = (NoError, circuit2B, newWires2B ++ connectedWires2 ++ wires3B, nextWireID2B, nextCompID2B, nextVarB,
         (flatten (map DrawWire connectedWires2)) ++ redraws6B, outputPairs2B, wireChanges,
         userWiresFromCirc4B, userWiresIntoCirc4B, newUserVarSubs5B
        )
where
  // DoRewrite2 will return done=True if the rewrite was performed inside a box in the circuit,
  // but some components not inside the box may still need to have their output types changed to
  // user-specified types after the call.
  (returnCode, done, circuit2A, wires2A, nextWireID2A, nextCompID2A, nextVarA, redrawsA, outputPairs2A, userWiresFromCirc1A,
   newUserVarSubs1A)
    = DoRewrite2 rewrite selectedWireID selectedPoint circuit wires nextWireID nextCompID nextVar outputPairs

  // Changes types to user-specified types, and returns any that it didn't find at this level (called only when
  // DoRewrite2 did the rewrite).
  (circuit3A, _, userWiresFromCirc2A) = SetUserTypeOnOutputs userWiresFromCirc1A circuit2A [] []

  //* The following code is called only if the rewrite wasn't performed by DoRewrite2

  // Removes all matched components from the circuit, and returns the part of the circuit that was above in reverse
  // order (circTop), the part that was below (circBot), and the part that was neither above nor below all the matched
  // components (circMid).  Also returns the enclosing rectangle for the matched part of the circuit, a list of
  // wire ID substitutions pairing wire IDs from the left-side of the rule with wire IDs from the circuit, and a list
  // of drawing functions used to erase the matched components.
  (goodMatch, circTop, circMid, circBot, enclosingRect, wireSubs1, redraws1B)
    = MatchLeftSide leftComp liveWireID leftTop leftMid leftBot selectedWireID circuit

  // Makes sure that all types on the right side have variables that are independent of the variables in the circuit.
  (rightSide1B, inConnects1B, outConnects1B, connectedPairs1B, nextVarB)
    = MakeIndependent rightSide inConnects outConnects connectedPairs nextVar

  // Creates a list pairing input wire IDs from the rewrite rule with the corresponding wires from the circuit
  // (inWireSubs1), a list pairing input wire types from the rule with wire types from the circuit (typePairs1B),
  // a list of drawing functions used to erase the current input wires in the circuit (redraws2B), a list of
  // wire IDs identifying wires in the circuit that need to be changed to have user-specified types because the
  // corresponding wire types in the rule are user-specified, a list of variable substitutions for variables that
  // must become user variables in the circuit (newUserVarSubs1B), and the remaining wire substitutions (wireSubs2)
  // and circuit wires (wires2) (with all input wires to the rewrite removed).
  (wireSubs2, wires2, inWireSubs1, typePairs1B, redraws2B, userWiresFromCirc1B, newUserVarSubs1B)
    = GetOuterWireInfo inConnects1B wireSubs1 wires [] [] redraws1B [] []

  // Creates a list pairing output wire IDs from the rewrite rule with wires from the circuit, adds to the lists
  // of type pairs and drawing functions, creates another list of wires in the circuit that need to have
  // user-specified types (these ones below the rewrite rather than above it), adds to the list of variable
  // substitutions for variables that must become user variables, and returns the remaining wire substitutions
  // (wireSubs3) and circuit wires (wires3).
  (wireSubs3, wires3, outWireSubs1, typePairs2B, redraws3B, userWiresIntoCirc1B, newUserVarSubs2B)
    = GetOuterWireInfo outConnects1B wireSubs2 wires2 [] typePairs1B redraws2B [] newUserVarSubs1B

  // The remaining outer wires to the rewrite are wires that will become connected in the rewrite - this
  // removes these wires from the circuit's wires (wires4) and from the wire substitutions (wireSubs4),
  // changes wire IDs on the inputs of components in the middle and bottom parts of the circuit to connect them
  // (circMid2, circBot2), adds more type pairs (typePairs3B) and drawing functions to erase these wires (redraws4B),
  // changes wire IDs as necessary in the output position pairs used to keep track of the relative positions of outputs
  // in the circuit (outputPairs2B), creates a list of pairs of wire IDs for wire IDs that could not be changed at this
  // level in the circuit (wireChanges), creates two lists of wires whose types should become user-specified
  // (userWiresFromCirc2B and userWiresIntoCirc2B), returns a list of the newly connected wires, with their wire IDs
  // set to the uppermost of the two wires that were connected, their types made user-specified if necessary, and their
  // endpoints adjusted, and adds to the list of variable substitutions for variables that must become user variables.
  //*^* VVV Note that this may not work in GILL (bottom connections may be outside a box)
  (wireSubs4, wires4, circMid2, circBot2, typePairs3B, redraws4B, outputPairs2B, wireChanges,
   userWiresFromCirc2B, userWiresIntoCirc2B, connectedWires1, newUserVarSubs3B)
    = ConnectWires connectedPairs1B wireSubs3 wires3 circMid circBot typePairs2B redraws3B outputPairs []
                   userWiresFromCirc1B userWiresIntoCirc1B [] newUserVarSubs2B

  // The remaining wire substitutions (wireSubs4) identify wires that are internal to the rewrite.  These are
  // removed from the circuit's wires (to produce wires5), and more drawing functions are added to erase them
  // (redraws5B).
  (oldWires, wires5) = filterInOut ((MemberOf (map snd wireSubs4)) o GetWireID) wires4
  redraws5B = (flatten (map DrawWire oldWires)) ++ redraws4B

  // If any of the wires internal to the rewrite in the circuit had a user-specified type, then all the rewrite's
  // input and output wires must have user-specified types after the rewrite.
  (userWiresFromCirc3B, userWiresIntoCirc3B, inWireSubs2, outWireSubs2, connectedWires2, newUserVarSubs4B)
    = if (any (IsUserType o GetWireType) oldWires)
         (allInWireIDs, allOutWireIDs, map SetUserTypeOnWireSub inWireSubs1, map SetUserTypeOnWireSub outWireSubs1,
          map SetUserTypeOnWire connectedWires1, MakeUserVarSubsFromTypes allOuterTypes newUserVarSubs3B
         )
         (userWiresFromCirc2B, userWiresIntoCirc2B, inWireSubs1, outWireSubs1, connectedWires1, newUserVarSubs3B)
  allInWireIDs     = connectedWireIDs ++ (map (GetWireID o snd) inWireSubs1)
  allOutWireIDs    = connectedWireIDs ++ (map (GetWireID o snd) outWireSubs1)
  connectedWireIDs = map GetWireID connectedWires1
  allOuterTypes    = (map (GetType o GetWireType) connectedWires1) ++ (map (GetType o GetWireType o snd) inWireSubs1) ++
                     (map (GetType o GetWireType o snd) outWireSubs1)
  SetUserTypeOnWireSub (id, wire)    = (id, SetUserTypeOnWire wire)
  SetUserTypeOnWire wire=:{wireType} = {wire & wireType=MakeUserType wireType}

  // The right-side of the rule will be moved by ruleOffsetB before being inserted into the circuit.
  ruleOffsetB = VectorDifference (RectangleCentre enclosingRect) (RectangleCentre ruleRect)

  // ModifyRule changes the positions and component IDs of all components in the right side, creates new outer wires
  // from inWireSubs2 and outWireSubs2 (with their endpoints changed) and adds them to the circuit's wires (to give
  // wires3B), creates new internal wires for the rewrite (newWires1B), returns the next free wire ID (nextWireID2B)
  // and component ID (nextCompID2B), and adds more drawing functions to draw the new components and wires (redraws6B).
  (rightSide2B, wires3B, newWires1B, nextWireID2B, nextCompID2B, _, redraws6B)
    = ModifyRule rightSide1B ruleOffsetB wires5 [] inWireSubs2 outWireSubs2 [] nextWireID nextCompID [] redraws5B

  // SubsIntoRule matches the type pairs and makes variable substitutions into the right side of the rule and the
  // new wires.  It also adds to the list of variables that must become user variables (any that occur in types
  // that are being substituted for user variables in the right side).
  (typesMatch, rightSide3B, newWires2B, newUserVarSubs5B) = SubsIntoRule rightSide2B newWires1B typePairs3B newUserVarSubs4B

  // A list of the enclosing rectangles for every component in the right side is used to test for overlaps.
  rightSideRects = map ComponentRectangle rightSide2B

  // User-specified types are set in the circuit (producing circTop2, circMid4, and circBot3) and those that couldn't
  // be done at this level are returned (userWiresFromCirc4B and userWiresIntoCirc4B).
  (circMid3, circTop2, userWiresFromCirc4B) = SetUserTypeOnOutputs userWiresFromCirc3B circMid2 circTop []
  (circMid4, circBot3, userWiresIntoCirc4B) = SetUserTypeOnInputs  userWiresIntoCirc3B circMid3 circBot2 []

  // The right side is inserted into the middle part of the circuit, and the top and bottom are glued back on.
  circuit2B = ReverseAppend circTop2 ((InsertAll rightSide3B circMid4) ++ circBot3)

  // Checks that the circuit still sequentializes after the rewrite
  (sequentializes, _)                = FILLSequentialize circuit2B

// This is an expansion
DoRewrite1 rewrite=:{liveWireID, rightSide, ruleRect, inConnects=[{cWireType=ruleWireType1}],
                     brokenWireConnect={cWireID=brokenWireID}
                    }
           selectedWireID selectedPoint circuit wires nextWireID nextCompID nextVar outputPairs
  | not (IsNotError returnCode)
      = (returnCode, [], [], nextWireID, nextCompID, nextVar, [], [], [], [], [], [])
  | done
      = (NoError, circuit2A, wires2A, nextWireID2A, nextCompID2A, nextVar2A, redrawsA, outputPairsA, [], [], [], [])
  | not typesMatch
      = (TypeMismatch, [], [], nextWireID, nextCompID, nextVar, [], [], [], [], [], [])
  | any (((flip any) (map ComponentRectangle circuit)) o Overlaps) rightSideRects
      = (NewCompOverlaps, [], [], nextWireID, nextCompID, nextVar, [], [], [], [], [], [])
  | IsDummy circComp   // The live wire is an input to this level of the circuit (so no component has it as an output wire)
      = if sequentializesB
           (if inputChangedB  // If false, the wire ID on the lower end of the broken wire still needs to be changed.
               (NoError, circuit2B, newWires2 ++ wires3, nextWireID2, nextCompID2, nextVar3, redraws, outputPairsB,
                [], [], [], []
               )
               (NoError, circuit2B, newWires2 ++ wires3, nextWireID2, nextCompID2, nextVar3, redraws, outputPairsB,
                [(selectedWireID, nextWireID)], [], [], []
               )
           )
           (BadSequent, [], [], nextWireID, nextCompID, nextVar, [], [], [], [], [], [])
  | otherwise
      = if sequentializesC
           (if inputChangedC  // If false, the wire ID on the lower end of the broken wire still needs to be changed.
               (NoError, circuit2C, newWires2 ++ wires3, nextWireID2, nextCompID2, nextVar3, redraws, outputPairsC,
                [], [], [], []
               )
               (NoError, circuit2C, newWires2 ++ wires3, nextWireID2, nextCompID2, nextVar3, redraws, outputPairsC,
                [(selectedWireID, nextWireID)], [], [], []
               )
           )
           (BadSequent, [], [], nextWireID, nextCompID, nextVar, [], [], [], [], [], [])
where
  // Will return done=True if the rewrite was done in a box, otherwise does nothing.
  (returnCode, done, circuit2A, wires2A, nextWireID2A, nextCompID2A, nextVar2A, redrawsA, outputPairsA, _, _)
    = DoRewrite2 rewrite selectedWireID selectedPoint circuit wires nextWireID nextCompID nextVar outputPairs

  //* The following code is called only if the rewrite wasn't performed by DoRewrite2

  // Finds the component with the live wire (the wire to be expanded) as an output wire.  If circComp is a
  // dummy component, the component wasn't found (it's not at this level), and the live wire must be an input
  // wire for this level of the circuit.
  (circTop, circComp, circBot1) = FindOutWire selectedWireID circuit

  // This finds the live wire in the circuit.
  (wire, wires2) = RemoveAndReturn (((==) selectedWireID) o GetWireID) wires

  // This gives the offset that determines the right side's new position before insertion into the circuit
  // (centred on the live wire).
  ruleOffset = VectorDifference (RectangleCentre wire.wireLine) (RectangleCentre ruleRect)

  // These make sure that the variables in the right side are independent of the variables in the circuit.
  (rightSide2, typeSubs2, nextVar2) = MakeIndependentCircuit rightSide [] nextVar
  (ruleWireType2, _, nextVar3)      = MakeIndependentWireType ruleWireType1 typeSubs2 nextVar2

  // This changes the positions and component IDs of components in the right side (rightSide3), adds the rewrite's
  // external wires (with the endpoints changed) to the circuit's other wires (wires3), creates new internal wires
  // for the rewrites (newWires1), returns the new next free wire ID (nextWireID2) and component ID (nextCompID2),
  // and adds drawing functions to draw the new components and wires (a list containing functions to erase the live
  // wire was passed as an argument).
  (rightSide3, wires3, newWires1, nextWireID2, nextCompID2, _, redraws)
    = ModifyRule rightSide2 ruleOffset wires2 [] [(liveWireID, wire)] [(brokenWireID, {wire & wireID=nextWireID})] []
                 (nextWireID+1) nextCompID [] (DrawWire wire)

  // SubsIntoRule matches the rule's wire type with the circuit's wire type and makes variable substitutions in the
  // right side (rightSide4 and newWires2).
  (typesMatch, rightSide4, newWires2, _) = SubsIntoRule rightSide3 newWires1 [(ruleWireType2, wire.wireType)] []

  // This is used to check for components that overlap other components in the circuit
  rightSideRects = map ComponentRectangle rightSide3


  //* The following are called only when the live wire is an input wire to this level of the circuit:

  // This changes the wire ID on the input of a component in the circuit and as necessary in the list of output
  // position pairs (used to keep track of the order of the circuit's output wires) to a new wire ID to "break" the
  // live wire.
  //*^* VVV Note that this may not work in GILL (bottom connection may be outside a box)
  (inputChangedB, _, circuit2, outputPairsB)
    = ChangeInputIDInRewrite selectedWireID (snd wire.wireLine) nextWireID [] circuit outputPairs
  // Adds the right side of the rewrite to the circuit
  circuit2B = ReverseAppend rightSide4 circuit2
  // Checks that the new circuit sequentializes
  (sequentializesB, _) = FILLSequentialize circuit2B

  //* These are called only when the live wire is *not* an input wire to this level of the circuit (otherwise
  //* they are similar to the above):

  //*^* VVV Note that this may not work in GILL (bottom connection may be outside a box)
  (inputChangedC, _, circBot2, outputPairsC)
    = ChangeInputIDInRewrite selectedWireID (snd wire.wireLine) nextWireID [] circBot1 outputPairs
  circuit2C = ReverseAppend circTop [circComp : (ReverseAppend rightSide4 circBot2)]
  (sequentializesC, _) = FILLSequentialize circuit2C

////////// DoRewrite2
// Takes as arguments:
//   - the rewrite rule,
//   - the wire ID of the wire selected as the live wire in the circuit where the rewrite is to be done,
//   - the mouse position that identified the live wire in the circuit,
//   - the circuit,
//   - the circuit's list of wires,
//   - the next unused wire ID for the circuit,
//   - the next unused component ID for the circuit,
//   - the next unused variable number for the circuit,
//   - the list of sorted output position pairs giving the horizontal positions and the connections of
//     the circuit's outputs.
// Returns:
//   - an error value indicating what error, if any, occurred,
//   - a boolean indicating if the rewrite was performed in one of the boxes in the circuit,
//   - the new circuit after the rewrite, if no error occurred,
//   - the new circuit's wires,
//   - the new next unused wire ID for the circuit,
//   - the new next unused component ID for the circuit,
//   - the new next unused variable number for the circuit,
//   - a list of drawing functions to be applied to the window picture,
//   - the new list of output position pairs,
//   - a list of IDs identifying wires from the circuit into the rewrite that need to be changed in the
//     circuit to have user-specified types (only used after recursion into boxes),
//   - a list of variable substitutions for variables that must become user variables.
//
// Assumptions:  Both endpoints of the live wire are on the outermost level of the rule (although a wire that is
//               external to the rule may exit a box in the circuit) - selectedPoint is used to determine the
//               level at which the rewrite is to be performed.
//
//*^*            NOTE that DoRewrite2 might be modified to do some of the work being done by MatchLeftSide - but
//*^*            DoRewrite2 would have to split the circuit at the component with the live wire as an *input*, not
//*^*            an output.

DoRewrite2
 :: Rewrite WireID Point !Circuit [Wire] WireID ComponentID Int [(Int, Connection)]
     -> (ErrorVal, Bool, Circuit, [Wire], WireID, ComponentID, Int, [DrawFunction], [(Int, Connection)], [WireID],
         [(Type, Type)])
DoRewrite2 rewrite liveWireID selectedPoint [box=:{spec=Box boxCircuit, inputs=[inC], pos=RCT boxRect} : circuit]
           wires nextWireID nextCompID nextVar outputPairs
  | IsInRectangle selectedPoint boxRect  // Rewrite will be done inside the box
      = if (IsNotError returnCode1)
           (if (all ((Encloses boxRect) o ComponentRectangle) newBoxCirc)  // Check if the new box circuit is in the box
               (NoError, True, [{box & spec=Box newBoxCirc, inputs=[newInC2]} : circuit], wires2A, nextWireID2A,
                nextCompID2A, nextVar2A, redraws1, outputPairsA, userWiresFromCircA, newUserVarSubsA
               )
               (NewCompOverlaps, False, [], [], nextWireID, nextCompID, nextVar, [], [], [], [])
           )
           (returnCode1, False, [], [], nextWireID, nextCompID, nextVar, [], [], [], [])
  | otherwise
      = (returnCode2, done, [box : newCirc], wires2B, nextWireID2B, nextCompID2B, nextVar2B, redraws2, outputPairsB,
         userWiresFromCircB, newUserVarSubsB
        )
where
  // Calls DoRewrite1 to do the rewrite in the box.
  (returnCode1, newBoxCirc, wires2A, nextWireID2A, nextCompID2A, nextVar2A, redraws1, outputPairsA, wireChanges,
   userWiresFromCircA, userWiresIntoCircA, newUserVarSubsA)
    = DoRewrite1 rewrite liveWireID selectedPoint boxCircuit wires nextWireID nextCompID nextVar outputPairs

  // Changes the box's internal input wire ID and wire type if necessary
  //*^* VVV In FILL, the only wire change and output type change possible is to the box's internal input
  newInC1 = if (isEmpty wireChanges) inC {inC & cWireID=snd (hd wireChanges)}
  newInC2 = if (isEmpty userWiresIntoCircA) newInC1 {newInC1 & cWireType=MakeUserType newInC1.cWireType}

  (returnCode2, done, newCirc, wires2B, nextWireID2B, nextCompID2B, nextVar2B, redraws2, outputPairsB, userWiresFromCircB,
   newUserVarSubsB)
    = DoRewrite2 rewrite liveWireID selectedPoint circuit wires nextWireID nextCompID nextVar outputPairs

DoRewrite2 rewrite liveWireID selectedPoint [comp : circuit] wires nextWireID nextCompID nextVar outputPairs
  = (returnCode, done, [comp : newCirc], wires2, nextWireID2, nextCompID2, nextVar2, redraws, outputPairs2,
     userWiresFromCirc, newUserVarSubs
    )
where
  (returnCode, done, newCirc, wires2, nextWireID2, nextCompID2, nextVar2, redraws, outputPairs2, userWiresFromCirc,
   newUserVarSubs)
    = DoRewrite2 rewrite liveWireID selectedPoint circuit wires nextWireID nextCompID nextVar outputPairs

DoRewrite2 _ _ _ [] wires nextWireID nextCompID nextVar outputPairs
  = (NoError, False, [], wires, nextWireID, nextCompID, nextVar, [], outputPairs, [], [])

///////// MakeIndependent
// Takes as arguments the right side's circuit, input connections, output connections,
// and connected wire information, and the next free variable number in the circuit,
// and changes all variables and user variables in the right side to variables that
// are not duplicated in the circuit.
MakeIndependent :: Circuit [Connection] [Connection] [(WireID, WireID, WireType)] Int
                    -> (Circuit, [Connection], [Connection], [(WireID, WireID, WireType)], Int)
MakeIndependent rightSide1 inConnects1 outConnects1 connectedPairs1 nextVar1
  = (rightSide2, inConnects2, outConnects2, connectedPairs2, nextVar5)
where
  // The second argument and the second return value for each of these functions is a list
  // of substitution pairs giving new variables numbers that have already been assigned to
  // variables and user variables in the circuit.
  (rightSide2, typeSubs1, nextVar2)   = MakeIndependentCircuit rightSide1 [] nextVar1
  (inConnects2, typeSubs2, nextVar3)  = MakeIndependentConnects inConnects1 typeSubs1 nextVar2
  (outConnects2, typeSubs3, nextVar4) = MakeIndependentConnects outConnects1 typeSubs2 nextVar3
  (connectedPairs2, _, nextVar5)      = MakeIndependentPairs connectedPairs1 typeSubs3 nextVar4

///////// MakeIndependentCircuit
// See MakeIndependent above.
MakeIndependentCircuit :: Circuit [(Type, Int)] Int -> (Circuit, [(Type, Int)], Int)

MakeIndependentCircuit [box=:{spec=Box boxCircuit, inputs, outputs} : circuit] typeSubs1 nextVar1
  = ([{box & spec=Box newBoxCirc, inputs=newInputs, outputs=newOutputs} : newCircuit], typeSubs5, nextVar5)
where
  (newInputs, typeSubs2, nextVar2)  = MakeIndependentConnects inputs typeSubs1 nextVar1
  (newOutputs, typeSubs3, nextVar3) = MakeIndependentConnects outputs typeSubs2 nextVar2
  (newBoxCirc, typeSubs4, nextVar4) = MakeIndependentCircuit boxCircuit typeSubs3 nextVar3
  (newCircuit, typeSubs5, nextVar5) = MakeIndependentCircuit circuit typeSubs4 nextVar4

MakeIndependentCircuit [comp=:{inputs, outputs} : circuit] typeSubs1 nextVar1
  = ([{comp & inputs=newInputs, outputs=newOutputs} : newCircuit], typeSubs4, nextVar4)
where
  (newInputs, typeSubs2, nextVar2)  = MakeIndependentConnects inputs typeSubs1 nextVar1
  (newOutputs, typeSubs3, nextVar3) = MakeIndependentConnects outputs typeSubs2 nextVar2
  (newCircuit, typeSubs4, nextVar4) = MakeIndependentCircuit circuit typeSubs3 nextVar3

MakeIndependentCircuit [] typeSubs nextVar = ([], typeSubs, nextVar)

///////// MakeIndependentConnects
// See MakeIndependent above.
MakeIndependentConnects :: [Connection] [(Type, Int)] Int -> ([Connection], [(Type, Int)], Int)

MakeIndependentConnects [connect=:{cWireType} : connects] typeSubs1 nextVar1
  = ([{connect & cWireType = newWireType} : newConnects], typeSubs3, nextVar3)
where
  (newWireType, typeSubs2, nextVar2) = MakeIndependentWireType cWireType typeSubs1 nextVar1
  (newConnects, typeSubs3, nextVar3) = MakeIndependentConnects connects typeSubs2 nextVar2

MakeIndependentConnects [] typeSubs maxVar = ([], typeSubs, maxVar)

///////// MakeIndependentPairs
// See MakeIndependent above.
MakeIndependentPairs :: [(WireID, WireID, WireType)] [(Type, Int)] Int -> ([(WireID, WireID, WireType)], [(Type, Int)], Int)

MakeIndependentPairs [(wireID1, wireID2, wireType) : pairs] typeSubs1 nextVar1
  = ([(wireID1, wireID2, newWireType) : newPairs], typeSubs3, nextVar3)
where
  (newWireType, typeSubs2, nextVar2) = MakeIndependentWireType wireType typeSubs1 nextVar1
  (newPairs, typeSubs3, nextVar3)    = MakeIndependentPairs pairs typeSubs2 nextVar2

MakeIndependentPairs [] typeSubs maxVar = ([], typeSubs, maxVar)

///////// MakeIndependentWireType
// See MakeIndependent above.
MakeIndependentWireType :: WireType [(Type, Int)] Int -> (WireType, [(Type, Int)], Int)

MakeIndependentWireType (Free type) typeSubs1 nextVar1
  = (Free newType, typeSubs2, nextVar2)
where
  (newType, typeSubs2, nextVar2) = ResetTypeVars type typeSubs1 nextVar1

MakeIndependentWireType (User type) typeSubs1 nextVar1
  = (User newType, typeSubs2, nextVar2)
where
  (newType, typeSubs2, nextVar2) = ResetTypeVars type typeSubs1 nextVar1

////////// ResetTypeVars
// Takes a type, a list of substitution pairs giving variable-number
// substitutions for variables and user variables already
// encountered, and the next free variable number, and returns the
// type with all variables and user variables replaced with new
// variables and user variables, a new list giving variable-number
// substitutions for variables and user variables (with any new ones
// added), and the new next free variable number.
ResetTypeVars :: !Type [(Type, Int)] Int -> (Type, [(Type, Int)], Int)

ResetTypeVars (Var x) subs nextVar
  | isEmpty backSubs = (Var nextVar, [(Var x, nextVar) : subs], nextVar+1)
  | otherwise        = (Var (snd (hd backSubs)), subs, nextVar)
where
  backSubs = dropWhile (not o (EqualTypes (Var x)) o fst) subs

ResetTypeVars (UserVar1 s) subs nextVar
  | isEmpty backSubs = (UserVar2 nextVar, [(UserVar1 s, nextVar) : subs], nextVar+1)
  | otherwise        = (UserVar2 (snd (hd backSubs)), subs, nextVar)
where
  backSubs = dropWhile (not o (EqualTypes (UserVar1 s)) o fst) subs

ResetTypeVars (UserVar2 n) subs nextVar
  | isEmpty backSubs = (UserVar2 nextVar, [(UserVar2 n, nextVar) : subs], nextVar+1)
  | otherwise        = (UserVar2 (snd (hd backSubs)), subs, nextVar)
where
  backSubs = dropWhile (not o (EqualTypes (UserVar2 n)) o fst) subs

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
  (types2, subs2, nextVar2) = ResetTypesVars types1 subs1 nextVar1

ResetTypeVars type subs nextVar = (type, subs, nextVar)

///////// ResetTypesVars
ResetTypesVars :: [Type] [(Type, Int)] Int -> ([Type], [(Type, Int)], Int)
ResetTypesVars [type : types] subs1 nextVar1
  = ([newType : newTypes], subs3, nextVar3)
where
  (newType, subs2, nextVar2)  = ResetTypeVars type subs1 nextVar1
  (newTypes, subs3, nextVar3) = ResetTypesVars types subs2 nextVar2

ResetTypesVars [] subs nextVar = ([], subs, nextVar)

///////// GetOuterWireInfo
// Takes as arguments:
//   - a list of external wire connections in the rewrite rule,
//   - a list of wire ID substititions pairing wire IDs from the rule with wire IDs in the circuit,
//   - a list of circuit wires,
//   - a list of the substitutions pairing wire IDs in the rule with wires from the circuit made so far,
//   - a list of the substitutions pairing wire types in the rule with wire types from the circuit made so far,
//   - a list of the drawing functions made so far,
//   - a list of the wire IDs identifying wires in the circuit that must be changed to have user-specified types
//     that have been found so far,
//   - a list of variable substitutions for all new user variables so far.
// Returns:
//   - a list of wire ID substititions with the substitutions for the outer wires in the first argument removed,
//   - a list of circuit wires with the wires for the outer wires in the first argument removed,
//   - a list of substitutions pairing outer wire IDs in the rule (given by the first argument) with the corresponding
//     wires in the circuit,
//   - a list of substitutions pairing wire types from the rule with wire types from the circuit,
//   - a list of drawing functions (now including functions to erase the outer wires from the first argument),
//   - a list of wire IDs identifying wires in the circuit that must be changed to have user-specified types,
//   - a list of variable substitutions for all new user variables so far.
GetOuterWireInfo
 :: ![Connection] [WireSub] [Wire] [(WireID, Wire)] [(WireType, WireType)] [DrawFunction] [WireID] [(Type, Type)]
     -> ([WireSub], [Wire], [(WireID, Wire)], [(WireType, WireType)], [DrawFunction], [WireID], [(Type, Type)])

// The outer wire in the rule has a user-specified type, so the corresponding wire in the circuit must also.
GetOuterWireInfo [{cWireID=ruleWireID, cWireType=User ruleType} : connects] wireSubs1 wires1 outerWireSubs typePairs redraws
                 newCircUserWires newUserVarSubs
  | IsUserType wire.wireType
     = GetOuterWireInfo connects wireSubs2 wires2 [(ruleWireID, wire) : outerWireSubs]
                        [(User ruleType, wire.wireType) : typePairs] ((DrawWire wire) ++ redraws) newCircUserWires
                        newUserVarSubs
  | otherwise
     = GetOuterWireInfo connects wireSubs2 wires2
                        [(ruleWireID, {wire & wireType=MakeUserType wire.wireType}) : outerWireSubs]
                        [(User ruleType, wire.wireType) : typePairs] ((DrawWire wire) ++ redraws)
                        [circWireID : newCircUserWires] (MakeUserVarSubsFromType (GetType wire.wireType) newUserVarSubs)
where
  ((_, circWireID), wireSubs2) = RemoveAndReturn (((==) ruleWireID) o fst) wireSubs1
  (wire, wires2)               = RemoveAndReturn (((==) circWireID) o GetWireID) wires1

GetOuterWireInfo [{cWireID=ruleWireID, cWireType=ruleWireType} : connects] wireSubs1 wires1 outerWireSubs typePairs redraws
                 newCircUserWires newUserVarSubs
  = GetOuterWireInfo connects wireSubs2 wires2 [(ruleWireID, wire) : outerWireSubs]
                     [(ruleWireType, wire.wireType) : typePairs] ((DrawWire wire) ++ redraws) newCircUserWires
                     newUserVarSubs
where
  ((_, circWireID), wireSubs2) = RemoveAndReturn (((==) ruleWireID) o fst) wireSubs1
  (wire, wires2)               = RemoveAndReturn (((==) circWireID) o GetWireID) wires1

GetOuterWireInfo [] wireSubs wires outerWireSubs typePairs redraws newCircUserWires newUserVarSubs
  = (wireSubs, wires, outerWireSubs, typePairs, redraws, newCircUserWires, newUserVarSubs)

////////// ConnectWires
// Takes as arguments:
//   - a list identifying connected wires in the rule, with the wire ID of the top wire in the left side (also the
//     connected wire in the right side), the wire ID of the bottom wire in the left side, and the wire type,
//   - a list pairing wire IDs from the left side of the rule with wire IDs in the circuit,
//   - a list of circuit wires,
//   - the middle part of this level of the circuit (parallel to the rewrite),
//   - the bottom part of this level of the circuit (below the rewrite),
//   - a list of all the substitutions so far pairing wire types from the rule with wire types from the circuit,
//   - a list of all the drawing functions so far,
//   - a list of output position pairs (used to keep track of the positions of the circuit's output wires),
//   - a list pairing old input wire IDs in the circuit with the new ones necessary to connect wires (this
//     only gives the substitutions for wire IDs that couldn't be changed at the current level of the circuit),
//   - a list of wire IDs identifying wires from the circuit into the rewrite that need to have their types be made
//     user-specified,
//   - a list of wire IDs identifying wires into the circuit from the rewrite that need to have their types be made
//     user-specified,
//   - a list of all the new connected wires in the circuit created so far, with the wire IDs of the top wire of
//     each pair of wires that were connected, their endpoints changed, and their types made user-specified if
//     necessary,
//   - a list of variable substitutions for all variables that must become user variables that have been found
//     so far.
// Returns:
//   - a list pairing left-side wire IDs with circuit wire IDs, with the ones corresponding to connected wires
//     removed,
//   - a list of circuit wires, with the ones that have been connected removed,
//   - the new middle of the circuit (with input wire IDs changed as necessary),
//   - the new bottom of the circuit (with input wire IDs changed as necessary),
//   - the new list wire type substitutions,
//   - the new list of drawing functions, with functions added to erase wires that are being connected,
//   - the new list of output position pairs, with wire IDs changed as necessary,
//   - a list pairing old input wire IDs in the circuit with the new ones necessary to connect wires (this
//     only gives the substitutions for wire IDs that couldn't be changed at the current level of the circuit),
//   - a list of wire IDs identifying wires into the circuit from rewrite that need to have their types be made
//     user-specified,
//   - a list of all the new connected wires in the circuit created so far, with the wire IDs of the top wire of
//     each pair of wires that were connected, their endpoints changed, and their types made user-specified if
//     necessary,
//   - a list of variable substitutions for all variables that must become user variables that have been found
//     so far.
ConnectWires :: ![(WireID, WireID, WireType)] [WireSub] [Wire] Circuit Circuit [(WireType, WireType)] [DrawFunction]
                [(Int, Connection)] [WireSub] [WireID] [WireID] [Wire] [(Type, Type)]
                 -> ([WireSub], [Wire], Circuit, Circuit, [(WireType, WireType)], [DrawFunction], [(Int, Connection)],
                     [WireSub], [WireID], [WireID], [Wire], [(Type, Type)])

// The rule's wire type is user-specified, so the new connected wire in the circuit must have a user-specified type.
ConnectWires [(ruleWireID1, ruleWireID2, User ruleType) : connectedPairs] wireSubs1 wires1 circMid1 circBot1 typePairs
             redraws outputPairs wireChanges userWiresFromCirc1 userWiresIntoCirc1 connectedWires newUserVarSubs1
  = ConnectWires connectedPairs wireSubs3 wires3 circMid2 circBot2
                 [(User ruleType, newWireType1), (User ruleType, newWireType2) : typePairs]
                 ((DrawWire wire1) ++ (DrawWire wire2) ++ redraws) outputPairs2 newWireChanges
                 userWiresFromCirc2 userWiresIntoCirc2 [newWire : connectedWires] newUserVarSubs2
where
  ((_,circWireID1), wireSubs2) = RemoveAndReturn (((==) ruleWireID1) o fst) wireSubs1
  ((_,circWireID2), wireSubs3) = RemoveAndReturn (((==) ruleWireID2) o fst) wireSubs2
  (wire1=:{wireLine=(start,_)}, wires2) = RemoveAndReturn (((==) circWireID1) o GetWireID) wires1
  (wire2=:{wireLine=(_,end)}, wires3)   = RemoveAndReturn (((==) circWireID2) o GetWireID) wires2
  newWireType1   = MakeUserType wire1.wireType
  newWireType2   = MakeUserType wire2.wireType
  newWire        = {wireID=circWireID1, wireLine=(start,end), wireType=newWireType1}
  newWireChanges = if inputChanged wireChanges [(circWireID2, circWireID1) : wireChanges]
  (inputChanged, circMid2, circBot2, outputPairs2)
    = ChangeInputIDInRewrite circWireID2 end circWireID1 circMid1 circBot1 outputPairs
  (userWiresFromCirc2, newUserVarSubs2)
    = if (IsUserType wire1.wireType)
         (userWiresFromCirc1, newUserVarSubs1)
         ([circWireID1 : userWiresFromCirc1], MakeUserVarSubsFromType (GetType wire1.wireType) newUserVarSubs1)
  userWiresIntoCirc2 = if (IsUserType wire2.wireType)
                          userWiresIntoCirc1
                          [circWireID1 : userWiresIntoCirc1]

ConnectWires [(ruleWireID1, ruleWireID2, ruleWireType) : connectedPairs] wireSubs1 wires1 circMid1 circBot1 typePairs
             redraws outputPairs wireChanges userWiresFromCirc1 userWiresIntoCirc1 connectedWires newUserVarSubs
  = ConnectWires connectedPairs wireSubs3 wires3 circMid2 circBot2
                 [(ruleWireType, wire1.wireType), (ruleWireType, wire2.wireType) : typePairs]
                 ((DrawWire wire1) ++ (DrawWire wire2) ++ redraws) outputPairs2 newWireChanges
                 userWiresFromCirc2 userWiresIntoCirc2 [newWire : connectedWires] newUserVarSubs
where
  ((_,circWireID1), wireSubs2) = RemoveAndReturn (((==) ruleWireID1) o fst) wireSubs1
  ((_,circWireID2), wireSubs3) = RemoveAndReturn (((==) ruleWireID2) o fst) wireSubs2
  (wire1=:{wireLine=(start,_)}, wires2) = RemoveAndReturn (((==) circWireID1) o GetWireID) wires1
  (wire2=:{wireLine=(_,end)}, wires3)   = RemoveAndReturn (((==) circWireID2) o GetWireID) wires2

  (inputChanged, circMid2, circBot2, outputPairs2)
    = ChangeInputIDInRewrite circWireID2 end circWireID1 circMid1 circBot1 outputPairs
  newWireChanges = if inputChanged wireChanges [(circWireID2, circWireID1) : wireChanges]

  // If one of the circuit's wires being connected has a user-specified type and the other doesn't,
  // then the one that doesn't must be changed.
  (userWiresFromCirc2, userWiresIntoCirc2, newWire)
    = if (IsUserType wire1.wireType)
         (if (IsUserType wire2.wireType)
             (userWiresFromCirc1, userWiresIntoCirc1, {wireID=circWireID1, wireLine=(start,end), wireType=wire1.wireType})
             (userWiresFromCirc1, [circWireID1 : userWiresIntoCirc1],
              {wireID=circWireID1, wireLine=(start,end), wireType=wire1.wireType}
             )
         )
         (if (IsUserType wire2.wireType)
             ([circWireID1 : userWiresFromCirc1], userWiresIntoCirc1,
              {wireID=circWireID1, wireLine=(start,end), wireType=wire2.wireType}
             )
             (userWiresFromCirc1, userWiresIntoCirc1, {wireID=circWireID1, wireLine=(start,end), wireType=wire2.wireType})
         )

ConnectWires [] wireSubs wires circMid circBot typePairs redraws outputPairs wireChanges
             userWiresFromCirc userWiresIntoCirc newConnectedWires newUserVarSubs
  = (wireSubs, wires, circMid, circBot, typePairs, redraws, outputPairs, wireChanges, userWiresFromCirc, userWiresIntoCirc,
     newConnectedWires, newUserVarSubs
    )

////////// ChangeInputIDInRewrite
// Takes as arguments:
//   - the old wire ID,
//   - the endpoint position at which the input wire ID will be found,
//   - the new wire ID,
//   - the middle part of the circuit,
//   - the bottom part of the circuit,
//   - a list of output position pairs for the circuit.
// Returns:
//   - True if it found the wire ID to be changed, False otherwise,
//   - the middle part of the circuit, with the wire ID changed if found,
//   - the top part of the circuit, with the wire ID changed if found,
//   - the output position pairs for the circuit, with the wire ID changed if found.
ChangeInputIDInRewrite :: WireID Point WireID !Circuit Circuit [(Int, Connection)]
                           -> (Bool, Circuit, Circuit, [(Int, Connection)])
ChangeInputIDInRewrite oldWireID endPoint newWireID [box=:{spec=Box boxCirc, pos=RCT boxRect} : circMid] circBot outputPairs
  | IsInRectangle endPoint boxRect = (inputChangedA, [{box & spec=Box newBoxCirc} : circMid], circBot, outputPairsA)
  | otherwise                      = (inputChangedB, [box : newCircMid], newCircBot, outputPairsB)
where
  (inputChangedA, _, newBoxCirc, outputPairsA)
    = ChangeInputIDInRewrite oldWireID endPoint newWireID [] boxCirc outputPairs
  (inputChangedB, newCircMid, newCircBot, outputPairsB)
    = ChangeInputIDInRewrite oldWireID endPoint newWireID circMid circBot outputPairs

ChangeInputIDInRewrite oldWireID endPoint newWireID [comp=:{spec=EndTerminal, inputs=[inC]} : circMid] circBot outputPairs
  | inC.cWireID==oldWireID = (True, [{comp & inputs=[{inC & cWireID=newWireID}]} : circMid], circBot, outputPairsA)
  | otherwise              = (inputChanged, [comp : newCircMid], newCircBot, outputPairsB)
where
  outputPairsA = ChangePositionPairID oldWireID newWireID outputPairs
  (inputChanged, newCircMid, newCircBot, outputPairsB)
    = ChangeInputIDInRewrite oldWireID endPoint newWireID circMid circBot outputPairs

ChangeInputIDInRewrite oldWireID endPoint newWireID [comp=:{inputs} : circMid] circBot outputPairs
  | isEmpty inputsBack = (inputChanged, [comp : newCircMid], newCircBot, outputPairs2)
  | otherwise
      = (True, [{comp & inputs = inputsFront ++ [{(hd inputsBack) & cWireID=newWireID} : tl inputsBack]} : circMid],
         circBot, outputPairs
        )
where
  // inputsBack will start with the connection with oldWireID, or be empty if it isn't there.
  (inputsFront, inputsBack) = span (((<>) oldWireID) o GetConnectWireID) inputs
  (inputChanged, newCircMid, newCircBot, outputPairs2)
    = ChangeInputIDInRewrite oldWireID endPoint newWireID circMid circBot outputPairs

ChangeInputIDInRewrite oldWireID endPoint newWireID [] [box=:{spec=Box boxCirc, pos=RCT boxRect} : circBot] outputPairs
  | IsInRectangle endPoint boxRect = (inputChangedA, [], [{box & spec=Box newBoxCirc} : circBot], outputPairsA)
  | otherwise                      = (inputChangedB, [], [box : newCircBot], outputPairsB)
where
  (inputChangedA, _, newBoxCirc, outputPairsA)
    = ChangeInputIDInRewrite oldWireID endPoint newWireID [] boxCirc outputPairs
  (inputChangedB, _, newCircBot, outputPairsB)
    = ChangeInputIDInRewrite oldWireID endPoint newWireID [] circBot outputPairs

ChangeInputIDInRewrite oldWireID endPoint newWireID [] [comp=:{spec=EndTerminal, inputs=[inC]} : circBot] outputPairs
  | inC.cWireID==oldWireID = (True, [], [{comp & inputs=[{inC & cWireID=newWireID}]} : circBot], outputPairsA)
  | otherwise              = (inputChanged, [], [comp : newCircBot], outputPairsB)
where
  outputPairsA = ChangePositionPairID oldWireID newWireID outputPairs
  (inputChanged, _, newCircBot, outputPairsB)
    = ChangeInputIDInRewrite oldWireID endPoint newWireID [] circBot outputPairs

ChangeInputIDInRewrite oldWireID endPoint newWireID [] [comp=:{inputs} : circBot] outputPairs
  | isEmpty inputsBack = (inputChanged, [], [comp : newCircBot], outputPairs2)
  | otherwise
      = (True, [], [{comp & inputs = inputsFront ++ [{(hd inputsBack) & cWireID=newWireID} : tl inputsBack]} : circBot],
         outputPairs
        )
where
  // inputsBack will start with the connection with oldWireID, or be empty if it isn't there.
  (inputsFront, inputsBack) = span (((<>) oldWireID) o GetConnectWireID) inputs
  (inputChanged, _, newCircBot, outputPairs2)
    = ChangeInputIDInRewrite oldWireID endPoint newWireID [] circBot outputPairs

ChangeInputIDInRewrite _ _ _ [] [] outputPairs = (False, [], [], outputPairs)

////////// MakeUserVarSubsFromTypes
// Takes a list of types and a list of substitutions for variables that must
// become user variables, and adds substitutions to replace any variables in the
// types with numeric user variables (with the same number).
MakeUserVarSubsFromTypes :: [Type] [(Type, Type)] -> [(Type, Type)]
MakeUserVarSubsFromTypes [type : types] userVarSubs
  = MakeUserVarSubsFromTypes types (MakeUserVarSubsFromType type userVarSubs)
MakeUserVarSubsFromTypes [] userVarSubs = userVarSubs

////////// MakeUserVarSubsFromType
// Takes a type and a list of substitutions for variables that must become user variables,
// and adds substitutions to replace any variables in the type with numeric user variables
// (with the same number).
MakeUserVarSubsFromType :: Type [(Type, Type)] -> [(Type, Type)]
MakeUserVarSubsFromType (Var n)      userVarSubs = InsertUserVarSub (Var n, UserVar2 n) userVarSubs
MakeUserVarSubsFromType (UserVar1 s) userVarSubs = userVarSubs
MakeUserVarSubsFromType (UserVar2 n) userVarSubs = userVarSubs
MakeUserVarSubsFromType Unit         userVarSubs = userVarSubs
MakeUserVarSubsFromType Counit       userVarSubs = userVarSubs
MakeUserVarSubsFromType (Const s)    userVarSubs = userVarSubs
MakeUserVarSubsFromType (Product (type1, type2)) userVarSubs
  = MakeUserVarSubsFromType type1 (MakeUserVarSubsFromType type2 userVarSubs)
MakeUserVarSubsFromType (Sum (type1, type2)) userVarSubs
  = MakeUserVarSubsFromType type1 (MakeUserVarSubsFromType type2 userVarSubs)
MakeUserVarSubsFromType (Then (type1, type2)) userVarSubs
  = MakeUserVarSubsFromType type1 (MakeUserVarSubsFromType type2 userVarSubs)
MakeUserVarSubsFromType (UserFunc name types) userVarSubs
  = MakeUserVarSubsFromTypes types userVarSubs

////////// InsertUserVarSub
// Adds a variable substitution to a list of variable substitutions unless it is a
// duplicate (assumes that any substitution for the same old type must be a duplicate).
InsertUserVarSub :: (Type, Type) [(Type, Type)] -> [(Type, Type)]
InsertUserVarSub (Var n1, userVar1) [(Var n2, userVar2) : userVarSubs]
  | n1==n2    = [(Var n2, userVar2) : userVarSubs]
  | otherwise = [(Var n2, userVar2) : InsertUserVarSub (Var n1, userVar1) userVarSubs]
InsertUserVarSub userVarSub [] = [userVarSub]

////////// InsertAll
// Inserts each component in the first list into the circuit at the top level (assumes that no cycles can result).
InsertAll :: ![Component] !Circuit -> Circuit
InsertAll [comp : comps] circuit
  = InsertAll comps newCircuit
where
  (_, newCircuit) = InsertUnboxed comp circuit

InsertAll [] circuit = circuit

////////// ModifyRule  // Right-hand side contains no start or end terminals
// Takes as arguments:
//   - the right side of the rule,
//   - the vector offset to reposition the right-side circuit,
//   - a list of wires for the circuit in which the rewrite is being done,
//   - a list of new wires internal to the rewrite (with their endpoints and wire IDs set, but types not changed),
//   - a list pairing input wire IDs in the rule with the corresponding circuit wires,
//   - a list pairing output wire IDs in the rule with the corresponding circuit wires,
//   - a list giving the rule wire IDs and the new circuit wire IDs (as a pair) with their new upper endpoints for
//     all wires internal to the rule for which output points have been found but not input points,
//   - the next free wire ID,
//   - the next free component ID,
//   - the new right-side circuit (in reverse order),
//   - a list of drawing functions.
// Returns:
//   - the new right-side circuit, repositioned and with its component IDs changed,
//   - the circuit's wires, with the rewrite's external wires repositioned and put back in,
//   - a list of new wires internal to the rewrite, positioned and numbered, but still with the rule's types,
//   - the new next free wire ID,
//   - the new next free component ID,
//   - a list giving the rule wire IDs and the new circuit wire IDs (as a pair) with their new upper endpoints for
//     all wires internal to the rule for which output points have been found but not input points (needed only
//     after modifying the circuit inside a box),
//   - a list of drawing functions, with functions added to draw the new components and wires.
ModifyRule :: !Circuit Vector [Wire] [Wire] [(WireID, Wire)] [(WireID, Wire)] [(WireSub, Point)] WireID ComponentID Circuit
              [DrawFunction] -> (Circuit, [Wire], [Wire], WireID, ComponentID, [(WireSub, Point)], [DrawFunction])

ModifyRule [box=:{spec=Box boxCircuit, id, inputs, outputs, pos=RCT ((x1,y1),(x2,y2))} : rule] (dx,dy) wires newWires
           inWireSubs outWireSubs wireSubs nextWireID nextCompID newCirc redraws1
  = ModifyRule rule (dx,dy) wires4 newWires3 inWireSubs outWireSubs wireSubs4 nextWireID3 (nextCompID2+1)
               [{box & spec=Box (reverse newBoxCirc), id=nextCompID2, inputs=inputs2, outputs=outputs2, pos=pos2}
                : newCirc
               ]
               ((DrawComponent {box & spec=Box [], pos=pos2}) ++ redraws4)
where
  pos2 = RCT ((x1+dx,y1+dy),(x2+dx,y2+dy))

  // Changes the wire IDs on the outputs, repositions the upper endpoint of external output wires and
  // adds them to the circuit's wires, adds to the list giving the rule's wire ID, the circuit's new
  // wire ID, and the upper endpoint of output wires for which input points haven't been found, and
  // adds drawing functions to draw the new external output wires.
  (outputs2, wires2, nextWireID2, wireSubs2, redraws2)
    = ChangeOutputs (Box []) pos2 0 outputs wires outWireSubs nextWireID wireSubs redraws1

  (newBoxCirc, wires3, newWires2, nextWireID3, nextCompID2, wireSubs3, redraws3)
    = ModifyRule boxCircuit (dx,dy) wires2 newWires inWireSubs outWireSubs wireSubs2 nextWireID2 nextCompID []
                 redraws2

  // Changes the wire IDs on the inputs, repositions the lower endpoint of external input wires and adds
  // them to the circuit's wires, adds new internal wires, removes the corresponding information about
  // internal wires from wireSubs3, and adds drawing functions to draw the new wires.
  (inputs2, wires4, newWires3, wireSubs4, redraws4)
    = ChangeInputs (Box []) pos2 0 inputs wires3 newWires2 inWireSubs wireSubs3 redraws3

ModifyRule [comp=:{id, spec, inputs, outputs} : rule] offset wires newWires inWireSubs outWireSubs
           wireSubs nextWireID nextCompID newCirc redraws1
  = ModifyRule rule offset wires3 newWires2 inWireSubs outWireSubs wireSubs3 nextWireID2 (nextCompID+1)
               [{comp2 & inputs=inputs2, outputs=outputs2} : newCirc] ((DrawComponent comp2) ++ redraws3)
where
  (comp2=:{pos}) = MoveComponent offset {comp & id=nextCompID}

  // Changes the wire IDs on the inputs, repositions the lower endpoint of external input wires and adds
  // them to the circuit's wires, adds new internal wires, removes the corresponding information about
  // internal wires from wireSubs3, and adds drawing functions to draw the new wires.
  (inputs2, wires2, newWires2, wireSubs2, redraws2)
    = ChangeInputs spec pos 0 inputs wires newWires inWireSubs wireSubs redraws1

  // Changes the wire IDs on the outputs, repositions the upper endpoint of external output wires and
  // adds them to the circuit's wires, adds to the list giving the rule's wire ID, the circuit's new
  // wire ID, and the upper endpoint of output wires for which input points haven't been found, and
  // adds drawing functions to draw the new external output wires.
  (outputs2, wires3, nextWireID2, wireSubs3, redraws3)
    = ChangeOutputs spec pos 0 outputs wires2 outWireSubs nextWireID wireSubs2 redraws2

ModifyRule [] _ wires newWires _ _ wireSubs nextWireID nextCompID newCirc redraws
  = (newCirc, wires, newWires, nextWireID, nextCompID, wireSubs, redraws)

////////// ChangeInputs  //*^* Could also change inWireSubs (removing substitutions as they are used)
// Takes as arguments:
//   - the specification of the component (used to determine its input connection positions),
//   - the position of the component (used to determine its input connection positions),
//   - an integer identifying the first connection (used to determine its input connection positions),
//   - a list of the component's input connections,
//   - a list of circuit wires,
//   - a list of new wires internal to the rewrite,
//   - a list pairing outer input wire IDs in the rule with the corresponding wires in the circuit,
//   - a list giving the old wire ID in the rule, the new wire ID in the circuit and the upper
//     endpoint for new internal wires whose lower endpoint hasn't been found yet,
//   - a list of drawing functions.
// Returns:
//   - the new list of input connections, with wire IDs changed,
//   - a list of circuit wires, with outer input wires put back in with their lower endpoints changed,
//   - a list of new wires internal to the rewrite, with new wire IDs and positions, but the old types,
//   - a list giving the old wire ID in the rule, the new wire ID in the circuit and the upper
//     endpoint for new internal wires whose lower endpoint hasn't been found yet,
//   - a list of drawing functions, with functions to draw new wires added.
//*^*ChangeInputs :: CompSpecifics Placement Int ![Connection] [Wire] [Wire] [(WireID, Wire)] [(WireSub, Point)]
ChangeInputs :: (CompSpecifics Component) Placement Int ![Connection] [Wire] [Wire] [(WireID, Wire)] [(WireSub, Point)]
                [DrawFunction] -> ([Connection], [Wire], [Wire], [(WireSub, Point)], [DrawFunction])
ChangeInputs spec pos n [input=:{cWireID, cWireType} : inputs] wires newWires inWireSubs wireSubs redraws
  | isEmpty backInSubs
      = ([{input & cWireID=(snd (fst wireSubA))} : inputs2A], wires2A, newWires2A, wireSubs3A, redrawsA)
  | IsUserType wireB.wireType
      = ([{input & cWireID=wireB.wireID, cWireType=MakeUserType cWireType} : inputs2B], wires3B, newWires2B, wireSubs2B,
         redrawsB
        )
  | otherwise
      = ([{input & cWireID=wireB.wireID} : inputs2B], wires3B, newWires2B, wireSubs2B, redrawsB)
where
  // If the input wire is external, the substitution pair giving the corresponding wire from the circuit
  // will be at the front of backInSubs, otherwise backInSubs will be empty.
  (frontInSubs, backInSubs) = span (((<>) cWireID) o fst) inWireSubs

  // If the input wire is internal, its new wire ID and upper endpoint will be found in wireSubs.
  (wireSubA, wireSubs2A)    = RemoveAndReturn (((==) cWireID) o fst o fst) wireSubs
  newWireA = {wireID=(snd (fst wireSubA)), wireLine=(snd wireSubA, InConnectionPoint spec pos n), wireType=cWireType}
  (inputs2A, wires2A, newWires2A, wireSubs3A, redrawsA)
     = ChangeInputs spec pos (n+1) inputs wires [newWireA : newWires] inWireSubs wireSubs2A
                    ((DrawWire newWireA) ++ redraws)

  wireB      = snd (hd backInSubs)
  (start, _) = wireB.wireLine
  newWireB   = {wireB & wireLine=(start, InConnectionPoint spec pos n)}
  (inputs2B, wires3B, newWires2B, wireSubs2B, redrawsB)
     = ChangeInputs spec pos (n+1) inputs [newWireB : wires] newWires (frontInSubs ++ (tl backInSubs)) wireSubs
                    ((DrawWire newWireB) ++ redraws)

ChangeInputs _ _ _ [] wires newWires _ wireSubs redraws
  = ([], wires, newWires, wireSubs, redraws)

////////// ChangeOutputs  //*^* Could also change outWireSubs (removing substitutions as they are used)
// Takes as arguments:
//   - the specification of the component (used to determine its output connection positions),
//   - the position of the component (used to determine its output connection positions),
//   - an integer identifying the first connection (used to determine its output connection positions),
//   - a list of the component's output connections,
//   - a list of circuit wires,
//   - a list pairing outer output wire IDs in the rule with the corresponding wires in the circuit,
//   - the next free wire ID in the circuit,
//   - a list giving the old wire ID in the rule, the new wire ID in the circuit and the upper
//     endpoint for new internal wires whose lower endpoint hasn't been found yet,
//   - a list of drawing functions.
// Returns:
//   - the new list of input connections, with wire IDs changed,
//   - a list of circuit wires, with outer output wires put back in with their upper endpoints changed,
//   - the new next free wire ID in the circuit,
//   - a list giving the old wire ID in the rule, the new wire ID in the circuit and the upper
//     endpoint for new internal wires whose lower endpoint hasn't been found yet,
//   - a list of drawing functions, with functions to draw new external output wires added.

//*^*ChangeOutputs :: CompSpecifics Placement Int ![Connection] [Wire] [(WireID, Wire)] WireID [(WireSub, Point)]
ChangeOutputs :: (CompSpecifics Component) Placement Int ![Connection] [Wire] [(WireID, Wire)] WireID [(WireSub, Point)]
                 [DrawFunction] -> ([Connection], [Wire], WireID, [(WireSub, Point)], [DrawFunction])
ChangeOutputs spec pos n [output=:{cWireID, cWireType} : outputs] wires outWireSubs nextWireID wireSubs redraws
  | isEmpty backOutSubs
      = ([{output & cWireID=nextWireID} : outputs2A], wires2A, nextWireID2A, wireSubs2A, redrawsA)
  | IsUserType wireB.wireType
      = ([{output & cWireID=wireB.wireID, cWireType=MakeUserType cWireType} : outputs2B], wires3B, nextWireID2B, wireSubs2B,
         redrawsB
        )
  | otherwise
      = ([{output & cWireID=wireB.wireID} : outputs2B], wires3B, nextWireID2B, wireSubs2B, redrawsB)
where
  // If the output wire is external, the substitution pair giving the corresponding wire from the circuit
  // will be at the front of backOutSubs, otherwise backOutSubs will be empty.
  (frontOutSubs, backOutSubs) = span (((<>) cWireID) o fst) outWireSubs

  (outputs2A, wires2A, nextWireID2A, wireSubs2A, redrawsA)
     = ChangeOutputs spec pos (n+1) outputs wires outWireSubs (nextWireID+1)
                     [((cWireID, nextWireID), OutConnectionPoint spec pos n) : wireSubs] redraws

  wireB           = snd (hd backOutSubs)
  (_, end)        = wireB.wireLine
  newWireB        = {wireB & wireLine=(OutConnectionPoint spec pos n, end)}
  (outputs2B, wires3B, nextWireID2B, wireSubs2B, redrawsB)
     = ChangeOutputs spec pos (n+1) outputs [newWireB : wires] (frontOutSubs ++ (tl backOutSubs)) nextWireID
                     wireSubs ((DrawWire newWireB) ++ redraws)

ChangeOutputs _ _ _ [] wires _ nextWireID wireSubs redraws
  = ([], wires, nextWireID, wireSubs, redraws)

////////// SubsIntoRule
// Takes the right-side circuit, the new wires internal to the rewrite, a list of substitutions
// pairing wire types from the rule with wire types from the circuit, and a list of substitutions
// for variables that must become user variables, matches the wire types from the circuit with the
// wire types from the rule, makes variable substitutions in the right-side circuit and the new wires,
// if the types can be matched, and adds any new substitutions for variables that must become user
// variables.
SubsIntoRule :: Circuit [Wire] ![(WireType,WireType)] [(Type, Type)] -> (Bool, Circuit, [Wire], [(Type, Type)])
SubsIntoRule rightSide newWires typePairs newUserVarSubs1
  | noProblem = (True, SubsIntoRuleCircuit subs rightSide, SubsIntoRuleWires subs newWires, newUserVarSubs2)
  | otherwise = (False, [], [], [])
where
  (noProblem, subs, newUserVarSubs2) = MatchTypePairs typePairs [] newUserVarSubs1

////////// MatchTypePairs
// Takes a list of substitutions pairing wire types from the rule with wire types from the circuit,
// a list of variable substitutions found so far, and a list of variable substitutions for variables
// that must become user variables, and matches the rule types with the circuit types (if possible),
// adding to the two substitution lists.
MatchTypePairs :: ![(WireType, WireType)] [(Type, Type)] [(Type, Type)] -> (Bool, [(Type, Type)], [(Type, Type)])

MatchTypePairs [(wireType1, wireType2) : typePairs] subs1 newUserVarSubs1
  | noProblem = MatchTypePairs typePairs subs2 newUserVarSubs2
  | otherwise = (False, [], [])
where
  (noProblem, subs2, newUserVarSubs2) = MatchRewriteTypes (GetType wireType2) (GetType wireType1) subs1 newUserVarSubs1

MatchTypePairs [] subs newUserVarSubs = (True, subs, newUserVarSubs)

//////// MatchRewriteTypes
// Matches two types.  Returns True if the first type is no more
// general than the second, with a list of substitutions for variables
// and user variables that will make second type equal to the first.
// The two types are assumed to have independent variables.  It won't
// allow a variable to be substituted by more than one type.  It also
// adds to a list of substitutions for variables that must become
// user variables (its third argument).
MatchRewriteTypes :: Type !Type [(Type, Type)] [(Type, Type)] -> (Bool, [(Type, Type)], [(Type, Type)])
MatchRewriteTypes type (Var a) subs1 newUserVarSubs
  = (success, subs2, newUserVarSubs)
where
  (success, subs2) = InsertRewriteSub (Var a, type) subs1
MatchRewriteTypes type (UserVar2 a) subs1 newUserVarSubs
  = (success, subs2, MakeUserVarSubsFromType type newUserVarSubs)
where
  (success, subs2) = InsertRewriteSub (UserVar2 a, type) subs1
MatchRewriteTypes type1 type2 subs newUserVarSubs
  | SameConstructor type1 type2 = MatchRewriteTypeArgs (Args type1) (Args type2) subs newUserVarSubs
  | otherwise                   = (False, [], [])

//////// MatchRewriteTypeArgs
MatchRewriteTypeArgs :: ![Type] ![Type] [(Type, Type)] [(Type, Type)] -> (Bool, [(Type, Type)], [(Type, Type)])
MatchRewriteTypeArgs [type1 : types1] [type2 : types2] subs1 newUserVarSubs1
  | success   = MatchRewriteTypeArgs types1 types2 subs2 newUserVarSubs2
  | otherwise = (False, [], [])
where
  (success, subs2, newUserVarSubs2) = MatchRewriteTypes type1 type2 subs1 newUserVarSubs1

MatchRewriteTypeArgs [] [] subs newUserVarSubs = (True, subs, newUserVarSubs)

MatchRewriteTypeArgs _ _ _ _ = (False, [], [])

//////// InsertRewriteSub
InsertRewriteSub :: !(Type, Type) ![(Type, Type)] -> (Bool, [(Type, Type)])
InsertRewriteSub sub subs = InsertRewriteSubAux sub subs []
where
  InsertRewriteSubAux (oldType1, newType1) [(oldType2, newType2) : subs] newSubs
    | EqualTypes oldType1 oldType2
        = if (EqualTypes newType1 newType2)
             (True, [(oldType2, newType2) : (subs ++ newSubs)])
             (False, [])
    | otherwise = InsertRewriteSubAux (oldType1, newType1) subs [(oldType2, newType2) : newSubs]

  InsertRewriteSubAux sub [] newSubs = (True, [sub : newSubs])

////////// SubsIntoRuleCircuit
// Makes variable and user-variable substitutions in the circuit, including user-specified types.
SubsIntoRuleCircuit :: [(Type, Type)] !Circuit -> Circuit
SubsIntoRuleCircuit subs [comp : circuit] = [SubsIntoRuleComponent subs comp : SubsIntoRuleCircuit subs circuit]
SubsIntoRuleCircuit _ [] = []

////////// SubsIntoRuleComponent
// Makes variable and user-variable substitutions in a component, including user-specified types.
SubsIntoRuleComponent :: [(Type, Type)] !Component -> Component
SubsIntoRuleComponent subs comp=:{spec=Box boxCircuit, inputs, outputs}
  = {comp & spec=Box (SubsIntoRuleCircuit subs boxCircuit),
            inputs=(map (SubsIntoRuleConnect subs) inputs),
            outputs=(map (SubsIntoRuleConnect subs) outputs)}
SubsIntoRuleComponent subs comp=:{inputs, outputs}
  = {comp & inputs=(map (SubsIntoRuleConnect subs) inputs), outputs=(map (SubsIntoRuleConnect subs) outputs)}

////////// SubsIntoRuleConnect
// Makes variable and user-variable substitutions in a connection, including user-specified types.
SubsIntoRuleConnect :: [(Type, Type)] !Connection -> Connection
SubsIntoRuleConnect subs connect=:{cWireType}
  = {connect & cWireType = SubsIntoRuleWireType subs cWireType}

///////// SubsIntoRuleWires
// Makes variable and user-variable substitutions in a list of wires, including user-specified types.
SubsIntoRuleWires :: [(Type, Type)] ![Wire] -> [Wire]
SubsIntoRuleWires subs [wire=:{wireType} : wires]
  = [{wire & wireType = SubsIntoRuleWireType subs wireType} : SubsIntoRuleWires subs wires]
SubsIntoRuleWires subs [] = []

////////// SubsIntoRuleWireType
// Makes variable and user-variable substitutions in a wire type, including user-specified types.
SubsIntoRuleWireType :: [(Type, Type)] !WireType -> WireType
SubsIntoRuleWireType subs (User type)
  = User (SubsIntoRuleType subs type)
SubsIntoRuleWireType subs (Free type)
  = Free (SubsIntoRuleType subs type)

///////// SubsIntoRuleType
SubsIntoRuleType :: ![(Type, Type)] Type -> Type
SubsIntoRuleType subs type = foldl (flip SubIntoRuleType) type subs

///////// SubIntoRuleType
SubIntoRuleType :: !(Type, Type) !Type -> Type

SubIntoRuleType sub (Product (type1, type2))
  = Product ((SubIntoRuleType sub type1), (SubIntoRuleType sub type2))

SubIntoRuleType sub (Sum (type1, type2))
  = Sum ((SubIntoRuleType sub type1), (SubIntoRuleType sub type2))

SubIntoRuleType sub (Then (type1, type2))
  = Then ((SubIntoRuleType sub type1), (SubIntoRuleType sub type2))

SubIntoRuleType sub (UserFunc name types)
  = UserFunc name (SubIntoRuleTypes sub types)

SubIntoRuleType (Var a, newType) (Var b)
  | a==b      = newType
  | otherwise = (Var b)

SubIntoRuleType (UserVar1 a, newType) (UserVar1 b)
  | a==b      = newType
  | otherwise = (UserVar1 b)

SubIntoRuleType (UserVar2 a, newType) (UserVar2 b)
  | a==b      = newType
  | otherwise = (UserVar2 b)

SubIntoRuleType _ type = type

////////// SubIntoRuleTypes
SubIntoRuleTypes :: (Type, Type) [Type] -> [Type]
SubIntoRuleTypes sub [type : types] = [SubIntoRuleType sub type : SubIntoRuleTypes sub types]
SubIntoRuleTypes _ [] = []

////////// SetUserTypeOnOutputs
// Changes the output wire types to be user-specified for wires identified by the wire ID list in the middle
// and top parts of the circuit, returning the new middle and top parts of the circuit and a list of wire
// IDs for any output connections that weren't found.  Does not recurse into boxes.
SetUserTypeOnOutputs :: ![WireID] Circuit Circuit [WireID] -> (Circuit, Circuit, [WireID])
SetUserTypeOnOutputs [] midCirc topCirc newUserWires = (midCirc, topCirc, newUserWires)
SetUserTypeOnOutputs [wireID : newUserWires1] midCirc topCirc newUserWires2
  | changed1  = SetUserTypeOnOutputs newUserWires1 newMidCirc topCirc newUserWires2
  | changed2  = SetUserTypeOnOutputs newUserWires1 midCirc newTopCirc newUserWires2
  | otherwise = SetUserTypeOnOutputs newUserWires1 newMidCirc newTopCirc [wireID : newUserWires2]
where
  (changed1, newMidCirc) = SetUserTypeOnOutput wireID midCirc
  (changed2, newTopCirc) = SetUserTypeOnOutput wireID topCirc

////////// SetUserTypeOnOutput
// Changes the output wire type to be user-specified for the wire identified by the wire ID in the given
// circuit, returning True if the output connection was found with the new circuit, and false otherwise.
// Does not recurse into boxes.
SetUserTypeOnOutput :: WireID !Circuit -> (Bool, Circuit)
SetUserTypeOnOutput wireID [comp=:{outputs} : circuit]
  | changed1  = (True, [{comp & outputs=newOutputs} : circuit])
  | otherwise = (changed2, [comp : newCircuit])
where
  (changed1, newOutputs) = MakeUserTypeInConnects wireID outputs
  (changed2, newCircuit) = SetUserTypeOnOutput wireID circuit

SetUserTypeOnOutput wireID [] = (False, [])

////////// SetUserTypeOnInputs
// Changes the input wire types to be user-specified for wires identified by the wire ID list in the middle
// and bottom parts of the circuit, returning the new middle and bottom parts of the circuit and a list of
// wire IDs for any input connections that weren't found.
SetUserTypeOnInputs :: ![WireID] Circuit Circuit [WireID] -> (Circuit, Circuit, [WireID])
SetUserTypeOnInputs [] midCirc botCirc newUserWires = (midCirc, botCirc, newUserWires)
SetUserTypeOnInputs [wireID : newUserWires1] midCirc botCirc newUserWires2
  | changed1  = SetUserTypeOnInputs newUserWires1 newMidCirc botCirc newUserWires2
  | changed2  = SetUserTypeOnInputs newUserWires1 midCirc newBotCirc newUserWires2
  | otherwise = SetUserTypeOnInputs newUserWires1 midCirc botCirc [wireID : newUserWires2]
where
  (changed1, newMidCirc) = SetUserTypeOnInput wireID midCirc
  (changed2, newBotCirc) = SetUserTypeOnInput wireID botCirc

////////// SetUserTypeOnInput
// Changes the input wire type to be user-specified for the wire identified by the wire ID in the given
// circuit, returning True and the new circuit if the input connection was found and False otherwise.
SetUserTypeOnInput :: WireID !Circuit -> (Bool, Circuit)

SetUserTypeOnInput wireID [box=:{spec=Box boxCircuit, inputs=[{cWireID, cWireType}]} : circuit]
  | wireID==cWireID = (True, [{box & inputs=[{cWireID=cWireID, cWireType = MakeUserType cWireType}]} : circuit])
  | changedInBox    = (True, [{box & spec=Box newBoxCircuit} : circuit])
  | otherwise       = (changedInCirc, [box : newCircuit])
where
  (changedInBox, newBoxCircuit) = SetUserTypeOnInput wireID boxCircuit
  (changedInCirc, newCircuit)   = SetUserTypeOnInput wireID circuit

SetUserTypeOnInput wireID [comp=:{inputs} : circuit]
  | changed1  = (True, [{comp & inputs=newInputs} : circuit])
  | otherwise = (changed2, [comp : newCircuit])
where
  (changed1, newInputs)  = MakeUserTypeInConnects wireID inputs
  (changed2, newCircuit) = SetUserTypeOnInput wireID circuit

SetUserTypeOnInput wireID [] = (False, [])

////////// MakeUserTypeInConnects
// Changes the connection with the given wire ID to have a user-specified type, if it
// is found, or returns False with the connections unchanged if not.
MakeUserTypeInConnects :: WireID ![Connection] -> (Bool, [Connection])
MakeUserTypeInConnects wireID [connect=:{cWireID, cWireType} : connects]
  | wireID==cWireID = (True, [{connect & cWireType=MakeUserType cWireType} : connects])
  | otherwise       = (done, [connect : newConnects])
where
  (done, newConnects) = MakeUserTypeInConnects wireID connects

MakeUserTypeInConnects wireID [] = (False, [])

////////// SetFreeTypeOnOutput
// Changes the output wire type to be free for the wire identified by the wire ID in the given circuit,
// returning True if the output connection was found with the new circuit, and false otherwise.  Does
// not recurse into boxes.
SetFreeTypeOnOutput :: WireID !Circuit -> (Bool, Circuit)
SetFreeTypeOnOutput wireID [comp=:{outputs} : circuit]
  | changed1  = (True, [{comp & outputs=newOutputs} : circuit])
  | otherwise = (changed2, [comp : newCircuit])
where
  (changed1, newOutputs) = MakeFreeTypeInConnects wireID outputs
  (changed2, newCircuit) = SetFreeTypeOnOutput wireID circuit

SetFreeTypeOnOutput wireID [] = (False, [])

////////// SetFreeTypeOnInput
// Changes the input wire type to be free for the wire identified by the wire ID in the given circuit,
// returning True and the new circuit if the input connection was found and False otherwise.
SetFreeTypeOnInput :: WireID !Circuit -> (Bool, Circuit)

SetFreeTypeOnInput wireID [box=:{spec=Box boxCircuit, inputs=[{cWireID, cWireType}]} : circuit]
  | wireID==cWireID = (True, [{box & inputs=[{cWireID=cWireID, cWireType = MakeFree cWireType}]} : circuit])
  | changedInBox    = (True, [{box & spec=Box newBoxCircuit} : circuit])
  | otherwise       = (changedInCirc, [box : newCircuit])
where
  (changedInBox, newBoxCircuit) = SetFreeTypeOnInput wireID boxCircuit
  (changedInCirc, newCircuit)   = SetFreeTypeOnInput wireID circuit

SetFreeTypeOnInput wireID [comp=:{inputs} : circuit]
  | changed1  = (True, [{comp & inputs=newInputs} : circuit])
  | otherwise = (changed2, [comp : newCircuit])
where
  (changed1, newInputs)  = MakeFreeTypeInConnects wireID inputs
  (changed2, newCircuit) = SetFreeTypeOnInput wireID circuit

SetFreeTypeOnInput wireID [] = (False, [])

////////// MakeFreeTypeInConnects
// Changes the connection with the given wire ID to have a free type, if it
// is found, or returns False with the connections unchanged if not.
MakeFreeTypeInConnects :: WireID ![Connection] -> (Bool, [Connection])
MakeFreeTypeInConnects wireID [connect=:{cWireID, cWireType} : connects]
  | wireID==cWireID = (True, [{connect & cWireType=MakeFree cWireType} : connects])
  | otherwise       = (done, [connect : newConnects])
where
  (done, newConnects) = MakeFreeTypeInConnects wireID connects

MakeFreeTypeInConnects wireID [] = (False, [])

////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////// MatchLeftSide
// Takes as arguments:
//   - the component in the rule with the live wire as an output,
//   - the live wire ID in the rule,
//   - the top part of the rule (everything that connects down to the live-wire component),
//   - the middle part of the rule (everything except the live-wire component that doesn't connect down to or from it,
//   - the bottom part of the rule (everything that connects down from the live-wire component),
//   - the live wire ID in the circuit,
//   - the circuit.
// Returns:
//   - True if the rule was matched in the circuit, False otherwise,
//   - the top part of the circuit (everything above the matched components),
//   - the middle part of the circuit (everything neither above nor below the matched components, except
//     the matched components, which are removed),
//   - the bottom part of the circuit (everything below the matched components),
//   - the enclosing rectangle for the matched components in the circuit,
//   - a list of drawing functions to erase the matched components.
//
// Assumes that the live wire doesn't have an endpoint in a box.
MatchLeftSide :: !Component WireID Circuit Circuit Circuit WireID !Circuit
                  -> (Bool, Circuit, Circuit, Circuit, Rectangle, [WireSub], [DrawFunction])

MatchLeftSide ruleComp=:{spec=StartTerminal} ruleWireID [] ruleMiddle ruleBottom circWireID circuit
  | IsDummy circComp   // If true, the live wire is an input wire to this level of the circuit.
      = if (success1A && success2A)
           (True, circTop2A, circMiddle2A, circBottom3A, enclosingRect2A, wireSubs2A, redraws2A)
           (False, [], [], [], dummyRectangle, [], [])
  | success1B && success2B
      = (True, circTop2B, circMiddle2B, circBottom3B, enclosingRect2B, wireSubs2B, redraws2B)
  | otherwise
      = (False, [], [], [], dummyRectangle, [], [])
where
  // Finds the component with the given wire as an output, returning the part of the circuit above it,
  // the component and the part of the circuit below it.  If the component isn't found, it returns
  // a dummy component.
  (circTop1, circComp, circBottom1) = FindOutWire circWireID circuit  // circTop reversed

  //* The following two function calls are made when the live wire is an input wire to this level of the circuit.

  // Matches the bottom part of the rule with the circuit, removing matched components and returning their
  // enclosing rectangle, the part of the circuit that was parallel to the matched components (circMiddle1A),
  // the part of the circuit that was below the matched components (circBottom2A), a list identifying input
  // connections for which the output connections haven't been found, pairing rule wire IDs with circuit wire IDs,
  // a list pairing all rule wire IDs matched so far with circuit wire IDs, and a list of drawing functions.
  (success1A, enclosingRect1A, circMiddle1A, circBottom2A, bottomPairsA, wireSubs1A, redraws1A)
    = SearchBottoms [(ruleWireID, circWireID)] [] ruleBottom [] circuit dummyRectangle [(ruleWireID, circWireID)] []

  // Matches the middle part of the rule with the circuit, removing matched components and returning their
  // enclosing rectangle, the part of the circuit that was above all matched components, the part of the circuit
  // that was parallel to matched components, the part of the circuit that was below matched components, a list
  // pairing all rule wire IDs with circuit wire IDs, and a list of drawing functions to erase all matched components.
  (success2A, enclosingRect2A, circTop2A, circMiddle2A, circBottom3A, wireSubs2A, redraws2A)
    = SearchMiddles [] bottomPairsA ruleMiddle [] circMiddle1A circBottom2A enclosingRect1A wireSubs1A redraws1A

  //* The following two function calls are made when the live wire is *not* an input wire to this level of the circuit.

  // Matches the bottom part of the rule with the circuit, as above.
  (success1B, enclosingRect1B, circMiddle1B, circBottom2B, bottomPairsB, wireSubs1B, redraws1B)
    = SearchBottoms [(ruleWireID, circWireID)] [] ruleBottom [] circBottom1 dummyRectangle [(ruleWireID, circWireID)] []

  // Matches the middle part of the rule with the circuit, as above.
  (success2B, enclosingRect2B, circTop2B, circMiddle2B, circBottom3B, wireSubs2B, redraws2B)
    = SearchMiddles [] bottomPairsB ruleMiddle [circComp : circTop1] circMiddle1B circBottom2B enclosingRect1B wireSubs1B
                    redraws1B

// ruleTop is reversed
MatchLeftSide ruleComp=:{spec=Box ruleBoxCircuit} ruleWireID ruleTop ruleMiddle ruleBottom circWireID circuit
  | (not (IsDummy circComp)) && goodMatch && noDups1 && noDups2 && matchingBoxes && success1 && success2 && success3
      = (True, circTop3, circMiddle, circBottom3, enclosingRect3, wireSubs6, redraws3)  // circTop3 reversed
  | otherwise
      = (False, [], [], [], dummyRectangle, [], [])
where
  // If a dummy component is returned as circComp, then no matching component was found at this level of the circuit.
  (circTop1, circComp, circBottom1)  = FindOutWire circWireID circuit // circTop1 reversed

  // Returns True if the two components are of the same type.
  goodMatch = EquivalentSpecs ruleComp.spec circComp.spec

  // Makes pairs of wire IDs from the rule component's input and output connections and the circuit component's
  // input and output connections, and also adds them to the list of wire ID substitutions.  Returns False for
  // its first argument if any wire ID is found to have been paired with a different wire ID already (this
  // indicates that the circuits don't match).
  (noDups1, inputPairs, wireSubs1)   = MakeWirePairs ruleComp.inputs circComp.inputs [(ruleWireID, circWireID)]
  (noDups2, outputPairs, wireSubs2)  = MakeWirePairs ruleComp.outputs circComp.outputs wireSubs1

  // Matches the two box circuits, starting from their internal input wire, and returns True if they match,
  // the new list of input connections for which output connections haven't been found (bottomPairs), the new
  // list of output connections for which input connections haven't been found (topPairs), and the new list pairing
  // matched rule wire IDs with circuit wire IDs.
  (matchingBoxes, bottomPairs, topPairs, wireSubs3)
    = MatchCircuits (GetConnectWireID (hd ruleComp.inputs)) ruleBoxCircuit (GetConnectWireID (hd circComp.inputs))
                    (BoxCircuit circComp) inputPairs outputPairs wireSubs2

  (success1, enclosingRect1, circTopMiddle, circTop2, topPairs2, wireSubs4, redraws1)
    = SearchTops bottomPairs [] ruleTop [] circTop1 (ComponentRectangle circComp) wireSubs3 (DrawComponent circComp)
  (success2, enclosingRect2, circBottomMiddle, circBottom2, bottomPairs2, wireSubs5, redraws2)
    = SearchBottoms topPairs [] ruleBottom [] circBottom1 enclosingRect1 wireSubs4 redraws1
  (success3, enclosingRect3, circTop3, circMiddle, circBottom3, wireSubs6, redraws3)
    = SearchMiddles topPairs2 bottomPairs2 ruleMiddle circTop2 (ReverseAppend circTopMiddle circBottomMiddle)
                    circBottom2 enclosingRect2 wireSubs5 redraws2

// ruleTop is reversed
MatchLeftSide ruleComp ruleWireID ruleTop ruleMiddle ruleBottom circWireID circuit
  | (not (IsDummy circComp)) && goodMatch && noDups1 && noDups2 && success1 && success2 && success3
      = (True, circTop3, circMiddle, circBottom3, enclosingRect3, wireSubs5, redraws3)  // circTop3 reversed
  | otherwise
      = (False, [], [], [], dummyRectangle, [], [])
where
  (circTop1, circComp, circBottom1) = FindOutWire circWireID circuit // circTop1 reversed
  goodMatch                         = EquivalentSpecs ruleComp.spec circComp.spec
  (noDups1, inputPairs, wireSubs1)  = MakeWirePairs ruleComp.inputs circComp.inputs [(ruleWireID, circWireID)]
  (noDups2, outputPairs, wireSubs2) = MakeWirePairs ruleComp.outputs circComp.outputs wireSubs1
  (success1, enclosingRect1, circTopMiddle, circTop2, topPairs, wireSubs3, redraws1)
    = SearchTops inputPairs [] ruleTop [] circTop1 (ComponentRectangle circComp) wireSubs2 (DrawComponent circComp)
  (success2, enclosingRect2, circBottomMiddle, circBottom2, bottomPairs, wireSubs4, redraws2)
    = SearchBottoms outputPairs [] ruleBottom [] circBottom1 enclosingRect1 wireSubs3 redraws1
  (success3, enclosingRect3, circTop3, circMiddle, circBottom3, wireSubs5, redraws3)
    = SearchMiddles topPairs bottomPairs ruleMiddle circTop2 (ReverseAppend circTopMiddle circBottomMiddle)
                    circBottom2 enclosingRect2 wireSubs4 redraws2

////////// FindOutWire
// Takes a wire ID and a circuit and finds the component with the wire as an output, returning the part of the
// circuit that is above the component in reverse order, the component, and the part of the circuit that is
// below the component.  If no such component is found, it returns a dummy component.
FindOutWire :: WireID !Circuit -> (Circuit, Component, Circuit)
FindOutWire wireID circuit = FindOutWireAux [] circuit
where
  FindOutWireAux circuit1 [comp : circuit2]
    | any ((==) wireID) (GetOutWireIDs comp)
        = (circuit1, comp, circuit2)
    | otherwise
        = FindOutWireAux [comp : circuit1] circuit2
  FindOutWireAux _ [] = ([], dummyComponent, [])

////////// MakeWirePairs
// Takes two lists of connections and a list of wire ID substitutions and pairs the wire IDs from the
// connections, returning False if any wire ID pairs include a wire ID that has already been paired
// with a different wire ID in the wire ID substitutions, and returning True, the wire ID pairs,
// and the new list of wire ID substitutions otherwise.
MakeWirePairs :: ![Connection] ![Connection] [WireSub] -> (Bool, [WireSub], [WireSub])
MakeWirePairs [{cWireID=id1} : connects1] [{cWireID=id2} : connects2] wireSubs1
  | insertOK  = (noDups, [(id1,id2) : newSubs], wireSubs3)
  | otherwise = (False, [], [])
where
  (insertOK, wireSubs2)        = InsertWireSub (id1,id2) wireSubs1
  (noDups, newSubs, wireSubs3) = MakeWirePairs connects1 connects2 wireSubs2
MakeWirePairs [] [] wireSubs = (True, [], wireSubs)

////////// InsertWireSub
// Inserts a wire ID substitution into a list of wire ID substitutions, returning False
// if it finds that a different substitution has already been made for either wire ID
// in the pair.
InsertWireSub :: !WireSub ![WireSub] -> (Bool, [WireSub])
InsertWireSub (wireIDa1, wireIDb1) [(wireIDa2, wireIDb2) : subs]
  | wireIDa1==wireIDa2
      = if (wireIDb1==wireIDb2)
           (True, [(wireIDa2, wireIDb2) : subs])
           (False, [])
  | wireIDb1==wireIDb2  = (False, [])
  | otherwise           = (insertOK, [(wireIDa2, wireIDb2) : newSubs])
where
  (insertOK, newSubs) = InsertWireSub (wireIDa1, wireIDb1) subs

InsertWireSub (wireIDa, wireIDb) [] = (True, [(wireIDa, wireIDb)])

////////// SearchTops
// Takes as arguments:
//   - a list of wire ID pairs for wires whose input end has been found but whose output end has not,
//   - a list of wire ID pairs for wires whose output end has been found but whose input end has not,
//   - the top part of the left-side of the rule (reversed),
//   - the middle part of the circuit, reversed (everything so far found to be parallel to matched components),
//   - the top part of the circuit, reversed,
//   - the enclosing rectangle for components matched so far,
//   - a list pairing left-side wire IDs with circuit wire IDs,
//   - a list of drawing functions to erase components matched so far.
// Returns:
//   - True if the top part of the left-side was matched, False otherwise,
//   - the enclosing rectangle for matched components,
//   - the middle part of the circuit, reversed
//   - the top part of the circuit, reversed
//   - a list of wire ID pairs for wires whose output end has been found but whose input end has not,
//   - a list pairing left-side wire IDs with circuit wire IDs,
//   - a list of drawing functions to erase matched components.
SearchTops :: [WireSub] [WireSub] !Circuit Circuit Circuit Rectangle [WireSub] [DrawFunction]
               -> (Bool, Rectangle, Circuit, Circuit, [WireSub], [WireSub], [DrawFunction])

// circTop and circMiddle are reversed
SearchTops bottomPairs topPairs [ruleComp=:{spec=StartTerminal, outputs=[{cWireID}]} : ruleTop]
           circMiddle1 circTop1 enclosingRect wireSubs1 redraws
  = SearchTops otherPairs topPairs ruleTop circMiddle1 circTop1 enclosingRect wireSubs1 redraws
where
  otherPairs = filter (((<>) cWireID) o fst) bottomPairs

SearchTops bottomPairs1 topPairs1 [ruleComp=:{spec=Box ruleBoxCircuit} : ruleTop]
           circMiddle1 circTop1 enclosingRect wireSubs1 redraws
  | (not (IsDummy circComp)) && goodMatch && noDups1 && noDups2 && matchingBoxes
      = SearchTops (otherPairs ++ bottomPairs3) (topPairs3 ++ topPairs1) ruleTop circMiddle2
                   circTop2 (RectangleUnion (ComponentRectangle circComp) enclosingRect) wireSubs4
                   ((DrawComponent circComp) ++ redraws)
  | otherwise = (False, dummyRectangle, [], [], [], [], [])
where
  (connectedPairs, otherPairs)      = filterInOut ((MemberOf (GetOutWireIDs ruleComp)) o fst) bottomPairs1
  (circComp, circMiddle2, circTop2) = FindCompAbove (snd (hd connectedPairs)) circMiddle1 circTop1
  goodMatch                         = EquivalentSpecs ruleComp.spec circComp.spec
  (noDups1, inputPairs, wireSubs2)  = MakeWirePairs ruleComp.inputs circComp.inputs wireSubs1
  (noDups2, outputPairs, wireSubs3) = MakeWirePairs ruleComp.outputs circComp.outputs wireSubs2
  (bottomPairs2, topPairs2)         = RemoveIntersection connectedPairs outputPairs
  (matchingBoxes, bottomPairs3, topPairs3, wireSubs4)
    = MatchCircuits (GetConnectWireID (hd ruleComp.inputs)) ruleBoxCircuit (GetConnectWireID (hd circComp.inputs))
                    (BoxCircuit circComp) (inputPairs ++ bottomPairs2) topPairs2 wireSubs3

SearchTops bottomPairs1 topPairs1 [ruleComp : ruleTop] circMiddle1 circTop1 enclosingRect wireSubs1 redraws
  | (not (IsDummy circComp)) && goodMatch && noDups1 && noDups2
      = SearchTops (otherPairs ++ inputPairs) (topPairs2 ++ topPairs1) ruleTop circMiddle2 circTop2
                   (RectangleUnion (ComponentRectangle circComp) enclosingRect) wireSubs3
                   ((DrawComponent circComp) ++ redraws)
  | otherwise = (False, dummyRectangle, [], [], [], [], [])
where
  (connectedPairs, otherPairs)      = filterInOut ((MemberOf (map GetConnectWireID ruleComp.outputs)) o fst) bottomPairs1
  (circComp, circMiddle2, circTop2) = FindCompAbove (snd (hd connectedPairs)) circMiddle1 circTop1
  goodMatch                         = EquivalentSpecs ruleComp.spec circComp.spec
  (noDups1, inputPairs, wireSubs2)  = MakeWirePairs ruleComp.inputs circComp.inputs wireSubs1
  (noDups2, outputPairs, wireSubs3) = MakeWirePairs ruleComp.outputs circComp.outputs wireSubs2
  topPairs2                         = filter (not o (MemberOf connectedPairs)) outputPairs

SearchTops [] topPairs [] circMiddle circTop enclosingRect wireSubs redraws
  = (True, enclosingRect, circMiddle, circTop, topPairs, wireSubs, redraws)

////////// FindCompAbove
// Takes a wire ID, the middle part of a circuit and the top part of a circuit, and
// returns the component with the wire as an output, the new middle part of the circuit
// (extended as far into the top part as necessary to find the component, with the
// component removed) and the new top part of the circuit (just the part above this
// component).  Returns a dummy component if no such component if found.
FindCompAbove :: WireID !Circuit Circuit -> (Component, Circuit, Circuit)
FindCompAbove wireID [comp : circMiddle1] circTop1
  | any ((==) wireID) (GetOutWireIDs comp)
      = (comp, circMiddle1, circTop1)
  | otherwise
      = (foundComp, [comp : circMiddle2], circTop2)
where
  (foundComp, circMiddle2, circTop2) = FindCompAbove wireID circMiddle1 circTop1

FindCompAbove wireID [] [comp : circTop1]
  | any ((==) wireID) (GetOutWireIDs comp)
      = (comp, [], circTop1)
  | otherwise
      = (foundComp, [comp : circMiddle2], circTop2)
where
  (foundComp, circMiddle2, circTop2) = FindCompAbove wireID [] circTop1

FindCompAbove _ [] [] = (dummyComponent, [], [])

////////// SearchBottoms
// Takes as arguments:
//   - a list of wire ID pairs for wires whose output end has been found but whose input end has not,
//   - a list of wire ID pairs for wires whose input end has been found but whose output end has not,
//   - the bottom part of the left-side of the rule,
//   - the middle part of the circuit (everything below the original component so far found to be
//     parallel to matched components),
//   - the bottom part of the circuit,
//   - the enclosing rectangle for components matched so far,
//   - a list pairing left-side wire IDs with circuit wire IDs,
//   - a list of drawing functions to erase components matched so far.
// Returns:
//   - True if the bottom part of the left-side was matched, False otherwise,
//   - the enclosing rectangle for matched components,
//   - the middle part of the circuit,
//   - the bottom part of the circuit,
//   - a list of wire ID pairs for wires whose input end has been found but whose output end has not,
//   - a list pairing left-side wire IDs with circuit wire IDs,
//   - a list of drawing functions to erase matched components.
SearchBottoms :: [WireSub] [WireSub] !Circuit Circuit Circuit Rectangle [WireSub] [DrawFunction]
                  -> (Bool, Rectangle, Circuit, Circuit, [WireSub], [WireSub], [DrawFunction])

// circMiddle is reversed, circBottom is not
SearchBottoms topPairs bottomPairs [ruleComp=:{spec=EndTerminal, inputs=[{cWireID}]} : ruleBottom]
              circMiddle1 circBottom1 enclosingRect wireSubs1 redraws
  = SearchBottoms otherPairs bottomPairs ruleBottom circMiddle1 circBottom1 enclosingRect wireSubs1 redraws
where
  otherPairs = filter (((<>) cWireID) o fst) topPairs

// Assumes box internal input does not connect outside
SearchBottoms topPairs1 bottomPairs1 [ruleComp=:{spec=Box ruleBoxCircuit} : ruleBottom]
              circMiddle1 circBottom1 enclosingRect wireSubs1 redraws
  | (not (IsDummy circComp)) && goodMatch && noDups1 && noDups2 && matchingBoxes
      = SearchBottoms (otherPairs ++ topPairs2) (bottomPairs2 ++ bottomPairs1) ruleBottom circMiddle2
                      circBottom2 (RectangleUnion (ComponentRectangle circComp) enclosingRect) wireSubs4
                      ((DrawComponent circComp) ++ redraws)
  | otherwise = (False, dummyRectangle, [], [], [], [], [])
where
  (connectedPairs, otherPairs)         = filterInOut ((MemberOf (GetInWireIDs ruleComp)) o fst) topPairs1
  (circComp, circMiddle2, circBottom2) = FindCompBelow (snd (hd connectedPairs)) circMiddle1 circBottom1
  goodMatch                            = EquivalentSpecs ruleComp.spec circComp.spec
  (noDups1, inputPairs, wireSubs2)     = MakeWirePairs ruleComp.inputs circComp.inputs wireSubs1
  (noDups2, outputPairs, wireSubs3)    = MakeWirePairs ruleComp.outputs circComp.outputs wireSubs2
  (matchingBoxes, bottomPairs2, topPairs2, wireSubs4)
    = MatchCircuits (GetConnectWireID (hd ruleComp.inputs)) ruleBoxCircuit (GetConnectWireID (hd circComp.inputs))
                    (BoxCircuit circComp) inputPairs (outputPairs ++ connectedPairs) wireSubs3

SearchBottoms topPairs1 bottomPairs1 [ruleComp : ruleBottom] circMiddle1 circBottom1 enclosingRect wireSubs1 redraws
  | (not (IsDummy circComp)) && goodMatch && noDups1 && noDups2
      = SearchBottoms (otherPairs ++ outputPairs) (bottomPairs1 ++ bottomPairs2) ruleBottom
                      circMiddle2 circBottom2 (RectangleUnion (ComponentRectangle circComp) enclosingRect) wireSubs3
                      ((DrawComponent circComp) ++ redraws)
  | otherwise = (False, dummyRectangle, [], [], [], [], [])
where
  (connectedPairs, otherPairs)         = filterInOut ((MemberOf (map GetConnectWireID ruleComp.inputs)) o fst) topPairs1
  (circComp, circMiddle2, circBottom2) = FindCompBelow (snd (hd connectedPairs)) circMiddle1 circBottom1
  goodMatch                            = EquivalentSpecs ruleComp.spec circComp.spec
  (noDups1, inputPairs, wireSubs2)     = MakeWirePairs ruleComp.inputs circComp.inputs wireSubs1
  (noDups2, outputPairs, wireSubs3)    = MakeWirePairs ruleComp.outputs circComp.outputs wireSubs2
  bottomPairs2                         = filter (not o (MemberOf connectedPairs)) inputPairs

SearchBottoms [] bottomPairs [] circMiddle circBottom enclosingRect wireSubs redraws
  = (True, enclosingRect, circMiddle, circBottom, bottomPairs, wireSubs, redraws)

////////// FindCompBelow
// Takes a wire ID, the middle part of a circuit and the bottom part of a circuit, and
// returns the component with the wire as an input, the new middle part of the circuit
// (extended as far into the bottom part as necessary to find the component, with the
// component removed) and the new bottom part of the circuit (just the part below the
// component).  Returns a dummy component if no such component if found.
FindCompBelow :: WireID !Circuit Circuit -> (Component, Circuit, Circuit)
FindCompBelow wireID [comp : circMiddle1] circBottom1
  | any ((==) wireID) (GetInWireIDs comp)
      = (comp, circMiddle1, circBottom1)
  | otherwise
      = (foundComp, [comp : circMiddle2], circBottom2)
where
  (foundComp, circMiddle2, circBottom2) = FindCompBelow wireID circMiddle1 circBottom1

FindCompBelow wireID [] [comp : circBottom1]
  | any ((==) wireID) (GetInWireIDs comp)
      = (comp, [], circBottom1)
  | otherwise
      = (foundComp, [comp : circMiddle2], circBottom2)
where
  (foundComp, circMiddle2, circBottom2) = FindCompBelow wireID [] circBottom1

FindCompBelow _ [] [] = (dummyComponent, [], [])

////////// SearchMiddles
// Takes as arguments:
//   - a list of wire ID pairs for wires whose output end has been found but whose input end has not,
//   - a list of wire ID pairs for wires whose input end has been found but whose output end has not,
//   - the middle part of the left-side,
//   - the top part of the circuit, reversed (the part above all components matched so far),
//   - the middle part of the circuit (everything parallel to matched components),
//   - the bottom part of the circuit (the part below matched components),
//   - the enclosing rectangle for components matched so far,
//   - a list pairing rule wire IDs with circuit wire IDs for wires matched so far,
//   - a list of drawing functions to erase matched components.
// Returns:
//   - True if it matched, False otherwise,
//   - the enclosing rectangle for matched components,
//   - the top part of the circuit, reversed,
//   - the middle part of the circuit,
//   - the bottom part of the circuit,
//   - a list pairing rule wire IDs with circuit wire IDs for all matched wires,
//   - a list of drawing functions to erase matched components.
//
// Searches repeated down and then up the middle, matching components connected to wires that have
// already been matched (identified by topPairs and bottomPairs), until everything has been matched
// or a mismatch is found.
SearchMiddles :: [WireSub] [WireSub] !Circuit Circuit Circuit Circuit Rectangle [WireSub] [DrawFunction]
                  -> (Bool, Rectangle, Circuit, Circuit, Circuit, [WireSub], [DrawFunction])
SearchMiddles topPairs1 bottomPairs1 ruleMiddle1 circTop1 circMiddle1 circBottom1 enclosingRect1 wireSubs1 redraws1
  | not noProblem1
      = (False, dummyRectangle, [], [], [], [], [])
  | isEmpty ruleMiddle2  // True if everything has been matched
      = (True, enclosingRect2, circTop1, circMiddle2, circBottom2, wireSubs2, redraws2)
  | not noProblem2
      = (False, dummyRectangle, [], [], [], [], [])
  | isEmpty ruleMiddle3  // True if everything has been matched
      = (True, enclosingRect3, circTop2, circMiddle3, circBottom2, wireSubs3, redraws3)
  | otherwise
      = SearchMiddles topPairs2 [] ruleMiddle3 circTop2 circMiddle3 circBottom2 enclosingRect3 wireSubs3 redraws3
where
  (noProblem1, bottomPairs2, ruleMiddle2, circMiddle2, circBottom2, enclosingRect2, wireSubs2, redraws2)
    = SearchDownMiddles topPairs1 bottomPairs1 ruleMiddle1 circMiddle1 circBottom1 [] [] enclosingRect1 wireSubs1 redraws1
  (noProblem2, topPairs2, ruleMiddle3, circMiddle3, circTop2, enclosingRect3, wireSubs3, redraws3)
    = SearchUpMiddles topPairs1 bottomPairs2 ruleMiddle2 circMiddle2 circTop1 [] [] enclosingRect2 wireSubs2 redraws2

////////// SearchDownMiddles
// Takes as arguments:
//   - a list of wire ID pairs for wires whose output end has been found but whose input end has not,
//   - a list of wire ID pairs for wires whose input end has been found but whose output end has not,
//   - the middle part of the left side,
//   - the middle part of the circuit,
//   - the bottom part of the circuit,
//   - a list of new wire ID pairs for wires whose input end has been found but whose output end has not,
//   - the part of the left-side middle that has been searched on this pass, reversed and with matched components removed,
//   - the enclosing rectangle for matched components,
//   - a list pairing rule wire IDs with circuit wire IDs for wires matched so far,
//   - a list of drawing functions to erase matched components.
// Returns:
//   - True if no mismatch was found, False otherwise,
//   - a list of new wire ID pairs for wires whose input end has been found but whose output end has not,
//   - the middle part of the left side, with matched components removed,
//   - the new middle part of the circuit,
//   - the new bottom part of the circuit,
//   - the enclosing rectangle for matched components,
//   - a list pairing rule wire IDs with circuit wire IDs for wires matched so far,
//   - a list of drawing functions to erase matched components.
SearchDownMiddles :: [WireSub] [WireSub] !Circuit Circuit Circuit [WireSub] Circuit Rectangle [WireSub] [DrawFunction]
                      -> (Bool, [WireSub], Circuit, Circuit, Circuit, Rectangle, [WireSub], [DrawFunction])

SearchDownMiddles topPairs1 bottomPairs [ruleComp=:{spec=EndTerminal, inputs=[{cWireID}]} : ruleMiddle]
                  circMiddle circBottom newBottomPairs newRuleMiddle enclosingRect wireSubs redraws
  | isEmpty connectTopPairs
      = SearchDownMiddles topPairs1 bottomPairs ruleMiddle circMiddle circBottom newBottomPairs
                          [ruleComp : newRuleMiddle] enclosingRect wireSubs redraws
  | otherwise
      = SearchDownMiddles topPairs2 bottomPairs ruleMiddle circMiddle circBottom newBottomPairs
                          newRuleMiddle enclosingRect wireSubs redraws
where
  (connectTopPairs, topPairs2) = filterInOut (((==) cWireID) o fst) topPairs1

SearchDownMiddles topPairs1 bottomPairs1 [ruleComp=:{spec=Box ruleBoxCircuit} : ruleMiddle]
                  circMiddle1 circBottom1 newBottomPairs1 newRuleMiddle enclosingRect wireSubs1 redraws
  | isEmpty connectTopPairs
      = SearchDownMiddles topPairs1 bottomPairs1 ruleMiddle circMiddle1 circBottom1
                          newBottomPairs1 [ruleComp : newRuleMiddle] enclosingRect wireSubs1 redraws
  | (not (IsDummy circComp)) && goodMatch && noDups1 && noDups2 && matchingBoxes
      = SearchDownMiddles (topPairs2 ++ otherTopPairs) bottomPairs3 ruleMiddle circMiddle2 circBottom2
                          (newBottomPairs2 ++ newBottomPairs1) newRuleMiddle
                          (RectangleUnion (ComponentRectangle circComp) enclosingRect) wireSubs4
                          ((DrawComponent circComp) ++ redraws)
  | otherwise
      = (False, [], [], [], [], dummyRectangle, [], [])
where
  (connectTopPairs, otherTopPairs)     = filterInOut ((MemberOf (GetInWireIDs ruleComp)) o fst) topPairs1
  (circComp, circMiddle2, circBottom2) = FindCompBelowInMiddle (snd (hd connectTopPairs)) circMiddle1 circBottom1
  goodMatch                            = EquivalentSpecs ruleComp.spec circComp.spec
  (noDups1, inputPairs, wireSubs2)     = MakeWirePairs ruleComp.inputs circComp.inputs wireSubs1
  (noDups2, outputPairs, wireSubs3)    = MakeWirePairs ruleComp.outputs circComp.outputs wireSubs2
  (matchingBoxes, bottomPairs2, topPairs2, wireSubs4)
    = MatchCircuits (GetConnectWireID (hd ruleComp.inputs)) ruleBoxCircuit (GetConnectWireID (hd circComp.inputs))
                    (BoxCircuit circComp) (inputPairs ++ bottomPairs1) (outputPairs ++ connectTopPairs) wireSubs3
  (bottomPairs3, newBottomPairs2)      = filterInOut (MemberOf bottomPairs1) bottomPairs2

SearchDownMiddles topPairs1 bottomPairs1 [ruleComp : ruleMiddle] circMiddle1 circBottom1
                  newBottomPairs1 newRuleMiddle enclosingRect wireSubs1 redraws
  | isEmpty connectTopPairs
      = SearchDownMiddles topPairs1 bottomPairs1 ruleMiddle circMiddle1 circBottom1
                          newBottomPairs1 [ruleComp : newRuleMiddle]
                          enclosingRect wireSubs1 redraws
  | (not (IsDummy circComp)) && goodMatch && noDups1 && noDups2
      = SearchDownMiddles (newTopPairs ++ topPairs2) bottomPairs2 ruleMiddle circMiddle2 circBottom2
                          (newBottomPairs2 ++ newBottomPairs1)
                          newRuleMiddle (RectangleUnion (ComponentRectangle circComp) enclosingRect) wireSubs3
                          ((DrawComponent circComp) ++ redraws)
  | otherwise
      = (False, [], [], [], [], dummyRectangle, [], [])
where
  (connectTopPairs, topPairs2)         = filterInOut ((MemberOf (map GetConnectWireID ruleComp.inputs)) o fst) topPairs1
  (connectBottomPairs, bottomPairs2)   = filterInOut ((MemberOf (map GetConnectWireID ruleComp.outputs)) o fst) bottomPairs1
  (circComp, circMiddle2, circBottom2) = FindCompBelowInMiddle (snd (hd connectTopPairs)) circMiddle1 circBottom1
  goodMatch                            = EquivalentSpecs ruleComp.spec circComp.spec
  (noDups1, inputPairs, wireSubs2)     = MakeWirePairs ruleComp.inputs circComp.inputs wireSubs1
  (noDups2, outputPairs, wireSubs3)    = MakeWirePairs ruleComp.outputs circComp.outputs wireSubs2
  newTopPairs                          = filter (not o (MemberOf connectBottomPairs)) outputPairs
  newBottomPairs2                      = filter (not o (MemberOf connectTopPairs)) inputPairs

SearchDownMiddles _ bottomPairs [] circMiddle circBottom newBottomPairs newRuleMiddle enclosingRect wireSubs redraws
  = (True, newBottomPairs ++ bottomPairs, newRuleMiddle, circMiddle, circBottom, enclosingRect, wireSubs, redraws)

////////// FindCompBelowInMiddle
// Takes a wire ID, the middle part of the circuit and the bottom part of the circuit, finds and removes the
// component with the wire identified by the wire ID as an input, returns the component, the new middle part
// of the circuit (extended into the bottom part if necessary to find the component), and the new bottom part
// of the circuit (just the part below the component).  Returns a dummy component if no such component is found.
FindCompBelowInMiddle :: WireID !Circuit Circuit -> (Component, Circuit, Circuit)
FindCompBelowInMiddle wireID [comp : circMiddle] circBottom
  | any ((==) wireID) (GetInWireIDs comp)
      = (comp, circMiddle, circBottom)
  | otherwise
      = (foundComp, [comp : circMiddle2], circBottom2)
where
  (foundComp, circMiddle2, circBottom2) = FindCompBelowInMiddle wireID circMiddle circBottom

FindCompBelowInMiddle wireID [] [comp : circBottom]
  | any ((==) wireID) (GetInWireIDs comp)
      = (comp, [], circBottom)
  | otherwise
      = (foundComp, [comp : circMiddle2], circBottom2)
where
  (foundComp, circMiddle2, circBottom2) = FindCompBelowInMiddle wireID [] circBottom

FindCompBelowInMiddle _ [] [] = (dummyComponent, [], [])

////////// SearchUpMiddles
// Takes as arguments:
//   - a list of wire ID pairs for wires whose output end has been found but whose input end has not,
//   - a list of wire ID pairs for wires whose input end has been found but whose output end has not,
//   - the middle part of the left side,
//   - the middle part of the circuit,
//   - the top part of the circuit, reversed,
//   - a list of new wire ID pairs for wires whose output end has been found but whose input end has not,
//   - the part of the left-side middle that has been searched on this pass, reversed and with matched components removed,
//   - the enclosing rectangle for matched components,
//   - a list pairing rule wire IDs with circuit wire IDs for wires matched so far,
//   - a list of drawing functions to erase matched components.
// Returns:
//   - True if no mismatch was found, False otherwise,
//   - a list of new wire ID pairs for wires whose output end has been found but whose input end has not,
//   - the middle part of the left side, with matched components removed,
//   - the new middle part of the circuit,
//   - the new top part of the circuit (reversed),
//   - the enclosing rectangle for matched components,
//   - a list pairing rule wire IDs with circuit wire IDs for wires matched so far,
//   - a list of drawing functions to erase matched components.
SearchUpMiddles :: [WireSub] [WireSub] !Circuit Circuit Circuit [WireSub] Circuit Rectangle [WireSub] [DrawFunction]
                     -> (Bool, [WireSub], Circuit, Circuit, Circuit, Rectangle, [WireSub], [DrawFunction])

SearchUpMiddles topPairs bottomPairs1 [ruleComp=:{spec=StartTerminal, outputs=[{cWireID}]} : ruleMiddle]
                circMiddle circTop newTopPairs newRuleMiddle enclosingRect wireSubs redraws
  | isEmpty connectBottomPairs
      = SearchUpMiddles topPairs bottomPairs1 ruleMiddle circMiddle circTop newTopPairs
                        [ruleComp : newRuleMiddle] enclosingRect wireSubs redraws
  | otherwise
      = SearchUpMiddles topPairs bottomPairs2 ruleMiddle circMiddle circTop newTopPairs
                        newRuleMiddle enclosingRect wireSubs redraws
where
  (connectBottomPairs, bottomPairs2) = filterInOut (((==) cWireID) o fst) bottomPairs1

SearchUpMiddles topPairs1 bottomPairs1 [ruleComp=:{spec=Box ruleBoxCircuit} : ruleMiddle]
                circMiddle1 circTop1 newTopPairs1 newRuleMiddle enclosingRect wireSubs1 redraws
  | isEmpty connectBottomPairs
      = SearchUpMiddles topPairs1 bottomPairs1 ruleMiddle circMiddle1 circTop1
                        newTopPairs1 [ruleComp : newRuleMiddle] enclosingRect wireSubs1 redraws
  | (not (IsDummy circComp)) && goodMatch && noDups1 && noDups2 && matchingBoxes
      = SearchUpMiddles topPairs3 (bottomPairs2 ++ otherBottomPairs) ruleMiddle (circTopMiddle ++ circMiddle2) circTop2
                        (newTopPairs2 ++ newTopPairs1) newRuleMiddle
                        (RectangleUnion (ComponentRectangle circComp) enclosingRect) wireSubs4
                        ((DrawComponent circComp) ++ redraws)
  | otherwise
      = (False, [], [], [], [], dummyRectangle, [], [])
where
  (connectBottomPairs, otherBottomPairs) = filterInOut ((MemberOf (GetOutWireIDs ruleComp)) o fst) bottomPairs1
  (circComp, circMiddle2, circTop2, circTopMiddle)
    = FindCompAboveInMiddle (snd (hd connectBottomPairs)) circMiddle1 circTop1 []
  goodMatch                              = EquivalentSpecs ruleComp.spec circComp.spec
  (noDups1, inputPairs, wireSubs2)       = MakeWirePairs ruleComp.inputs circComp.inputs wireSubs1
  (noDups2, outputPairs, wireSubs3)      = MakeWirePairs ruleComp.outputs circComp.outputs wireSubs2
  (matchingBoxes, bottomPairs2, topPairs2, wireSubs4)
    = MatchCircuits (GetConnectWireID (hd ruleComp.inputs)) ruleBoxCircuit (GetConnectWireID (hd circComp.inputs))
                    (BoxCircuit circComp) (inputPairs ++ connectBottomPairs) (outputPairs ++ topPairs1) wireSubs3
  (topPairs3, newTopPairs2)              = filterInOut (MemberOf topPairs1) topPairs2

SearchUpMiddles topPairs1 bottomPairs1 [ruleComp : ruleMiddle] circMiddle1 circTop1
                newTopPairs1 newRuleMiddle enclosingRect wireSubs1 redraws
  | isEmpty connectBottomPairs
      = SearchUpMiddles topPairs1 bottomPairs1 ruleMiddle circMiddle1 circTop1
                        newTopPairs1 [ruleComp : newRuleMiddle]
                        enclosingRect wireSubs1 redraws
  | (not (IsDummy circComp)) && goodMatch && noDups1 && noDups2
      = SearchUpMiddles topPairs2 (newBottomPairs ++ bottomPairs2) ruleMiddle (circTopMiddle ++ circMiddle2) circTop2
                        (newTopPairs2 ++ newTopPairs1) newRuleMiddle
                        (RectangleUnion (ComponentRectangle circComp) enclosingRect) wireSubs3
                        ((DrawComponent circComp) ++ redraws)
  | otherwise
      = (False, [], [], [], [], dummyRectangle, [], [])
where
  (connectTopPairs, topPairs2)       = filterInOut ((MemberOf (map GetConnectWireID ruleComp.inputs)) o fst) topPairs1
  (connectBottomPairs, bottomPairs2) = filterInOut ((MemberOf (map GetConnectWireID ruleComp.outputs)) o fst) bottomPairs1
  (circComp, circMiddle2, circTop2, circTopMiddle)
    = FindCompAboveInMiddle (snd (hd connectBottomPairs)) circMiddle1 circTop1 []
  goodMatch                          = EquivalentSpecs ruleComp.spec circComp.spec
  (noDups1, inputPairs, wireSubs2)   = MakeWirePairs ruleComp.inputs circComp.inputs wireSubs1
  (noDups2, outputPairs, wireSubs3)  = MakeWirePairs ruleComp.outputs circComp.outputs wireSubs2
  newTopPairs2                       = filter (not o (MemberOf connectBottomPairs)) outputPairs
  newBottomPairs                     = filter (not o (MemberOf connectTopPairs)) inputPairs

SearchUpMiddles topPairs _ [] circMiddle circTop newTopPairs newRuleMiddle enclosingRect wireSubs redraws
  = (True, newTopPairs ++ topPairs, newRuleMiddle, circMiddle, circTop, enclosingRect, wireSubs, redraws)

////////// FindCompAboveInMiddle
// Takes a wire ID, the middle part of the circuit, the top part of the circuit (reversed), and the part of
// the top circuit that has been searched, finds and removes the component with the wire identified by the
// wire ID as an output, and returns the component, the new middle part of the circuit (with the component
// removed, if it was there), the new top part of the circuit (just the part above the component, still reversed),
// and the part of the top that was below the component.  Returns a dummy component if no such component is found.
FindCompAboveInMiddle :: WireID !Circuit Circuit Circuit -> (Component, Circuit, Circuit, Circuit)
FindCompAboveInMiddle wireID [comp : circMiddle1] circTop1 []
  | any ((==) wireID) (GetOutWireIDs comp)
      = (comp, circMiddle1, circTop1, [])
  | otherwise
      = (foundComp, [comp : circMiddle2], circTop2, circTopMiddle)
where
  (foundComp, circMiddle2, circTop2, circTopMiddle) = FindCompAboveInMiddle wireID circMiddle1 circTop1 []

FindCompAboveInMiddle wireID [] [comp : circTop1] circTopMiddle1
  | any ((==) wireID) (GetOutWireIDs comp)
      = (comp, [], circTop1, circTopMiddle1)
  | otherwise
      = (foundComp, [], circTop2, circTopMiddle2)
where
  (foundComp, _, circTop2, circTopMiddle2) = FindCompAboveInMiddle wireID [] circTop1 [comp : circTopMiddle1]

FindCompAboveInMiddle _ [] [] _ = (dummyComponent, [], [], [])

////////// RectangleUnion
// Takes two rectangles (the second of which might be a dummy rectangle) and returns the
// smallest rectangle that encloses them both, or just the first rectangle if the second
// one is a dummy.
RectangleUnion :: !Rectangle !Rectangle -> Rectangle
RectangleUnion rect ((0,0),(0,0)) = rect
RectangleUnion ((leftX1, topY1), (rightX1, bottomY1)) ((leftX2, topY2), (rightX2, bottomY2))
  = ((min leftX1 leftX2, min topY1 topY2), (max rightX1 rightX2, max bottomY1 bottomY2))

dummyRectangle :== ((0,0),(0,0))

////////////////////////////////////////

////////// DoBoxRewrite
// Performs the standard box-reduction rewrite, which removes a box and a lolly (leaving the box
// circuit) if they have a connection from the external output of the box to the second input
// of the lolly, using the wire between the box and the lolly as the live wire.  If the wire from
// the box's internal output or the wire between the box and the lolly has a user-specified type,
// the new input wire to the box circuit must have a user-specified type, and if the wire into the
// box's internal input or the wire between the box and the lolly has a user-specified type, the
// new output wire to the box circuit must have a user-specified type.
//
// Takes as arguments:
//   - the ID of the live wire selected in the circuit to identify where the rewrite should take place,
//   - the mouse position that identified the selected wire,
//   - the circuit,
//   - the circuit's wires,
//   - the output position pairs used to keep track of the horizontal positions of the circuit's outputs.
// Returns:
//   - an error value indicating what error, if any, occurred,
//   - the new circuit after the rewrite is performed (if no error occurred),
//   - the new circuit's wires,
//   - a list of drawing functions to erase old components and wires and draw replacements,
//   - the new output position pairs, with wire IDs changed as necessary,
//   - a list giving the old and new wire IDs for the inputs of connections in the circuits
//     whose wire IDs must be changed because of the rewrite (only used after returning from
//     a box),
//   - a list of wire IDs identifying wires from the circuit above the rewrite that must
//     have user-specified types (only used after returning from a box),
//   - a list of wire IDs identifying wires into the circuit below the rewrite that must
//     have user-specified types (only used after returning from a box).
DoBoxRewrite :: WireID Point !Circuit [Wire] [(Int, Connection)]
                 -> (ErrorVal, Circuit, [Wire], [DrawFunction], [(Int, Connection)], [WireSub], [WireID], [WireID])
DoBoxRewrite selectedWireID selectedPoint circuit wires outputPairs
  | not (IsNotError returnCode)
      = (returnCode, [], [], [], [], [], [], [])
  | done
      = (NoError, circuit3A, wires2A, redrawsA, outputPairsA, [], newUserTypesFromCirc2A, [])
  | not (IsBox circComp1)                                      // Upper component of the live wire must be a box.
      = (BadMatch, [], [], [], [], [], [], [])
  | not (selectedWireID==(hd (tl circComp1.outputs)).cWireID)  // The wire must be the external output wire of the box.
      = (BadMatch, [], [], [], [], [], [], [])
  | not (IsLolly circComp2)                                    // Lower component of the live wire must be a lolly.
      = (BadMatch, [], [], [], [], [], [], [])
  | not (selectedWireID==(hd (tl circComp2.inputs)).cWireID)   // The wire must be the second input wire of the lolly.
      = (BadMatch, [], [], [], [], [], [], [])
  | isEmpty boxCirc
      = (NoError, circuit2B, wires6B, redrawsB, outputPairsB, wireChangesB, newUserTypesFromCirc2B, newUserTypesIntoCirc2B)
  | otherwise
      = (NoError, circuit2C, wires7C, redrawsC, outputPairsC, wireChangesC, newUserTypesFromCirc2C, newUserTypesIntoCirc2C)
where
  // If the rewrite has been done inside a box, done=True and newUserTypesFromCirc1A gives a list of wire IDs for
  // wires originating outside the box circuit that need to have user-specified types;  otherwise nothing is done.
  (returnCode, done, circuit2A, wires2A, redrawsA, outputPairsA, newUserTypesFromCirc1A)
    = DoBoxRewriteAux selectedWireID selectedPoint circuit wires outputPairs
  // Sets all possible user-specified types, and returns the wire IDs that weren't found at this level.
  (circuit3A, _, newUserTypesFromCirc2A) = SetUserTypeOnOutputs newUserTypesFromCirc1A circuit2A [] []

  //* The following code applies when DoBoxRewriteAux didn't do the rewrite (meaning that it is at this level).

  // circComp1 should be the box, circComp2 should be the lolly
  (circTop, circComp1, circBottom1)    = FindOutWire selectedWireID circuit  // circTop reversed
  (circMiddle, circComp2, circBottom2) = FindInWire selectedWireID circBottom1

  boxCirc = BoxCircuit circComp1

  (lollyWire1=:{wireLine=(start1, _)}, wires2B)   // The lolly's first input wire
    = RemoveAndReturn (((==) (hd circComp2.inputs).cWireID) o GetWireID) wires
  (lollyWire2=:{wireLine=(_, end2)}, wires3B)     // The lolly's output wire
    = RemoveAndReturn (((==) (hd circComp2.outputs).cWireID) o GetWireID) wires2B
  (boxLollyWire, wires4B)                         // The wire between the box and the lolly
    = RemoveAndReturn (((==) (hd (tl circComp1.outputs)).cWireID) o GetWireID) wires3B
  (boxWire1=:{wireLine=(_, end1)}, wires5B)       // The wire from the box's internal output
    = RemoveAndReturn (((==) (hd circComp1.outputs).cWireID) o GetWireID) wires4B

  //* The following code (up to the next //*) applies when the box circuit is empty - i.e. a
  //* single wire connects its internal connections.

  newUserTypesFromCirc1B
    = if ((not (IsUserType lollyWire1.wireType)) &&
          (any IsUserType [lollyWire2.wireType, boxLollyWire.wireType, boxWire1.wireType])
         )
         [lollyWire1.wireID]
         []
  newUserTypesIntoCirc1B
    = if ((not (IsUserType lollyWire2.wireType)) &&
          (any IsUserType [lollyWire1.wireType, boxLollyWire.wireType, boxWire1.wireType])
         )
         [lollyWire1.wireID]
         []
  newWireB = if (any IsUserType [lollyWire2.wireType, boxLollyWire.wireType, boxWire1.wireType])
                {lollyWire1 & wireLine=(start1, end2), wireType = MakeUserType lollyWire1.wireType}
                {lollyWire1 & wireLine=(start1, end2)}
  wires6B  = [newWireB : wires5B]

  // Erase the box, the lolly, and the old wires, and draw the new wire.
  redrawsB = (DrawWire lollyWire1) ++ (DrawWire lollyWire2) ++ (DrawWire boxLollyWire) ++ (DrawWire boxWire1) ++
             (DrawWire newWireB) ++ (DrawComponent circComp1) ++ (DrawComponent circComp2)

  // Connect the lolly's first input wire with its output wire.
  (inputChangedB, _, circBottom3B, outputPairsB)
    = ChangeInputIDInRewrite lollyWire2.wireID end2 lollyWire1.wireID [] circBottom2 outputPairs
  (wireChangesB, (circBottom4B, _, newUserTypesIntoCirc2B))
    = if inputChangedB
         ([], SetUserTypeOnInputs newUserTypesIntoCirc1B circBottom3B [] [])
         ([(lollyWire2.wireID, lollyWire1.wireID)], (circBottom3B, [], newUserTypesIntoCirc1B))
  (circMiddle2B, circTop2B, newUserTypesFromCirc2B)
    = SetUserTypeOnOutputs newUserTypesFromCirc1B circMiddle circTop []
  circuit2B    = ReverseAppend circTop2B (ReverseAppend circMiddle2B circBottom4B)

  //* The following code applies when the box's circuit is *not* empty.

  (boxWire2=:{wireLine=(start2, _)}, wires6C)  // The wire into the box's internal input
    = RemoveAndReturn (((==) (hd circComp1.inputs).cWireID) o GetWireID) wires5B

  newUserTypesIntoCirc1C
    = if ((not (IsUserType lollyWire2.wireType)) &&
          (any IsUserType [boxWire2.wireType, boxLollyWire.wireType])
         )
         [boxWire2.wireID]
         []
  newUserTypesFromBox1C
    = if ((not (IsUserType boxWire2.wireType)) &&
          (any IsUserType [lollyWire2.wireType, boxLollyWire.wireType])
         )
         [boxWire2.wireID]
         []
  newUserTypesFromCirc1C
    = if ((not (IsUserType lollyWire1.wireType)) &&
          (any IsUserType [boxWire1.wireType, boxLollyWire.wireType])
         )
         [lollyWire1.wireID]
         []
  newUserTypesIntoBox1C
    = if ((not (IsUserType boxWire1.wireType)) &&
          (any IsUserType [lollyWire1.wireType, boxLollyWire.wireType])
         )
         [lollyWire1.wireID]
         []
  newWire1C = if (any IsUserType [boxWire1.wireType, boxLollyWire.wireType])
                 {lollyWire1 & wireLine=(start1, end1), wireType = MakeUserType lollyWire1.wireType}
                 {lollyWire1 & wireLine=(start1, end1)}
  newWire2C = if (any IsUserType [lollyWire2.wireType, boxLollyWire.wireType])
                 {boxWire2 & wireLine=(start2, end2), wireType=MakeUserType boxWire2.wireType}
                 {boxWire2 & wireLine=(start2, end2)}

  wires7C = [newWire1C, newWire2C : wires6C]

  // Connect the lolly's first input wire to the wire out of the box's internal output.
  (_, _, newBoxCirc1C, _) = ChangeInputIDInRewrite boxWire1.wireID end1 lollyWire1.wireID [] boxCirc []

  // Connect the box's external output wire to the lolly's output wire.
  (inputChangedC, _, circBottom3C, outputPairsC)
    = ChangeInputIDInRewrite lollyWire2.wireID end2 boxWire2.wireID [] circBottom2 outputPairs

     //*^* VVV In FILL, nothing can connect down from the box except the lolly, so the
     //*^* VVV "middle" can be placed ahead of the box circuit.  Also, the lolly cannot connect down from
     //*^* VVV the box by the other input, so no cycle can result from the rewrite.
  (wireChangesC, (circBottom4C, _, newUserTypesIntoCirc2C))
    = if inputChangedC
         ([], SetUserTypeOnInputs newUserTypesIntoCirc1C circBottom3C [] [])
         ([(lollyWire2.wireID, boxWire2.wireID)], (circBottom3C, [], newUserTypesIntoCirc1C))
  (circMiddle2C, circTop2C, newUserTypesFromCirc2C)
    = SetUserTypeOnOutputs newUserTypesFromCirc1C circMiddle circTop []
  (newBoxCirc2C, _, _) = SetUserTypeOnInputs newUserTypesIntoBox1C newBoxCirc1C [] []
  (newBoxCirc3C, _, _) = SetUserTypeOnOutputs newUserTypesFromBox1C newBoxCirc2C [] []

  circuit2C    = ReverseAppend circTop2C (ReverseAppend circMiddle2C (newBoxCirc3C ++ circBottom4C))

  // Erase the box, the lolly and the old wires, and draw the new wires.
  redrawsC = (DrawWire boxWire1) ++ (DrawWire boxWire2) ++ (DrawWire boxLollyWire) ++ (DrawWire lollyWire1) ++
             (DrawWire lollyWire2) ++ (DrawWire newWire1C) ++ (DrawWire newWire2C) ++
             (DrawComponent {circComp1 & spec=Box []}) ++ (DrawComponent circComp2)

////////// DoBoxRewrite
// Searches down the circuit, checking to see if the rewrite should be done inside any boxes (determined by
// checking to see if the mouse position is in a box) and calling DoBoxRewrite to do the rewrite if so.
// If a rewrite is done, the wire ID and wire type on the box's internal input may need to be changed
// (as indicated by wireChanges and newUserTypesIntoCircA).
//
// Takes as arguments:
//   - the wire ID of the selected live wire,
//   - the mouse position used to select the live wire,
//   - the circuit,
//   - the circuit's wires,
//   - the circuit's output position pairs, used to keep track of the horizontal positions of its outputs.
// Returns:
//   - an error value indicating what error, if any, occurred,
//   - True if the rewrite was done in a box, False otherwise,
//   - the new circuit if the rewrite was done,
//   - the new circuit's wires,
//   - a list of drawing functions to redraw the circuit,
//   - the circuit's new output position pairs, with wire IDs changed as necessary,
//   - a list of wire IDs identifying wires above the rewrite that need to be changed to have user-specified types.
DoBoxRewriteAux :: WireID Point !Circuit [Wire] [(Int, Connection)]
                    -> (ErrorVal, Bool, Circuit, [Wire], [DrawFunction], [(Int, Connection)], [WireID])

DoBoxRewriteAux liveWireID selectedPoint [box=:{spec=Box boxCircuit, inputs=[inC], pos=RCT boxRect} : circuit]
                wires outputPairs
  | IsInRectangle selectedPoint boxRect
      = if (IsNotError returnCodeA)
           (NoError, True, [{box & spec=Box newBoxCirc, inputs=[newInC2]} : circuit], wires2A, redrawsA, outputPairsA,
            newUserTypesFromCircA
           )
           (returnCodeA, False, [], [], [], [], [])
  | otherwise
      = (returnCodeB, done, [box : newCirc], wires2B, redrawsB, outputPairsB, newUserTypesFromCircB)
where
  (returnCodeA, newBoxCirc, wires2A, redrawsA, outputPairsA, wireChanges, newUserTypesFromCircA, newUserTypesIntoCircA)
    = DoBoxRewrite liveWireID selectedPoint boxCircuit wires outputPairs

   //*^* VVV In FILL, the only wire ID/type that might need to be changed is on the box's internal input
  newInC1 = if (isEmpty wireChanges) inC {inC & cWireID=snd (hd wireChanges)}
  newInC2 = if (isEmpty newUserTypesIntoCircA) newInC1 {newInC1 & cWireType = MakeUserType inC.cWireType}

  (returnCodeB, done, newCirc, wires2B, redrawsB, outputPairsB, newUserTypesFromCircB)
    = DoBoxRewriteAux liveWireID selectedPoint circuit wires outputPairs

DoBoxRewriteAux liveWireID selectedPoint [comp : circuit] wires outputPairs
  = (returnCode, done, [comp : newCirc], wires2, redraws, outputPairs2, newUserTypesFromCirc)
where
  (returnCode, done, newCirc, wires2, redraws, outputPairs2, newUserTypesFromCirc)
    = DoBoxRewriteAux liveWireID selectedPoint circuit wires outputPairs

DoBoxRewriteAux _ _ [] wires outputPairs
  = (NoError, False, [], wires, [], outputPairs, [])

////////// FindInWire
// Finds the component with the wire identified by the wire ID as an input, and
// returns the part of the circuit that is above the component, reversed, the
// component, and the part of the circuit that is below the component.  If the
// component isn't found, it returns a dummy component.
FindInWire :: WireID !Circuit -> (Circuit, Component, Circuit)
FindInWire wireID circuit = FindInWireAux [] circuit
where
  FindInWireAux circuit1 [comp : circuit2]
    | any ((==) wireID) (GetInWireIDs comp)
        = (circuit1, comp, circuit2)
    | otherwise
        = FindInWireAux [comp : circuit1] circuit2
  FindInWireAux _ [] = ([], dummyComponent, [])

