// Contains code used to completely retype a circuit after the deletion
// of a wire or component or the removal or change of a user-specified
// type.  It is assumed that component connections that aren't 
// attached to a wire will have Free types and a User type will be kept
// on both component connections of the wire that carries it.

implementation module typeCircuit

import StdEnv, circuitDefs, utilities

//////// Retype
// Retypes the circuit, returning True if it was successful (all types
// unified), the retyped circuit, the new list of wires, and the
// new next free variable number.
Retype :: !Circuit -> (Bool, Circuit, [Wire], Int)
Retype circuit
  | success1  = if success2
                   (True, newCircuit2, newWires, newNextVar)
                   (False, [], [], 0)
  | otherwise = (False, [], [], 0)
where
  maxVar                              = FindMaxUserTypeVar circuit 0
  (success1, newCircuit1, newNextVar) = ResetVariables circuit (maxVar+1)
  (success2, newCircuit2, newWires)   = ReunifyCircuit newCircuit1

//////// FindMaxUserTypeVar
// Finds the maximum variable number of a UserVar2 in a user-specified type
// (a UserVar2 can occur in a user-specified type that arises from a rewrite).
FindMaxUserTypeVar :: !Circuit Int -> Int

FindMaxUserTypeVar [box=:{spec=Box boxCircuit, inputs, outputs} : circuit] maxVar
  = seq [FindMaxUserTypeVarInConnects inputs, FindMaxUserTypeVarInConnects outputs, FindMaxUserTypeVar boxCircuit,
         FindMaxUserTypeVar circuit
        ] maxVar

FindMaxUserTypeVar [comp=:{inputs, outputs} : circuit] maxVar
  = seq [FindMaxUserTypeVarInConnects inputs, FindMaxUserTypeVarInConnects outputs, FindMaxUserTypeVar circuit] maxVar

FindMaxUserTypeVar [] maxVar = maxVar

//////// FindMaxUserTypeVarInConnects
FindMaxUserTypeVarInConnects :: ![Connection] Int -> Int

FindMaxUserTypeVarInConnects [{cWireType=User type} : connects] maxVar
  = seq [FindMaxVarInType type, FindMaxUserTypeVarInConnects connects] maxVar

FindMaxUserTypeVarInConnects [_ : connects] maxVar
  = FindMaxUserTypeVarInConnects connects maxVar

FindMaxUserTypeVarInConnects [] maxVar = maxVar

//////// ResetVariables
// Resets the types of all components, using variable numbers starting with nextVar.
// User-specified types are not changed, but all Free types are set to the most
// general possible type given the component's type characteristics and any user-
// specified types on its connections.
ResetVariables :: !Circuit Int -> (Bool, Circuit, Int)

ResetVariables [comp=:{spec=Box boxCircuit, inputs, outputs} : circuit] nextVar
  | success1
      = if success2
           (if success3
               (True, [{comp & spec=Box newBoxCircuit, inputs=newInputs, outputs=newOutputs} : newCircuit], newNextVar)
               (False, [], nextVar)
           )
           (False, [], nextVar)
  | otherwise = (False, [], nextVar)
where
  (success1, newInputs, newOutputs, nextVar2) = SetConnectionTypes (Box []) inputs outputs nextVar
  (success2, newBoxCircuit, nextVar3)         = ResetVariables boxCircuit nextVar2
  (success3, newCircuit, newNextVar)          = ResetVariables circuit nextVar3

ResetVariables [comp=:{spec, inputs, outputs} : circuit] nextVar
  | success1  = if success2
                   (True, [{comp & inputs=newInputs, outputs=newOutputs} : newCircuit], newNextVar)
                   (False, [], nextVar)
  | otherwise = (False, [], nextVar)
where
  (success1, newInputs, newOutputs, nextVar2) = SetConnectionTypes spec inputs outputs nextVar
  (success2, newCircuit, newNextVar)          = ResetVariables circuit nextVar2

ResetVariables [] nextVar = (True, [], nextVar)

//////// SetConnectionTypes
// Sets the input and output connections of a component to the most general possible
// types, given the component's type characteristics and any user-specified types.
//*^* SetConnectionTypes :: !CompSpecifics [Connection] [Connection] Int -> (Bool, [Connection], [Connection], Int)
SetConnectionTypes :: !(CompSpecifics Component) [Connection] [Connection] Int -> (Bool, [Connection], [Connection], Int)

SetConnectionTypes StartTerminal [] [outC=:{cWireType=(Free type)}] nextVar
  = (True, [], [{outC & cWireType=(Free (Var nextVar))}], nextVar+1)

SetConnectionTypes StartTerminal [] outputs nextVar
  = (True, [], outputs, nextVar)
  
SetConnectionTypes EndTerminal [inC=:{cWireType=(Free type)}] [] nextVar
  = (True, [{inC & cWireType=(Free (Var nextVar))}], [], nextVar+1)

SetConnectionTypes EndTerminal inputs [] nextVar
  = (True, inputs, [], nextVar)

SetConnectionTypes TensorE inputs outputs nextVar
  | success1  = if success2
                   (True, map (SubsIntoConnect subs2) newInputs, map (SubsIntoConnect subs2) newOutputs, nextVar+2)
                   (False, [], [], nextVar)
  | otherwise = (False, [], [], nextVar)
where
  (success1, newInputs, subs1)  = ChangeTypes inputs [Product (Var nextVar, Var (nextVar+1))] []
  (success2, newOutputs, subs2) = ChangeTypes outputs [Var nextVar, Var (nextVar+1)] subs1

SetConnectionTypes TensorI inputs outputs nextVar
  | success1  = if success2
                   (True, map (SubsIntoConnect subs2) newInputs, map (SubsIntoConnect subs2) newOutputs, nextVar+2)
                   (False, [], [], nextVar)
  | otherwise = (False, [], [], nextVar)
where
  (success1, newInputs, subs1)  = ChangeTypes inputs [Var nextVar, Var (nextVar+1)] []
  (success2, newOutputs, subs2) = ChangeTypes outputs [Product (Var nextVar, Var (nextVar+1))] subs1

SetConnectionTypes SumE inputs outputs nextVar
  | success1  = if success2
                   (True, map (SubsIntoConnect subs2) newInputs, map (SubsIntoConnect subs2) newOutputs, nextVar+2)
                   (False, [], [], nextVar)
  | otherwise = (False, [], [], nextVar)
where
  (success1, newInputs, subs1)  = ChangeTypes inputs [Sum (Var nextVar, Var (nextVar+1))] []
  (success2, newOutputs, subs2) = ChangeTypes outputs [Var nextVar, Var (nextVar+1)] subs1

SetConnectionTypes SumI inputs outputs nextVar
  | success1  = if success2
                   (True, map (SubsIntoConnect subs2) newInputs, map (SubsIntoConnect subs2) newOutputs, nextVar+2)
                   (False, [], [], nextVar)
  | otherwise = (False, [], [], nextVar)
where
  (success1, newInputs, subs1)  = ChangeTypes inputs [Var nextVar, Var (nextVar+1)] []
  (success2, newOutputs, subs2) = ChangeTypes outputs [Sum (Var nextVar, Var (nextVar+1))] subs1

SetConnectionTypes Lolly inputs outputs nextVar
  | success1  = if success2
                   (True, map (SubsIntoConnect subs2) newInputs, map (SubsIntoConnect subs2) newOutputs, nextVar+2)
                   (False, [], [], nextVar)
  | otherwise = (False, [], [], nextVar)
where
  (success1, newInputs, subs1)  = ChangeTypes inputs [Var nextVar, Then (Var nextVar, Var (nextVar+1))] []
  (success2, newOutputs, subs2) = ChangeTypes outputs [Var (nextVar+1)] subs1

SetConnectionTypes UnitEL inputs outputs nextVar
  | success1  = if success2
                   (True, map (SubsIntoConnect subs2) newInputs, map (SubsIntoConnect subs2) newOutputs, nextVar+1)
                   (False, [], [], nextVar)
  | otherwise = (False, [], [], nextVar)
where
  (success1, newInputs, subs1)  = ChangeTypes inputs [Unit, Var nextVar] []
  (success2, newOutputs, subs2) = ChangeTypes outputs [Var nextVar] subs1

SetConnectionTypes UnitER inputs outputs nextVar
  | success1  = if success2
                   (True, map (SubsIntoConnect subs2) newInputs, map (SubsIntoConnect subs2) newOutputs, nextVar+1)
                   (False, [], [], nextVar)
  | otherwise = (False, [], [], nextVar)
where
  (success1, newInputs, subs1)  = ChangeTypes inputs [Var nextVar, Unit] []
  (success2, newOutputs, subs2) = ChangeTypes outputs [Var nextVar] subs1

SetConnectionTypes CounitIL inputs outputs nextVar
  | success1  = if success2
                   (True, map (SubsIntoConnect subs2) newInputs, map (SubsIntoConnect subs2) newOutputs, nextVar+1)
                   (False, [], [], nextVar)
  | otherwise = (False, [], [], nextVar)
where
  (success1, newInputs, subs1)  = ChangeTypes inputs [Var nextVar] []
  (success2, newOutputs, subs2) = ChangeTypes outputs [Counit, Var nextVar] subs1

SetConnectionTypes CounitIR inputs outputs nextVar
  | success1  = if success2
                   (True, map (SubsIntoConnect subs2) newInputs, map (SubsIntoConnect subs2) newOutputs, nextVar+1)
                   (False, [], [], nextVar)
  | otherwise = (False, [], [], nextVar)
where
  (success1, newInputs, subs1)  = ChangeTypes inputs [Var nextVar] []
  (success2, newOutputs, subs2) = ChangeTypes outputs [Var nextVar, Counit] subs1

SetConnectionTypes (Box _) inputs outputs nextVar
  | success1  = if success2
                   (True, map (SubsIntoConnect subs2) newInputs, map (SubsIntoConnect subs2) newOutputs, nextVar+2)
                   (False, [], [], nextVar)
  | otherwise = (False, [], [], nextVar)
where
  (success1, newInputs, subs1)  = ChangeTypes inputs [Var (nextVar+1)] []
  (success2, newOutputs, subs2) = ChangeTypes outputs [Var nextVar, Then (Var nextVar, Var (nextVar+1))] subs1

SetConnectionTypes (Generic _ inputTypes outputTypes) inputs outputs nextVar
  | success1  = if success2
                   (True, map (SubsIntoConnect subs2) newInputs, map (SubsIntoConnect subs2) newOutputs, maxVar2+1)
                   (False, [], [], nextVar)
  | otherwise = (False, [], [], nextVar)
where
  (newInputTypes, maxVar1)      = AddToVarsInTypes nextVar inputTypes (nextVar-1)
  (newOutputTypes, maxVar2)     = AddToVarsInTypes nextVar outputTypes maxVar1
  (success1, newInputs, subs1)  = ChangeTypes inputs newInputTypes []
  (success2, newOutputs, subs2) = ChangeTypes outputs newOutputTypes subs1

SetConnectionTypes _ inputs outputs nextVar    // for UnitI and CounitE
  = (True, inputs, outputs, nextVar)

//////// ChangeTypes
// Changes all free connection types to the types given in the list of types,
// and returns the list of substitutions necessary to make free types in
// the connections match user types.
ChangeTypes :: ![Connection] ![Type] [Substitution] -> (Bool, [Connection], [Substitution])

ChangeTypes [connect=:{cWireType=(User type1)} : connects] [type2 : types] subs1
  | matched   = if success
                   (True, [{connect & cWireType=(User type1)} : newConnects], newSubs)
                   (False, [], [])
  | otherwise = (False, [], [])
where
  (matched, subs2)                = MatchTypes type1 type2 subs1
  (success, newConnects, newSubs) = ChangeTypes connects types subs2

ChangeTypes [connect : connects] [type : types] subs
  | success   = (True, [{connect & cWireType=(Free type)} : newConnects], newSubs)
  | otherwise = (False, [], [])
where
  (success, newConnects, newSubs) = ChangeTypes connects types subs

ChangeTypes [] [] subs = (True, [], subs)

//////// ReunifyCircuit
// Reunifies a circuit from scratch, returning True if successful with the
// new circuit and wires.
ReunifyCircuit :: !Circuit -> (Bool, Circuit, [Wire])
ReunifyCircuit circuit = UnifyAndSubstitute (GetConnectionPairs circuit) circuit []

//////// UnifyAndSubstitute     // Assumes that user types are the same at each connection point of the wire
// Takes a list that gives wire IDs and the two endpoint types and positions for every
// wire in the circuit, unifies the two types for each wire, does the necessary substitutions,
// and returns the new circuit and wires if successful.
UnifyAndSubstitute :: ![(WireID, (WireType, Point), (WireType, Point))] Circuit [Wire] -> (Bool, Circuit, [Wire])

UnifyAndSubstitute [(id, (Free outType, start), (Free inType, end)) : connects] circuit wires
  | success   = UnifyAndSubstitute (map (SubsIntoPair subs) connects) (SubsIntoCircuit subs circuit)
                                   [newWire : SubsIntoWires subs wires]
  | otherwise = (False, [], [])
where
  (success, subs) = Unify outType inType
  newWire = {wireID=id, wireLine=(start, end), wireType=(Free (SubsIntoType subs outType))}

UnifyAndSubstitute [(id, (User outType, start), (User inType, end)) : connects] circuit wires
  | EqualTypes outType inType  //*^* Debugging check - should always be true
      = UnifyAndSubstitute connects circuit [{wireID=id, wireLine=(start, end), wireType=(User outType)} : wires]

UnifyAndSubstitute [] circuit wires = (True, circuit, wires)

//////// SubsIntoPairs
SubsIntoPair :: [Substitution] !(WireID, !(WireType, Point), !(WireType, Point))
                 -> (WireID, (WireType, Point), (WireType, Point))
SubsIntoPair subs (id, (outwireType, start), (inwireType, end))
  = (id, (SubsIntoWireType subs outwireType, start), (SubsIntoWireType subs inwireType, end))

//////// GetConnectionPairs
// Gets a list with the wire ID and the endpoint types and positions for every wire 
// in the circuit.
GetConnectionPairs :: !Circuit -> [(WireID, (WireType, Point), (WireType, Point))]
GetConnectionPairs circuit
  | isEmpty leftovers  //*^* Debugging check
      = flatten pairs
where
  (pairs, leftovers) = GetConnectionPairsAux [] circuit

//////// GetConnectionPairsAux
GetConnectionPairsAux :: [(WireID, (WireType, Point))] !Circuit
                         -> ([[(WireID, (WireType, Point), (WireType, Point))]], [(WireID, (WireType, Point))])

GetConnectionPairsAux outCs [comp=:{spec=Box boxCircuit, pos, inputs, outputs=[outC1, outC2]} : circuit]
  = ([pairs2 : (pairs1 ++ pairs3)], newOutCs3)
where
  (pairs1, newOutCs1) = GetConnectionPairsAux ((GetOutInfo (Box boxCircuit) pos [outC1] 0) ++ outCs) boxCircuit
  (pairs2, newOutCs2) = GetPairs (Box boxCircuit) pos inputs 0 newOutCs1
  (pairs3, newOutCs3) = GetConnectionPairsAux ((GetOutInfo (Box boxCircuit) pos [outC2] 1) ++ newOutCs2) circuit

GetConnectionPairsAux outCs [comp=:{spec, pos, inputs, outputs} : circuit]
  = ([pairs1 : pairs2], newOutCs2)
where
  (pairs1, newOutCs1) = GetPairs spec pos inputs 0 outCs
  (pairs2, newOutCs2) = GetConnectionPairsAux ((GetOutInfo spec pos outputs 0) ++ newOutCs1) circuit

GetConnectionPairsAux outCs [] = ([], outCs)

//////// GetOutInfo
// Given a list of output connections, returns a list with the wire ID, type and endpoint
// for each output.
GetOutInfo :: (CompSpecifics Component) Placement ![Connection] Int -> [(WireID, (WireType, Point))]
GetOutInfo spec pos [{cWireID, cWireType} : outCs] n
  | cWireID==(-1)
      = GetOutInfo spec pos outCs (n+1)
  | otherwise
      = [(cWireID, (cWireType, OutConnectionPoint spec pos n)) : GetOutInfo spec pos outCs (n+1)]
GetOutInfo _ _ [] _ = []

//////// GetPairs
// Given a list of input connections and a list with the wire ID, type and position for all output
// connections encountered in the circuit so far, returns a list with the wire ID and endpoint types
// and positions for all corresponding wires and the new list of information about unmatched output
// connections.
//*^* GetPairs :: CompSpecifics Placement ![Connection] Int [(WireID, (WireType, Point))]
GetPairs :: (CompSpecifics Component) Placement ![Connection] Int [(WireID, (WireType, Point))]
            -> ([(WireID, (WireType, Point), (WireType, Point))], [(WireID, (WireType, Point))])

GetPairs spec pos [{cWireID, cWireType} : inCs] n outCs
  | cWireID==(-1) = GetPairs spec pos inCs (n+1) outCs
  | otherwise     = ([(cWireID, (outWireType, start), (cWireType, InConnectionPoint spec pos n)) : pairs], newOutCs2)
where
  ((_, (outWireType, start)), newOutCs1) = RemoveAndReturn (((==) cWireID) o fst) outCs
  (pairs, newOutCs2) = GetPairs spec pos inCs (n+1) newOutCs1

GetPairs _ _ [] _ outCs = ([], outCs)
