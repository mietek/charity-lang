// Contains code that determines whether two circuits are the same, starting
// from a pair of corresponding wires in the two circuits.

implementation module circuitWireMatch

import StdEnv
import circuitDefs, rewriteDefs, utilities, circuitSave

////////// MatchCircuits
// Takes as arguments:
//   - the wire ID from the first circuit,
//   - the first circuit,
//   - the wire ID from the second circuit,
//   - the second circuit,
//   - a list of matched wire ID pairs identifying wires from below the circuit that may connect into it,
//   - a list of matched wire ID pairs identifying wires from above the circuit that may connect into it,
//   - a list of wire ID pairs for all wires matched so far.
// Returns:
//   - True if the circuits are equal, False otherwise,
//   - a list of matched wire ID pairs identifying the wires from below or in the circuit that connect above it,
//   - a list of matched wire ID pairs identifying the wires from above or in the circuit that connect below it,
//   - a list of wire ID pairs for all wires matched.
// NOTE:  Does not check for equivalency of types.
MatchCircuits :: WireID !Circuit WireID Circuit [WireSub] [WireSub] [WireSub] -> (Bool, [WireSub], [WireSub], [WireSub])
MatchCircuits ruleWireID rule circWireID circuit bottomPairs1 topPairs1 wireSubs
  | isEmpty rule
      = if (isEmpty circuit)
           (True, bottomPairs2, topPairs2, wireSubs)
           (False, [], [], [])
  | IsDummy ruleComp1  // This will happen if the wire connects to an input in the circuit, but not an output.
      = MatchCircuitsAux ruleComp2 ruleTop2 ruleBottom2 circComp2 circTop2 circBottom2 bottomPairs2 topPairs2 wireSubs
  | otherwise
      = MatchCircuitsAux ruleComp1 ruleTop1 ruleBottom1 circComp1 circTop1 circBottom1 bottomPairs1 topPairs1 wireSubs
where
  (bottomPairs2, topPairs2)          = RemoveIntersection bottomPairs1 topPairs1

  (ruleTop1, ruleComp1, ruleBottom1) = FindOutWireInCircuit ruleWireID rule    // Splits the circuit at the component,
  (circTop1, circComp1, circBottom1) = FindOutWireInCircuit circWireID circuit // returning a dummy if it is not found.

  (ruleTop2, ruleComp2, ruleBottom2) = FindInWireInCircuit ruleWireID rule
  (circTop2, circComp2, circBottom2) = FindInWireInCircuit circWireID circuit

////////// MatchCircuitsAux
// Uses a depth-first search of the first circuit to match it to the second.
// Takes as arguments:
//   - a component from the first circuit,
//   - the part of the first circuit that is above the component (reversed),
//   - the part of the first circuit that is below the component,
//   - the corresponding component in the second circuit,
//   - the part of the second circuit that is above the component (reversed),
//   - the part of the second circuit that is below the component (reversed),
//   - a list of matched wire ID pairs identifying wires from below the circuit that may connect into it,
//   - a list of matched wire ID pairs identifying wires from above the circuit that may connect into it,
//   - a list of wire ID pairs for all wires matched so far.
// Returns:
//   - True if the circuits are equal, False otherwise,
//   - a list of matched wire ID pairs identifying the wires from below or in the circuit that connect above it,
//   - a list of matched wire ID pairs identifying the wires from above or in the circuit that connect below it,
//   - a list of wire ID pairs for all wires matched.
MatchCircuitsAux :: !Component Circuit Circuit !Component Circuit Circuit [WireSub] [WireSub] [WireSub]
                     -> (Bool, [WireSub], [WireSub], [WireSub])

MatchCircuitsAux ruleComp=:{spec=Box ruleBoxCircuit} ruleTop ruleBottom
                 circComp=:{spec=Box circBoxCircuit} circTop circBottom bottomPairs1 topPairs1 wireSubs1
  | goodMatch && boxesMatch && noProblem1 && noProblem2 && (isEmpty (flatten circTops3)) && (isEmpty (flatten circBottoms3))
      = (True, bottomPairs4, topPairs4, wireSubs5)
  | otherwise
      = (False, [], [], [])
where
  (goodMatch, inputPairs1, outputPairs1, wireSubs2) = CheckMatch ruleComp circComp wireSubs1
  (boxesMatch, inputPairs2, outputPairs2, wireSubs3)
    = MatchCircuits (GetConnectWireID (hd ruleComp.inputs)) ruleBoxCircuit
                    (GetConnectWireID (hd circComp.inputs)) circBoxCircuit
                    inputPairs1 outputPairs1 wireSubs2
  (topPairs2, newBottomPairs) = RemoveIntersection topPairs1 inputPairs2
  (bottomPairs2, newTopPairs) = RemoveIntersection bottomPairs1 outputPairs2
  (noProblem1, ruleTops2, ruleBottoms2, circTops2, circBottoms2, bottomPairs3, topPairs3, wireSubs4)
    = GoAlongInputs inputPairs2 [ruleTop] [ruleBottom] [circTop] [circBottom]
                    (newBottomPairs ++ bottomPairs2) (newTopPairs ++ topPairs2) wireSubs3
  (noProblem2, _, _, circTops3, circBottoms3, bottomPairs4, topPairs4, wireSubs5)
    = GoAlongOutputs outputPairs2 ruleTops2 ruleBottoms2 circTops2 circBottoms2 bottomPairs3 topPairs3 wireSubs4

MatchCircuitsAux ruleComp ruleTop ruleBottom circComp circTop circBottom bottomPairs1 topPairs1 wireSubs1
  | goodMatch && noProblem1 && noProblem2 && (isEmpty (flatten circTops3)) && (isEmpty (flatten circBottoms3))
      = (True, bottomPairs4, topPairs4, wireSubs4)
  | otherwise
      = (False, [], [], [])
where
  (goodMatch, inputPairs, outputPairs, wireSubs2) = CheckMatch ruleComp circComp wireSubs1
  (bottomPairs2, newTopPairs)                     = RemoveIntersection bottomPairs1 outputPairs
  (topPairs2, newBottomPairs)                     = RemoveIntersection topPairs1 inputPairs
  (noProblem1, ruleTops2, ruleBottoms2, circTops2, circBottoms2, bottomPairs3, topPairs3, wireSubs3)
    = GoAlongInputs inputPairs [ruleTop] [ruleBottom] [circTop] [circBottom]
                    (newBottomPairs ++ bottomPairs2) (newTopPairs ++ topPairs2) wireSubs2
  (noProblem2, _, _, circTops3, circBottoms3, bottomPairs4, topPairs4, wireSubs4)
    = GoAlongOutputs outputPairs ruleTops2 ruleBottoms2 circTops2 circBottoms2 bottomPairs3 topPairs3 wireSubs3

////////// GoAlongInputs
// This function matches the two circuits by travelling up along all of the wires identified by the first
// list of wire ID pairs.
// Takes as arguments:
//   - a list of wire ID pairs to be searched along,
//   - the part of the first circuit that is above the component whose input wires are being travelled,
//     saved as a list of lists of components in reverse order (the break between each pair of 
//     lists identifies the former position of a component that has already been visited in the search),
//   - the part of the first circuit that is below the current component,
//   - the part of the second circuit that is above the current component,
//   - the part of the second circuit that is below the current component,
//   - a list of wire ID pairs identifying wires whose starting point hasn't been found,
//   - a list of wire ID pairs identifying wires whose ending point hasn't been found,
//   - a list of wire ID pairs for all wires matched so far.
// Returns:
//   - True if it doesn't find any mismatch between the circuits, False otherwise,
//   - the part of the first circuit that is above the current component, with matched components removed,
//   - the part of the first circuit that is below the current component, with matched components removed,
//   - the part of the second circuit that is above the current component, with matched components removed,
//   - the part of the second circuit that is below the current component, with matched components removed,
//   - a list of wire ID pairs identifying wires whose starting point hasn't been found,
//   - a list of wire ID pairs identifying wires whose ending point hasn't been found,
//   - a list of wire ID pairs for all wires matched so far.
GoAlongInputs :: ![(WireID, WireID)] [Circuit] [Circuit] [Circuit] [Circuit] [WireSub] [WireSub] [WireSub]
                   -> (Bool, [Circuit], [Circuit], [Circuit], [Circuit], [WireSub], [WireSub], [WireSub])
GoAlongInputs [(ruleInput, circInput) : inputPairs] ruleTops ruleBottoms
              circTops circBottoms bottomPairs topPairs wireSubs1
  | IsDummy ruleComp  // The component has already been matched and removed from the circuit
      = GoAlongInputs inputPairs ruleTops ruleBottoms circTops circBottoms bottomPairs topPairs wireSubs1
  | noProblem1
      = GoAlongInputs inputPairs ruleTops3 ruleBottoms3 circTops3 circBottoms3 bottomPairs2 topPairs2 wireSubs2
  | otherwise
      = (False, [], [], [], [], [], [], [])
where
  (ruleTopsB, ruleComp, ruleTopsA) = FindOutWireInCircs ruleInput ruleTops
  (circTopsB, circComp, circTopsA) = FindOutWireInCircs circInput circTops
  (noProblem1, ruleTops2, ruleBottoms2, circTops2, circBottoms2, bottomPairs2, topPairs2, wireSubs2)
    = GoFromOutput (ruleInput, circInput) ruleTopsA ruleComp (ruleTopsB ++ ruleBottoms)
                   circTopsA circComp (circTopsB ++ circBottoms) bottomPairs topPairs wireSubs1
  // StitchAndTransfer is used to return the two circuits to the positions that they were at before
  // the call to GoFromOutput by returning what remains of the parts of the circuits below and above
  // the current components, and gluing together the lists that were split apart when ruleComp and
  // circComp were found.
  (ruleBottoms3, ruleTops3)        = StitchAndTransfer (length ruleTopsB) ruleBottoms2 ruleTops2
  (circBottoms3, circTops3)        = StitchAndTransfer (length circTopsB) circBottoms2 circTops2

GoAlongInputs [] ruleTops ruleBottoms circTops circBottoms bottomPairs topPairs wireSubs
  = (True, ruleTops, ruleBottoms, circTops, circBottoms, bottomPairs, topPairs, wireSubs)

////////// FindOutWireInCircs
// Takes as arguments:
//   - a wire ID identifying the wire to be found,
//   - a list of lists of components representing a circuit,
// Returns:
//   - everything in the circuit that was before the component 
//     with the given wire as an output, in reverse order, 
//     including the front part of the list in which the component
//     was found,
//   - the component with the wire as an output (or a dummy component,
//     if no such component is found),
//   - everything in the circuit that was after the component, including
//     the back of the list in which the component was found.
FindOutWireInCircs :: WireID ![Circuit] -> ([Circuit], Component, [Circuit])
FindOutWireInCircs wireID circuits = FindOutWireInCircsAux [] circuits
where
  FindOutWireInCircsAux circuits1 [circuit : circuits2]
    | IsDummy comp = FindOutWireInCircsAux [front : circuits1] circuits2
    | otherwise    = ([front : circuits1], comp, [back : circuits2])
  where
    (front, comp, back) = FindOutWireInCircuit wireID circuit
  FindOutWireInCircsAux circuits1 [] = (circuits1, dummyComponent, [])

////////// FindOutWireInCircuit
FindOutWireInCircuit :: WireID !Circuit -> (Circuit, Component, Circuit)
FindOutWireInCircuit wireID circuit = FindOutWireInCircuitAux [] circuit
where
  FindOutWireInCircuitAux circuit1 [comp : circuit2]
    | any ((==) wireID) (GetOutWireIDs comp)
        = (circuit1, comp, circuit2)
    | otherwise
        = FindOutWireInCircuitAux [comp : circuit1] circuit2
  FindOutWireInCircuitAux circuit1 [] = (circuit1, dummyComponent, [])

////////// GoFromOutput
// Matches the two circuits starting at a pair of corresponding components that were
// reached by an output to the two components (which will not be included in the search).
// Takes as arguments:
//   - a wire ID pair identifying the output on each component,
//   - a list of lists of components representing the part of the first circuit that is above the current component,
//     in reverse order,
//   - the current component in the first circuit,
//   - a list of lists of components representing the part of the first circuit that is below the current component,
//   - a list of lists of components representing the part of the second circuit that is above the current component,
//     in reverse order,
//   - the current component in the second circuit,
//   - a list of lists of components representing the part of the second circuit that is below the current component,
//   - a list of wire ID pairs identifying wires whose starting point hasn't been found,
//   - a list of wire ID pairs identifying wires whose ending point hasn't been found,
//   - a list of wire ID pairs for all wires matched so far.
// Returns:
//   - True if it doesn't find any mismatch between the circuits, False otherwise,
//   - the part of the first circuit that is above the current component, with matched components removed,
//   - the part of the first circuit that is below the current component, with matched components removed,
//   - the part of the second circuit that is above the current component, with matched components removed,
//   - the part of the second circuit that is below the current component, with matched components removed,
//   - a list of wire ID pairs identifying wires whose starting point hasn't been found,
//   - a list of wire ID pairs identifying wires whose ending point hasn't been found,
//   - a list of wire ID pairs for all wires matched so far.
GoFromOutput :: WireSub [Circuit] !Component [Circuit] [Circuit] !Component [Circuit] [WireSub] [WireSub] [WireSub]
                 -> (Bool, [Circuit], [Circuit], [Circuit], [Circuit], [WireSub], [WireSub], [WireSub])

GoFromOutput fromWirePair ruleTops ruleComp=:{spec=Box ruleBoxCircuit} ruleBottoms
             circTops circComp circBottoms bottomPairs1 topPairs1 wireSubs1
  | goodMatch && boxesMatch && noProblem1 && noProblem2
      = (True, ruleTops3, ruleBottoms3, circTops3, circBottoms3, bottomPairs4, topPairs4, wireSubs5)
  | otherwise
      = (False, [], [], [], [], [], [], [])
where
  (goodMatch, inputPairs1, outputPairs1, wireSubs2) = CheckMatch ruleComp circComp wireSubs1
  (boxesMatch, inputPairs2, outputPairs2, wireSubs3)
    = MatchCircuits (GetConnectWireID (hd ruleComp.inputs)) ruleBoxCircuit
                    (GetConnectWireID (hd circComp.inputs)) (BoxCircuit circComp)
                    inputPairs1 outputPairs1 wireSubs2

  (bottomPairs2, newTopPairs)                     = RemoveIntersection bottomPairs1 outputPairs2
  (topPairs2, newBottomPairs)                     = RemoveIntersection topPairs1 inputPairs2

  (noProblem1, ruleTops2, ruleBottoms2, circTops2, circBottoms2, bottomPairs3, topPairs3, wireSubs4)
    = GoAlongInputs inputPairs2 ruleTops ruleBottoms circTops circBottoms
                    (newBottomPairs ++ bottomPairs2) (newTopPairs ++ topPairs2) wireSubs3
  (noProblem2, ruleTops3, ruleBottoms3, circTops3, circBottoms3, bottomPairs4, topPairs4, wireSubs5)
    = GoAlongOutputs (filter ((<>) fromWirePair) outputPairs2) ruleTops2 ruleBottoms2 circTops2 circBottoms2
                     bottomPairs3 topPairs3 wireSubs4

GoFromOutput fromWirePair ruleTops ruleComp ruleBottoms circTops circComp circBottoms bottomPairs1 topPairs1 wireSubs1
  | goodMatch && noProblem1 && noProblem2
      = (True, ruleTops3, ruleBottoms3, circTops3, circBottoms3, bottomPairs4, topPairs4, wireSubs4)
  | otherwise
      = (False, [], [], [], [], [], [], [])
where
  (goodMatch, inputPairs, outputPairs, wireSubs2) = CheckMatch ruleComp circComp wireSubs1
  (bottomPairs2, newTopPairs)                     = RemoveIntersection bottomPairs1 outputPairs
  (topPairs2, newBottomPairs)                     = RemoveIntersection topPairs1 inputPairs

  (noProblem1, ruleTops2, ruleBottoms2, circTops2, circBottoms2, bottomPairs3, topPairs3, wireSubs3)
    = GoAlongInputs inputPairs ruleTops ruleBottoms circTops circBottoms
                    (newBottomPairs ++ bottomPairs2) (newTopPairs ++ topPairs2) wireSubs2
  (noProblem2, ruleTops3, ruleBottoms3, circTops3, circBottoms3, bottomPairs4, topPairs4, wireSubs4)
    = GoAlongOutputs (filter ((<>) fromWirePair) outputPairs) ruleTops2 ruleBottoms2 circTops2 circBottoms2
                     bottomPairs3 topPairs3 wireSubs3

////////// GoAlongOutputs
// Exactly like GoAlongInputs, except that it travels down along the output wires of the current component.
GoAlongOutputs :: ![WireSub] [Circuit] [Circuit] [Circuit] [Circuit] [WireSub] [WireSub] [WireSub]
                   -> (Bool, [Circuit], [Circuit], [Circuit], [Circuit], [WireSub], [WireSub], [WireSub])
GoAlongOutputs [(ruleOutput, circOutput) : outputPairs] ruleTops ruleBottoms
               circTops circBottoms bottomPairs topPairs wireSubs1
  | IsDummy ruleComp
      = GoAlongOutputs outputPairs ruleTops ruleBottoms circTops circBottoms bottomPairs topPairs wireSubs1
  | noProblem1
      = GoAlongOutputs outputPairs ruleTops3 ruleBottoms3 circTops3 circBottoms3 bottomPairs2 topPairs2 wireSubs2
  | otherwise
      = (False, [], [], [], [], [], [], [])
where
  (ruleBottomsA, ruleComp, ruleBottomsB) = FindInWireInCircs ruleOutput ruleBottoms
  (circBottomsA, circComp, circBottomsB) = FindInWireInCircs circOutput circBottoms
  (noProblem1, ruleTops2, ruleBottoms2, circTops2, circBottoms2, bottomPairs2, topPairs2, wireSubs2)
    = GoFromInput (ruleOutput, circOutput) (ruleBottomsA ++ ruleTops) ruleComp ruleBottomsB
                  (circBottomsA ++ circTops) circComp circBottomsB bottomPairs topPairs wireSubs1
  // StitchAndTransfer is used to return the two circuits to the positions that they were at before
  // the call to GoFromInput by returning what remains of the parts of the circuits below and above
  // the current components, and gluing together the lists that were split apart when ruleComp and
  // circComp were found.
  (ruleTops3, ruleBottoms3) = StitchAndTransfer (length ruleBottomsA) ruleTops2 ruleBottoms2
  (circTops3, circBottoms3) = StitchAndTransfer (length circBottomsA) circTops2 circBottoms2

GoAlongOutputs [] ruleTops ruleBottoms circTops circBottoms bottomPairs topPairs wireSubs
  = (True, ruleTops, ruleBottoms, circTops, circBottoms, bottomPairs, topPairs, wireSubs)

////////// FindInWireInCircs
// Like FindOutWireInCircs, except that it searches for a component with the given wire as an input.
FindInWireInCircs :: WireID ![Circuit] -> ([Circuit], Component, [Circuit])
FindInWireInCircs wireID circuits = FindInWireInCircsAux [] circuits
where
  FindInWireInCircsAux circuits1 [circuit : circuits2]
    | IsDummy comp = FindInWireInCircsAux [top : circuits1] circuits2
    | otherwise    = ([top : circuits1], comp, [bottom : circuits2])
  where
    (top, comp, bottom) = FindInWireInCircuit wireID circuit
  FindInWireInCircsAux circuits1 [] = (circuits1, dummyComponent, [])

////////// FindInWireInCircuit
FindInWireInCircuit :: WireID !Circuit -> (Circuit, Component, Circuit)
FindInWireInCircuit wireID circuit = FindInWireInCircuitAux [] circuit
where
  FindInWireInCircuitAux circuit1 [comp : circuit2]
    | any ((==) wireID) (GetInWireIDs comp)
        = (circuit1, comp, circuit2)
    | otherwise
        = FindInWireInCircuitAux [comp : circuit1] circuit2
  FindInWireInCircuitAux circuit1 [] = (circuit1, dummyComponent, [])

////////// GoFromInput
// Like GoFromOutput.
GoFromInput :: WireSub [Circuit] !Component [Circuit] [Circuit] !Component [Circuit] [WireSub] [WireSub] [WireSub]
                -> (Bool, [Circuit], [Circuit], [Circuit], [Circuit], [WireSub], [WireSub], [WireSub])
GoFromInput fromWirePair ruleTops ruleComp=:{spec=Box ruleBoxCircuit} ruleBottoms
            circTops circComp circBottoms bottomPairs1 topPairs1 wireSubs1
  | goodMatch && boxesMatch && noProblem1 && noProblem2
      = (True, ruleTops3, ruleBottoms3, circTops3, circBottoms3, bottomPairs4, topPairs4, wireSubs5)
  | otherwise
      = (False, [], [], [], [], [], [], [])
where
  (goodMatch, inputPairs1, outputPairs1, wireSubs2) = CheckMatch ruleComp circComp wireSubs1
  (boxesMatch, inputPairs2, outputPairs2, wireSubs3)
    = MatchCircuits (GetConnectWireID (hd ruleComp.inputs)) ruleBoxCircuit
                    (GetConnectWireID (hd circComp.inputs)) (BoxCircuit circComp)
                    inputPairs1 outputPairs1 wireSubs2

  (bottomPairs2, newTopPairs) = RemoveIntersection bottomPairs1 outputPairs2
  (topPairs2, newBottomPairs) = RemoveIntersection topPairs1 inputPairs2

  (noProblem1, ruleTops2, ruleBottoms2, circTops2, circBottoms2, bottomPairs3, topPairs3, wireSubs4)
    = GoAlongInputs (filter ((<>) fromWirePair) inputPairs2) ruleTops ruleBottoms circTops circBottoms
                    (newBottomPairs ++ bottomPairs2) (newTopPairs ++ topPairs2) wireSubs3
  (noProblem2, ruleTops3, ruleBottoms3, circTops3, circBottoms3, bottomPairs4, topPairs4, wireSubs5)
    = GoAlongOutputs outputPairs2 ruleTops2 ruleBottoms2 circTops2 circBottoms2 bottomPairs3 topPairs3 wireSubs4

GoFromInput fromWirePair ruleTops ruleComp ruleBottoms circTops circComp circBottoms bottomPairs1 topPairs1 wireSubs1
  | goodMatch && noProblem1 && noProblem2
      = (True, ruleTops3, ruleBottoms3, circTops3, circBottoms3, bottomPairs4, topPairs4, wireSubs4)
  | otherwise
      = (False, [], [], [], [], [], [], [])
where
  (goodMatch, inputPairs, outputPairs, wireSubs2) = CheckMatch ruleComp circComp wireSubs1
  (bottomPairs2, newTopPairs)                     = RemoveIntersection bottomPairs1 outputPairs
  (topPairs2, newBottomPairs)                     = RemoveIntersection topPairs1 inputPairs

  (noProblem1, ruleTops2, ruleBottoms2, circTops2, circBottoms2, bottomPairs3, topPairs3, wireSubs3)
    = GoAlongInputs (filter ((<>) fromWirePair) inputPairs) ruleTops ruleBottoms circTops circBottoms
                    (newBottomPairs ++ bottomPairs2) (newTopPairs ++ topPairs2) wireSubs2
  (noProblem2, ruleTops3, ruleBottoms3, circTops3, circBottoms3, bottomPairs4, topPairs4, wireSubs4)
    = GoAlongOutputs outputPairs ruleTops2 ruleBottoms2 circTops2 circBottoms2 bottomPairs3 topPairs3 wireSubs3

////////// CheckMatch
CheckMatch :: !Component !Component [WireSub] -> (Bool, [WireSub], [WireSub], [WireSub])
CheckMatch comp1=:{spec=spec1, inputs=inputs1, outputs=outputs1} comp2=:{spec=spec2, inputs=inputs2, outputs=outputs2}
           wireSubs1
  | (EquivalentSpecs spec1 spec2) && inputSubsOK && outputSubsOK
      = (True, inputPairs, outputPairs, wireSubs3)
  | otherwise
      = (False, [], [], [])
where
  (inputSubsOK, inputPairs, wireSubs2)   = MakeWireSubs inputs1 inputs2 [] wireSubs1
  (outputSubsOK, outputPairs, wireSubs3) = MakeWireSubs outputs1 outputs2 [] wireSubs2

////////// MakeWireSubs
MakeWireSubs :: ![Connection] ![Connection] [WireSub] [WireSub] -> (Bool, [WireSub], [WireSub])
MakeWireSubs [{cWireID=id1} : connects1] [{cWireID=id2} : connects2] newWireSubs wireSubs1
  | insertOK  = MakeWireSubs connects1 connects2 [(id1,id2) : newWireSubs] wireSubs2
  | otherwise = (False, [], [])
where
  (insertOK, wireSubs2) = InsertWireSub (id1,id2) wireSubs1

MakeWireSubs [] [] newWireSubs wireSubs = (True, newWireSubs, wireSubs)

////////// InsertWireSub
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

////////// StitchAndTransfer
// Used in repositioning between two lists of lists of components
// representing the top and bottom parts of a circuit.  It glues
// together the two lists at the "break" between the two lists of
// lists, then transfers lists over until the former position is
// reached.
StitchAndTransfer :: !Int ![Circuit] ![Circuit] -> ([Circuit], [Circuit])
StitchAndTransfer n [frontPiece : fronts] [backPiece : backs]
  = ReverseTransfer (n-1) fronts [ReverseAppend frontPiece backPiece : backs]

////////// ReverseTransfer
ReverseTransfer :: !Int [[a]] [[a]] -> ([[a]], [[a]])
ReverseTransfer 0 list1 list2 = (list1, list2)
ReverseTransfer n [x : list1] list2 = ReverseTransfer (n-1) list1 [reverse x : list2]
