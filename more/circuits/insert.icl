// This module contains code used to insert components into a circuit, in both modes
// (in proof mode, a sequentialization test must be done).  The circuit and all box
// circuits are kept in order, so any component that connects down from another
// component will be later in the circuit (list of components) than the second
// component.  All functions assumes that the original circuit is sorted.

implementation module insert

import StdEnv, circuitDefs, utilities, sequent

///////// InsertComponent
// Inserts a component (which may need to be inserted into a box or boxes) into a circuit.
// Returns True if there was no problem (the insertion did not result in a cycle) and the
// new circuit.
InsertComponent :: Component !Circuit -> (Bool, Circuit)
InsertComponent comp circuit
  | success                = InsertUnboxed newComp newCircuit
  | otherwise              = (False, [])
where
  (success, newComp, newCircuit) = PutInBox comp circuit

///////// PutInBox
// Checks if the component needs to be inserted into any box in the circuit, and
// recursively calls InsertComponent to do the insertion if so.  Returns either
// the component or the box (if the component was inserted into a box) to be
// inserted into the top level of the circuit.  (Inserting the component into
// the box changes its connections, and may require that its position in the
// circuit be changed.)
PutInBox :: Component !Circuit -> (Bool, Component, Circuit)

PutInBox comp [box=:{spec=Box boxCircuit, outputs=[_, outC2], pos=RCT rect} : circuit]
  | Encloses rect (ComponentRectangle comp)                // The component is inside the box
      = if (isMember (outC2.cWireID) (GetInWireIDs comp))  // If moved comp is attached to box external out connect
           (False, comp, [])
           (if success1
               (True, {box & spec=Box newBoxCircuit}, circuit)
               (False, comp, [])
           )
  | otherwise
      = (success2, newComp, [box : newCircuit])
where
  (success1, newBoxCircuit)       = InsertComponent comp boxCircuit
  (success2, newComp, newCircuit) = PutInBox comp circuit

PutInBox comp1 [comp2 : circuit]
  = (success, newComp, [comp2 : newCircuit])
where
  (success, newComp, newCircuit) = PutInBox comp1 circuit

PutInBox comp [] = (True, comp, [])

///////// InsertComponentInProof
// Like InsertComponent, but checks sequentialization of innermost box that the component is inserted into
// (in FILL, this should ensure that the whole circuit still sequentializes if it sequentialized before).
InsertComponentInProof :: Component !Circuit -> (Bool, Bool, Circuit)
InsertComponentInProof comp circuit
  | success   = if inserted
                   (True, sequentialized, newCircuit2)
                   (False, False, [])
  | otherwise = (False, False, [])
where
  (success, sequentialized, newComp, newCircuit1) = PutInBoxInProof comp circuit
  (inserted, newCircuit2)                         = InsertUnboxed newComp newCircuit1

///////// PutInBoxInProof
// Like PutInBox, but with the sequentialization check.
PutInBoxInProof :: Component !Circuit -> (Bool, Bool, Component, Circuit)

PutInBoxInProof comp [box=:{spec=Box boxCircuit, outputs=[_, outC2], pos=RCT rect} : circuit]
  | Encloses rect (ComponentRectangle comp)
      = if (isMember (outC2.cWireID) (GetInWireIDs comp))  // If moved comp is attached to box external out connect
           (False, False, comp, [])
           (if success1
               (if sequentialized1
                   (True, True, newBox, circuit)
                   (if sequentializes
                       (True, True, newBox, circuit)
                       (False, False, comp, [])
                   )
               )
               (False, False, comp, [])
           )
  | otherwise
      = (success2, sequentialized2, newComp, [box : newCircuit])
where
  (success1, sequentialized1, newBoxCircuit)       = InsertComponentInProof comp boxCircuit
  newBox                                           = {box & spec=Box newBoxCircuit}
  (sequentializes, _)                              = FILLSequentialize [newBox]
  (success2, sequentialized2, newComp, newCircuit) = PutInBoxInProof comp circuit

PutInBoxInProof comp1 [comp2 : circuit]
  = (success, sequentialized, newComp, [comp2 : newCircuit])
where
  (success, sequentialized, newComp, newCircuit) = PutInBoxInProof comp1 circuit

PutInBoxInProof comp [] = (True, False, comp, [])

///////// InsertUnboxed
// This inserts a component at the uppermost level of the circuit (it doesn't do
// insertion into boxes) and checks that the insertion doesn't result in a
// cycle.
InsertUnboxed :: Component !Circuit -> (Bool, Circuit)

InsertUnboxed x [y : circuit]
  | (ConnectsDown x y)
      = CheckOrder x [y : circuit]
  | otherwise
      = (insertOK, [y : newCircuit])
where
  (insertOK, newCircuit) = InsertUnboxed x circuit

InsertUnboxed x [] = (True, [x])

///////// CheckOrder
// Calls MoveStuff to get anything in the circuit that connects down to x
// (directly or indirectly) so that it can be placed before the rest, and
// checks there is no cycle involving x (x does not connect down to
// anything that connects down to it).
CheckOrder :: !Component !Circuit -> (Bool, Circuit)
CheckOrder x circuit
  | any (MemberOf (GetOutWireIDs x)) inWires
      = (False, [])
  | otherwise
      = (True, movedStuff ++ rest)
where
  // x is included in movedStuff, and inWires contains wire IDs of all inputs in movedStuff that aren't
  // connected to anything else in movedStuff.
  (movedStuff, rest, inWires) = MoveStuff x circuit

//*^*///////// CheckOrder
//*^*CheckOrder :: Component Circuit -> (Bool, Circuit)
//*^*CheckOrder x circuit
//*^*  | (any (ConnectsDown x) movedStuff)
//*^*      = (False, [])
//*^*  | otherwise
//*^*      = (True, movedStuff ++ rest)
//*^*where
//*^*  (movedStuff, rest) = MoveStuff x circuit

///////// MoveStuff
MoveStuff :: Component !Circuit -> (Circuit, Circuit, [WireID])

MoveStuff x [y : circuit]
  | isEmpty connectedWires
      = (movedStuff, [y : rest], inWires)
  | otherwise
      = ([y : movedStuff], rest, (GetInWireIDs y) ++ otherWires)
where
  (movedStuff, rest, inWires) = MoveStuff x circuit
  (connectedWires, otherWires) = filterInOut (MemberOf (GetOutWireIDs y)) inWires

MoveStuff x [] = ([x], [], GetInWireIDs x)

//*^*///////// MoveStuff
//*^*MoveStuff :: Component Circuit -> (Circuit, Circuit)
//*^*
//*^*MoveStuff x [y : circuit]
//*^*  | (any (ConnectsDown y) movedStuff)
//*^*      = ([y : movedStuff], rest)
//*^*  | otherwise
//*^*      = (movedStuff, [y : rest])
//*^*where
//*^*  (movedStuff, rest) = MoveStuff x circuit
//*^*
//*^*MoveStuff x [] = ([x], [])

//////// ConnectsDown
ConnectsDown :: !Component Component -> Bool
ConnectsDown comp1 comp2
  = AnyMembers (GetOutWireIDs comp1) (GetInWireIDs comp2)

///////// AnyMembers
// Takes two lists and returns True if any member of the
// first list is also a member of the second, False
// otherwise.
AnyMembers :: ![a] [a] -> Bool | == a
AnyMembers list1 list2 = any (MemberOf list2) list1
