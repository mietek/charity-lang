// Contains code used in rewiring thinning links in proof mode.

implementation module rewire

import StdEnv, deltaPicture
import circuitDefs, utilities, sequent, insert

///////// Rewire
// Takes as arguments:
//   - the thinning link to be rewired,
//   - the mouse position identifying the thinning link's new position,
//   - the circuit (including the thinning link),
//   - the circuit's wires,
//   - a list of position pairs giving the relative positions of the inputs in proof mode,
//   - a list of position pairs giving the relative positions of the outputs in proof mode.
// Returns:
//   - an ErrorVal indicating what error, if any, occurred,
//   - the new circuit (with the thinning link rewired),
//   - the new circuit's wires,
//   - the newly rewired thinning link,
//   - a list of functions to be applied to redraw the circuit.
//   - the new list of input position pairs, with wire IDs changed as necessary,
//   - the new list of output position pairs, with wire IDs changed as necessary.
//
// Steps:
//   - Gets the wire pointed to by the mouse position (if none,
//     returns an error).
//   - Gets the three wires connected to the lasso and checks
//     that none of them is the selected wire.
//   - Removes the thinning link from the circuit and checks that
//     its new position won't overlap any components.
//   - Changes the component to put it in its new position, and to change the
//     connections.  (The new wire is split into two - for UnitE thinning links,
//     the bottom wire gets the UnitE's old in-wire ID as its ID, and for CounitI
//     thinning links, the top wire gets the old in-wire ID as its ID.  In both
//     cases, the old input and output wires for the thinning link become one
//     wire with the thinning link's old out-wire ID as the ID.)
//   - Calls either RewireUnitE or RewireCounitI to do the rewiring.
//   - Modifies the input and output position pairs as necessary, and changes
//     the positions and types of the new wire and the wires connected to the
//     original thinning link.
Rewire :: Component Point Circuit ![Wire] [(Int, Connection)] [(Int, Connection)]
           -> (ErrorVal, Circuit, [Wire], Component, [DrawFunction], [(Int, Connection)], [(Int, Connection)])
Rewire comp newPoint circuit wires1 inputPairs outputPairs
  | newWire.wireID==(-1)
      = (NoSelection, [], [], dummyComponent, [], [], [])
  | newWire.wireID==inWireID || newWire.wireID==outWireID || newWire.wireID==lassoWireID
      = (BadWire, [], [], dummyComponent, [], [], [])
  | OverlapCheck {comp & pos=PT newPoint} newCircuit1
      = (NewCompOverlaps, [], [], dummyComponent, [], [], [])
  | IsUnitE comp.spec
      = if (IsNotError returnCodeA)
           (if ((IsUserType inWire.wireType) && (not (IsUserType outWire.wireType)))
               (NoError, ChangeTypeOnInput outWire.wireID (snd outWire.wireLine) inWire.wireType newCircuit2A,
                newWires, newComp, redraws, inputPairsA, outputPairsA
               )
               (if ((IsUserType outWire.wireType) && (not (IsUserType inWire.wireType)))
                   (NoError, ChangeTypeOnOutput outWire.wireID (fst inWire.wireLine) outWire.wireType newCircuit2A,
                    newWires, newComp, redraws, inputPairsA, outputPairsA
                   )
                   (NoError, newCircuit2A, newWires, newComp, redraws, inputPairsA, outputPairsA)
               )
           )
           (returnCodeA, [], [], dummyComponent, [], [], [])
  | otherwise
      = if (IsNotError returnCodeB)
           (if ((IsUserType inWire.wireType) && (not (IsUserType outWire.wireType)))
               (NoError, ChangeTypeOnInput outWire.wireID (snd outWire.wireLine) inWire.wireType newCircuit2B,
                newWires, newComp, redraws, inputPairsB, outputPairs
               )
               (if ((IsUserType outWire.wireType) && (not (IsUserType inWire.wireType)))
                   (NoError, ChangeTypeOnOutput outWire.wireID (fst inWire.wireLine) outWire.wireType newCircuit2B,
                    newWires, newComp, redraws, inputPairsB, outputPairs
                   )
                   (NoError, newCircuit2B, newWires, newComp, redraws, inputPairsB, outputPairs)
               )
           )
           (returnCodeB, [], [], dummyComponent, [], [], [])
where
  (newWire, wires2)                = RemoveSelectedWire newPoint wires1 []
  newCircuit1                      = RemoveWholeComp comp circuit
  (inWireID,outWireID,lassoWireID) = GetLassoWireIDs comp
  (inWire, wires3)                 = RemoveAndReturn (((==) inWireID) o GetWireID) wires2
  (outWire, wires4)                = RemoveAndReturn (((==) outWireID) o GetWireID) wires3
  (lassoWire, wires5)              = RemoveAndReturn (((==) lassoWireID) o GetWireID) wires4

  newComp = ChangeLassoComp comp newPoint newWire

  (returnCodeA, newCircuit2A)
    = RewireUnitE comp newComp newWire.wireID (snd newWire.wireLine) inWire.wireID (fst inWire.wireLine) outWire.wireID
                  lassoWire.wireID (fst lassoWire.wireLine) [] newCircuit1 False
  inputPairsA  = ChangePositionPairID inWireID outWireID inputPairs
  outputPairsA = ChangePositionPairID newWire.wireID inWireID outputPairs
  (returnCodeB, newCircuit2B)
    = RewireCounitI comp newComp newWire.wireID (fst newWire.wireLine) inWire.wireID (fst inWire.wireLine) outWire.wireID
                    lassoWire.wireID (snd lassoWire.wireLine) [] newCircuit1 False False
  inputPairsB  = ChangePositionPairID newWire.wireID inWireID (ChangePositionPairID inWireID outWireID inputPairs)

  newWires = [newNewWire, newInWire, newOutWire, newLassoWire : wires5]
  (newNewWire, newInWire, newOutWire, newLassoWire) = ChangeWires comp.spec newPoint newWire inWire outWire lassoWire

  redraws = (DrawWire newWire) ++ (DrawWire inWire) ++ (DrawWire outWire) ++ (DrawWire lassoWire) ++
            (DrawWire newNewWire) ++ (DrawWire newInWire) ++ (DrawWire newOutWire) ++ (DrawWire newLassoWire) ++
            (DrawComponent comp) ++ (DrawComponent newComp)

///////// RewireUnitE
// Searches for the upper connection point of the main lasso wire (the wire that attaches down into the
// unit), descending into boxes when necessary, splitting the circuit as it goes, and changing the wire ID
// on the upper connection of the wire that connects down into the lasso of the original thinning link (the
// in wire) to the wire ID of the  wire that connects down from the lasso of the original thinning link (the
// out wire) if it encounters it before it finds the upper connection of the lasso wire.  When it finds the
// lasso wire's upper connection, it goes back through the circuit that preceded it at that level, collecting
// everything that doesn't have a direct or indirect connection down to the lasso wire and combines this 
// with the part of the circuit that hasn't yet been searched at that level (fixing the thinning link's old
// wire at this point if it hasn't already been done), then checks that the rewiring is possible by
// sequentializing this part of the circuit as far as possible, then verifying that the old wire and the new
// wire are in the same sequent box.  (If the lasso wire is connected to the internal output of a box, the
// sequentialization test is not necessary, since the box contents must sequentialize to a single sequent box.
// Note that it is not allowed to rewire a thinning link to a position outside of a box if the lasso wire's upper
// connection is inside the box.)  If the rewiring is OK, it "breaks" the new wire (by changing the wire ID
// on its lower connection point to the wire ID of the thinning link's former in wire), then inserts the new
// thinning link into the lower part of the circuit (InsertUnitE verifies that the thinning link is not being
// inserted into a box when it has no connection into that box) and reassembles the circuit.
//
// Takes as arguments:
//   - the original thinning link,
//   - the new thinning link, with its position and connection wire IDs changed,
//   - the wire ID of the wire that it is being moved to (the new wire),
//   - the lower endpoint of the new wire,
//   - the wire ID of the wire that connects down to the lasso of the original thinning link (the in wire),
//   - the upper endpoint of the in wire,
//   - the wire ID of the wire that connects down from the lasso of the original thinning link (the out wire),
//   - the wire ID of the  that connects into the unit of the thinning link (the lasso wire),
//   - the upper endpoint of the lasso wire,
//   - the part of the circuit that has already been searched, in reverse order,
//   - the part of the circuit that has not yet been searched,
//   - a bool indicating whether the wire that the thinning link started out on has been "fixed" yet,
//     i.e. whether the wire ID on the former starting connection of the thinning link's in-wire
//     has been changed to the wire ID of the thinning link's former out wire.
// Returns:
//   - an error value indicating what error, if any, occurred (in this case, always BadRewiring or NoError),
//   - the new circuit (if no error occurred), with the thinning link moved to its new position.
RewireUnitE :: Component Component WireID Point WireID Point WireID WireID Point Circuit !Circuit Bool
                -> (ErrorVal, Circuit)

RewireUnitE oldComp=:{pos=PT oldPoint} newComp=:{pos=PT newPoint} newWireID newWireEnd inWireID inWireStart outWireID
            lassoWireID lassoWireStart topCirc
            [box=:{spec=Box boxCircuit, inputs=[inC], outputs=[outC1, outC2], pos=RCT boxRect} : circuit] wireFixed
  | lassoWireID==outC1.cWireID
      = if (IsInRectangle newPoint boxRect)   // Assumes old position must be inside box
           (if insert1OK  // Verifies that the comp isn't in a box when the wire just crosses the box
               //*^* When not in FILL, may need to break the new wire (at the input) below the box
               (NoError, ReverseAppend topCirc [{box & spec=Box newBoxCircuit1, inputs=[newInC]} : circuit])
               (BadRewiring, [])
           )
           (BadRewiring, [])
  | lassoWireID==outC2.cWireID
      = if (IsInRectangle newPoint boxRect)    // Assumes old position must be outside box
           (BadRewiring, [])
           (if (CheckRewiring outWireID newWireID newBottomCirc1)
               (if insert2OK  // Verifies that the comp isn't in a box when the wire just crosses the box
                   (NoError, newTopCirc ++ newBottomCirc2)
                   (BadRewiring, [])
               )
               (BadRewiring, [])
           )
  | IsInRectangle lassoWireStart boxRect
      = if (IsInRectangle newPoint boxRect)     //*^* In FILL, the oldPoint will always be in the box
           //*^* When not in FILL, may need to break new wire (at input) below the box after returning.
           (if (IsNotError returnCode)
               (NoError,
                ReverseAppend
                  topCirc
                  [{box & spec=Box newBoxCircuit2, inputs=[newInC], outputs=[newOutC1, outC2]} : circuit]
               )
               (returnCode, [])
           )
           (BadRewiring, [])
  | otherwise
      = RewireUnitE oldComp newComp newWireID newWireEnd inWireID inWireStart outWireID lassoWireID lassoWireStart
                    [{box & outputs=[outC1, newOutC2]} : topCirc] circuit (wireFixed || wireFixed2)
where
  newInC                 = if (inC.cWireID==newWireID)  {inC & cWireID=inWireID}  inC
  (wireFixed1, newOutC1) = if (outC1.cWireID==inWireID) (True, {outC1 & cWireID=outWireID}) (False, outC1)
  (wireFixed2, newOutC2) = if (outC2.cWireID==inWireID) (True, {outC2 & cWireID=outWireID}) (False, outC2)

  (insert1OK, newBoxCircuit1)
    = if wireFixed
         (if (inC.cWireID==newWireID)
             (InsertUnitE newComp newWireEnd boxCircuit)
             (InsertUnitE newComp newWireEnd (ChangeInput newWireID newWireEnd inWireID boxCircuit))
         )
         (if (inC.cWireID==newWireID)
             (InsertUnitE newComp newWireEnd (ChangeOutput inWireID inWireStart outWireID boxCircuit))
             (InsertUnitE newComp newWireEnd
                          (ChangeOutput inWireID inWireStart outWireID
                             (ChangeInput newWireID newWireEnd inWireID boxCircuit)
                          )
             )
         )

  newCircuit2                  = if wireFixed circuit (ChangeOutput inWireID inWireStart outWireID circuit)
  (newTopCirc, newBottomCirc1) = PushUpFrom topCirc [box] newCircuit2
  (insert2OK, newBottomCirc2)  = InsertUnitE newComp newWireEnd (ChangeInput newWireID newWireEnd inWireID newBottomCirc1)

  (returnCode, newBoxCircuit2)
    = RewireUnitE oldComp newComp newWireID newWireEnd inWireID inWireStart outWireID lassoWireID lassoWireStart []
                  boxCircuit (wireFixed || wireFixed1)

RewireUnitE oldComp newComp newWireID newWireEnd inWireID inWireStart outWireID lassoWireID lassoWireStart topCirc
            [comp=:{outputs} : circuit] wireFixed
  | any (((==) lassoWireID) o GetConnectWireID) outputs
      = if (CheckRewiring outWireID newWireID newBottomCirc1)
           (if insertOK
               (NoError, newTopCirc ++ newBottomCirc2)
               (BadRewiring, [])
           )
           (BadRewiring, [])
  | wireFixed
      = RewireUnitE oldComp newComp newWireID newWireEnd inWireID inWireStart outWireID lassoWireID lassoWireStart
                    [comp : topCirc] circuit True
  | otherwise
      = RewireUnitE oldComp newComp newWireID newWireEnd inWireID inWireStart outWireID lassoWireID lassoWireStart
                    [{comp & outputs=newOutputs} : topCirc] circuit wireFixed`
where
  (wireFixed`, newOutputs)     = SetConnection inWireID outWireID outputs
  newCircuit                   = if (wireFixed || wireFixed`) circuit (ChangeOutput inWireID inWireStart outWireID circuit)
  (newTopCirc, newBottomCirc1) = PushUpFrom topCirc [{comp & outputs=newOutputs}] newCircuit
  (insertOK, newBottomCirc2)   = InsertUnitE newComp newWireEnd (ChangeInput newWireID newWireEnd inWireID newBottomCirc1)

///////// PushUpFrom
// This is used to transfer all components from the top part of the circuit to the bottom part of the
// circuit that do not have a direct or indirect connection to the lasso wire of the thinning link.
//
// Takes as arguments:
//   - the part of the top circuit that hasn't been searched yet (in reverse order),
//   - the part of the top circuit (so far) that does connect down to the lasso wire,
//   - the bottom circuit (everything that doesn't connect down to the lasso wire, so far).
// Returns:
//   - the top circuit (everything that connects down to the lasso wire),
//   - the bottom circuit (everything that doesn't connect down to the lasso wire).
PushUpFrom :: !Circuit Circuit Circuit -> (Circuit, Circuit)
PushUpFrom [comp : circuit] topCirc bottomCirc
  | any (ConnectsDown comp) topCirc = PushUpFrom circuit [comp : topCirc] bottomCirc
  | otherwise                       = PushUpFrom circuit topCirc [comp : bottomCirc]

PushUpFrom [] topCirc bottomCirc = (topCirc, bottomCirc)

///////// InsertUnitE
// This inserts a unit elimination thinning link into the circuit, verifying that it isn't
// inside any box that it doesn't have a connection into.
//
// Takes as arguments:
//   - the thinning link,
//   - the lower endpoint of the thinning link,
//   - the circuit.
// Returns:
//   - True if the insertion was OK, False otherwise,
//   - the new circuit (if there was no problem).
InsertUnitE :: !Component Point !Circuit -> (Bool, Circuit)
InsertUnitE comp bottomWireEnd circuit
  | success                = InsertUnboxed newComp newCircuit  //*^* InsertUnboxed (with check for cycles) not necesary
  | otherwise              = (False, [])
where
  (success, newComp, newCircuit) = PutUnitEInBox comp bottomWireEnd circuit

///////// PutUnitEInBox
// Like PutInBox in the insert module, but checks that the component connects into the box before putting it in.
PutUnitEInBox :: !Component Point !Circuit -> (Bool, Component, Circuit)

PutUnitEInBox comp=:{pos=PT centre} bottomWireEnd [box=:{spec=Box boxCircuit, outputs=[_, outC2], pos=RCT rect} : circuit]
  | IsInRectangle centre rect
      = if (IsInRectangle bottomWireEnd rect)  //*^* In FILL, this is the only way the wire can connect into the box
           (if success1
               (True, {box & spec=Box newBoxCircuit}, circuit)
               (False, comp, [])
           )
           (False, comp, [])
  | otherwise
      = (success2, newComp, [box : newCircuit])
where
  (success1, newBoxCircuit)       = InsertUnitE comp bottomWireEnd boxCircuit
  (success2, newComp, newCircuit) = PutUnitEInBox comp bottomWireEnd circuit

PutUnitEInBox comp1 bottomWireEnd [comp2 : circuit]
  = (success, newComp, [comp2 : newCircuit])
where
  (success, newComp, newCircuit) = PutUnitEInBox comp1 bottomWireEnd circuit

PutUnitEInBox comp _ [] = (True, comp, [])

///////// RewireCounitI
// Searches for the lower connection point of the main lasso wire (the wire that attaches up into the
// counit), descending into a box only when it finds that both the old and the new positions of the
// thinning link are in the box (since a counit-introduction thinning link cannot change level in a
// rewiring).  It splits the circuit as it goes, and changes the wire ID on the upper connection of the
// wire that connects down into the lasso of the original thinning link (the in wire) to the wire ID of
// the  wire that connects down from the lasso of the original thinning link (the out wire) when it
// encounters it (it must encounter it before it encounters the lasso wire's connection point), and also
// changing the wire ID on the upper connection of the thinning link's new wire to the wire ID of the 
// thinning link's in wire if it finds it on the internal output connection of a box.  When it finds the
// lasso wire's lower connection (or finds that the lasso connects into a box that doesn't contain it),
// it first verifies that the thinning link is not being rewired into a box further down in the circuit,
// then goes through the part of the circuit (at that level) that hasn't yet been searched, collecting
// everything that doesn't have a direct or indirect connection up to the lasso wire and combining this with
// the upper part of the circuit, then checks that the rewiring is possible by sequentializing this part of
// the circuit as far as possible and verifying that the old wire and the new wire are in the same sequent
// box.  (If the lasso wire connects down into the box's internal input, the sequentialization test is not
// necessary, since the box contents must sequentialize to a single sequent box.)  It then "breaks" the
// new wire if this hasn't already been done (by changing the wire ID on its upper connection point to the
// wire ID of the thinning link's in wire), then inserts the new thinning link into the upper part of the
// circuit and reassembles the circuit.
//
// Takes as arguments:
//   - the original thinning link,
//   - the new thinning link, with its position and connection wire IDs changed,
//   - the wire ID of the wire that it is being moved to (the new wire),
//   - the upper endpoint of the new wire,
//   - the wire ID of the wire that connects down to the lasso of the original thinning link (the in wire),
//   - the upper endpoint of the in wire,
//   - the wire ID of the wire that connects down from the lasso of the original thinning link (the out wire),
//   - the wire ID of the  that connects into the counit of the thinning link (the lasso wire),
//   - the lower endpoint of the lasso wire,
//   - the part of the circuit that has already been searched, in reverse order,
//   - the part of the circuit that has not yet been searched,
//   - a bool indicating whether the wire that the thinning link started out on has been "fixed" yet,
//     i.e. whether the wire ID on the former starting connection of the thinning link's in-wire
//     has been changed to the wire ID of the thinning link's former out wire,
//   - a bool indicating whether the wire that the thinning link is being moved to has been "broken" yet,
//     i.e. whether the wire ID on the former starting connection of the new wire has been changed to the
//     wire ID of the thinning link's former in wire.
// Returns:
//   - an error value indicating what error, if any, occurred (in this case, always BadRewiring or NoError),
//   - the new circuit (if no error occurred), with the thinning link moved to its new position.
RewireCounitI :: !Component !Component WireID Point WireID Point WireID WireID Point Circuit !Circuit Bool Bool
                   -> (ErrorVal, Circuit)

//*^* If not in FILL, might also need to fix inWire in boxCircuit when component is not in the box circuit
RewireCounitI oldComp=:{pos=PT oldPoint} newComp=:{pos=PT newPoint} newWireID newWireStart inWireID inWireStart outWireID
              lassoWireID lassoWireEnd topCirc
              [box=:{spec=Box boxCircuit, pos=RCT boxRect, inputs=[inC], outputs=[outC1, outC2]} : circuit]
              wireFixed wireBroken
  | IsInRectangle oldPoint boxRect
      = if (IsInRectangle newPoint boxRect)  //*^* In FILL, the lasso connection will always be in the box if this is true
           (if (inC.cWireID==lassoWireID)
               (NoError, ReverseAppend topCirc` [{box & spec=Box newBoxCircuit1C, outputs=[newOutC1, outC2]} : circuit])
               (if (IsNotError returnCode)
                   (NoError, ReverseAppend topCirc` [{box & spec=Box newBoxCircuit2, outputs=[newOutC1, outC2]} : circuit])
                   (returnCode, [])
               )
           )
           (BadRewiring, [])
  | IsInRectangle newPoint boxRect  //*^* CounitIs cannot change level by a rewiring in FILL
      = (BadRewiring, [])
  | IsInRectangle lassoWireEnd boxRect
      = if (not (LassoInBox newComp circuit))   //*^* A CounitI cannot change level by a rewiring in FILL
           (if (CheckRewiring newWireID outWireID newTopCirc1)
               (NoError, newTopCirc2 ++ newBottomCirc)
               (BadRewiring, [])
           )
           (BadRewiring, [])
  | otherwise
      = RewireCounitI oldComp newComp newWireID newWireStart inWireID inWireStart outWireID lassoWireID lassoWireEnd
                      [{box & outputs=[outC1, newOutC2]} : topCirc] circuit (wireFixed || wireFixed2) wireBroken
where
  (wireFixed1, wireBroken1, newOutC1)
    = if (outC1.cWireID==inWireID)
         (True, False, {outC1 & cWireID=outWireID})
         (if (outC1.cWireID==newWireID)
             (False, True, {outC1 & cWireID=inWireID})
             (False, False, outC1)
         )
  (wireFixed2, newOutC2) = if (outC2.cWireID==inWireID) (True, {outC2 & cWireID=outWireID}) (False, outC2)

  (wireBroken`, topCirc`)
    = if (wireBroken || wireBroken1)
         (True, topCirc)
         (if (IsInRectangle newWireStart boxRect)
             (False, topCirc)
             (True, ChangeOutput newWireID newWireStart inWireID topCirc)
         )

  newBoxCircuit1A
    = if (wireFixed || wireFixed1)
         boxCircuit
         (ChangeOutput inWireID inWireStart outWireID boxCircuit)
  newBoxCircuit1B
    = if wireBroken`
         newBoxCircuit1A
         (ChangeOutput newWireID newWireStart inWireID newBoxCircuit1A)

  (_, newBoxCircuit1C) = InsertUnboxed newComp newBoxCircuit1B

  (returnCode, newBoxCircuit2)
    = RewireCounitI oldComp newComp newWireID newWireStart inWireID inWireStart outWireID lassoWireID lassoWireEnd
                    [] boxCircuit (wireFixed || wireFixed1) wireBroken`

  (newTopCirc1, newBottomCirc) = PushDownFrom circuit topCirc [box]
  (_, newTopCirc2) = if wireBroken
                        (InsertUnboxed newComp newTopCirc1)
                        (InsertUnboxed newComp (ChangeOutput newWireID newWireStart inWireID newTopCirc1))

RewireCounitI oldComp newComp newWireID newWireStart inWireID inWireStart outWireID lassoWireID lassoWireEnd
              topCirc [comp=:{inputs, outputs} : circuit] wireFixed wireBroken
  | any (((==) lassoWireID) o GetConnectWireID) inputs
      = if (not (LassoInBox newComp circuit))   //*^* A CounitI cannot change level by rewiring in FILL
           (if (CheckRewiring newWireID outWireID newTopCirc1)
               (NoError, newTopCirc2 ++ newBottomCirc)
               (BadRewiring, [])
           )
           (BadRewiring, [])
  | wireFixed = RewireCounitI oldComp newComp newWireID newWireStart inWireID inWireStart outWireID lassoWireID lassoWireEnd
                              [comp : topCirc] circuit True wireBroken
  | otherwise = RewireCounitI oldComp newComp newWireID newWireStart inWireID inWireStart outWireID lassoWireID lassoWireEnd
                              [{comp & outputs=newOutputs} : topCirc] circuit wireFixed` wireBroken
where
  (newTopCirc1, newBottomCirc) = PushDownFrom circuit topCirc [comp]
  (_, newTopCirc2)
    = if wireBroken
         (InsertUnboxed newComp newTopCirc1)
         (InsertUnboxed newComp (ChangeOutput newWireID newWireStart inWireID newTopCirc1))

  (wireFixed`, newOutputs) = SetConnection inWireID outWireID outputs

///////// LassoInBox
// Checks if the thinning link (the first argument) is in a box anywhere in the circuit.
LassoInBox :: !Component !Circuit -> Bool
LassoInBox comp1=:{pos=PT centre} [box=:{spec=Box _, pos=RCT boxRect} : circuit]
  = (IsInRectangle centre boxRect) || LassoInBox comp1 circuit
LassoInBox comp1 [comp2 : circuit]
  = LassoInBox comp1 circuit
LassoInBox _ [] = False

///////// PushDownFrom
// Collects everything in the bottom part of the circuit that does not have a direct or indirect
// connection from the thinning link's lasso wire and adds it to the top part of the circuit.
//
// Takes as arguments:
//   - the part of the bottom circuit that hasn't been searched yet,
//   - the top circuit (everything that doesn't connect down from the lasso wire, so far),
//   - the bottom circuit (everything that does connect down from the lasso wire, so far)
// Returns:
//   - the top circuit (everything that doesn't connect down from the lasso wire),
//   - the bottom circuit (everything that does connect down from the lasso wire)
PushDownFrom :: !Circuit Circuit Circuit -> (Circuit, Circuit)
PushDownFrom [comp : circuit] topCirc bottomCirc
  | any ((flip ConnectsDown) comp) bottomCirc
      = PushDownFrom circuit topCirc [comp : bottomCirc]
  | otherwise
      = PushDownFrom circuit [comp : topCirc] bottomCirc

PushDownFrom [] topCirc bottomCirc = (reverse topCirc, reverse bottomCirc)

///////// ChangeInput
// Changes the wire ID on an input connection.
//
// Takes as arguments:
//   - the old wire ID,
//   - the input connection point of the old wire,
//   - the new wire ID,
//   - the circuit
// Returns:
//   - the circuit with the wire ID changed.
ChangeInput :: WireID Point WireID !Circuit -> Circuit
ChangeInput oldWireID endPoint newWireID [box=:{spec=Box boxCircuit, inputs=[inC], pos=RCT boxRect} : circuit]
  | oldWireID==inC.cWireID
      = [{box & inputs=[{inC & cWireID=newWireID}]} : circuit]
  | IsInRectangle endPoint boxRect
      = [{box & spec=Box (ChangeInput oldWireID endPoint newWireID boxCircuit)} : circuit]
  | otherwise
      = [box : ChangeInput oldWireID endPoint newWireID circuit]

ChangeInput oldWireID endPoint newWireID [comp=:{inputs} : circuit]
  | isEmpty backInputs = [comp : ChangeInput oldWireID endPoint newWireID circuit]
  | otherwise          = [{comp & inputs=(frontInputs ++ [{(hd backInputs) & cWireID=newWireID} : tl backInputs])}
                          : circuit]
where
  (frontInputs, backInputs) = span (((<>) oldWireID) o GetConnectWireID) inputs

ChangeInput _ _ _ [] = []

///////// ChangeOutput
// Changes the wire ID on an output connection.
//
// Takes as arguments:
//   - the old wire ID,
//   - the output connection point of the old wire,
//   - the new wire ID,
//   - the circuit
// Returns:
//   - the circuit with the wire ID changed.
ChangeOutput :: WireID Point WireID !Circuit -> Circuit
ChangeOutput oldWireID startPoint newWireID [box=:{spec=Box boxCircuit, outputs=[outC1, outC2], pos=RCT boxRect} : circuit]
  | oldWireID==outC1.cWireID
      = [{box & outputs=[{outC1 & cWireID=newWireID}, outC2]} : circuit]
  | oldWireID==outC2.cWireID
      = [{box & outputs=[outC1, {outC2 & cWireID=newWireID}]} : circuit]
  | IsInRectangle startPoint boxRect
      = [{box & spec=Box (ChangeOutput oldWireID startPoint newWireID boxCircuit)} : circuit]
  | otherwise
      = [box : ChangeOutput oldWireID startPoint newWireID circuit]

ChangeOutput oldWireID startPoint newWireID [comp=:{outputs} : circuit]
  | isEmpty backOutputs = [comp : ChangeOutput oldWireID startPoint newWireID circuit]
  | otherwise           = [{comp & outputs=(frontOutputs ++ [{(hd backOutputs) & cWireID=newWireID} : tl backOutputs])}
                           : circuit]
where
  (frontOutputs, backOutputs) = span (((<>) oldWireID) o GetConnectWireID) outputs

///////// RemoveSelectedWire
// Removes and returns the wire indicated by a point from a list of wires.
//
// Takes as arguments:
//   - a point on the wire,
//   - the list of wires being searched,
//   - the list of wires already searched.
// Returns:
//   - the selected wire,
//   - the original list of wires with the selected wire removed.
RemoveSelectedWire :: Point ![Wire] [Wire] -> (Wire, [Wire])

RemoveSelectedWire nearPoint [wire : wires] newWires
  | IsOnLine nearPoint wire.wireLine
      = (wire, ReverseAppend wires newWires)
  | otherwise
      = RemoveSelectedWire nearPoint wires [wire : newWires]

RemoveSelectedWire _ [] newWires = ({wireID=(-1), wireLine=((0,0),(0,0)), wireType=Free Unit}, newWires)

///////// IsUnitE
//*^*IsUnitE :: !CompSpecifics -> Bool
IsUnitE :: !(CompSpecifics Component) -> Bool
IsUnitE UnitEL = True
IsUnitE UnitER = True
IsUnitE _ = False

///////// ChangeLassoComp
// Modifies a thinning link that is being rewired to give its new position and wire IDs.
// Unit-elimination thinning links have their in wire's ID changed to the new wire's ID,
// their out wire's ID changed to the former in wire's ID and the types of both their in
// and out wires changed to the type of the new wire.
// Counit-introduction thinning links have their out wire's ID changed to the new wire's
// ID and the types of both their in and out wires changed to the new wire's type.
//
// Takes as arguments:
//   - the original thinning link,
//   - its new position point,
//   - the new wire.
// Returns:
//   - the new thinning link.
ChangeLassoComp :: !Component Point !Wire -> Component
ChangeLassoComp comp=:{spec=UnitEL, inputs=[lassoInC, wireInC], outputs=[wireOutC]} newPoint {wireID, wireType}
  = {comp & inputs=[lassoInC, {wireInC & cWireID=wireID, cWireType=wireType}],
            outputs=[{wireOutC & cWireID=wireInC.cWireID, cWireType=wireType}],
            pos=PT newPoint}
ChangeLassoComp comp=:{spec=UnitER, inputs=[wireInC, lassoInC], outputs=[wireOutC]} newPoint {wireID, wireType}
  = {comp & inputs=[{wireInC & cWireID=wireID, cWireType=wireType}, lassoInC],
            outputs=[{wireOutC & cWireID=wireInC.cWireID, cWireType=wireType}],
            pos=PT newPoint}
ChangeLassoComp comp=:{spec=CounitIL, inputs=[wireInC], outputs=[lassoOutC, wireOutC]} newPoint {wireID, wireType}
  = {comp & inputs=[{wireInC & cWireType=wireType}],
            outputs=[lassoOutC, {wireOutC & cWireID=wireID, cWireType=wireType}],
            pos=PT newPoint}
ChangeLassoComp comp=:{spec=CounitIR, inputs=[wireInC], outputs=[wireOutC, lassoOutC]} newPoint {wireID, wireType}
  = {comp & inputs=[{wireInC & cWireType=wireType}],
            outputs=[{wireOutC & cWireID=wireID, cWireType=wireType}, lassoOutC],
            pos=PT newPoint}

///////// ChangeWires
// Changes the endpoints and types of the four wires involved in the rewiring.
// For unit-elimination thinning links, the out wire becomes the "old" wire, the in wire
// becomes the thinning link's new out wire, and the new wire becomes the thinning link's
// new in wire.
// For counit-introduction thinning links, the out wire becomes the "old" wire, the in
// wire continues to be the thinning link's in wire (but changes type), and the new
// wire becomes the thinning link's new out wire.
//*^*ChangeWires :: !CompSpecifics Point Wire !Wire Wire !Wire -> (Wire, Wire, Wire, Wire)
ChangeWires :: !(CompSpecifics Component) Point Wire !Wire Wire !Wire -> (Wire, Wire, Wire, Wire)
ChangeWires UnitEL centre newWire inWire outWire lassoWire=:{wireLine=(start,_)}
  | IsUserType inWire.wireType
      = ({outWire & wireLine=(fst inWire.wireLine, snd outWire.wireLine), wireType=inWire.wireType},
         {newWire & wireLine=(fst newWire.wireLine, LassoInConnection centre)},
         {inWire & wireLine=(LassoOutConnection centre, snd newWire.wireLine), wireType=newWire.wireType},
         {lassoWire & wireLine=(start, LassoLeftInConnection centre)}
        )
  | otherwise
      = ({outWire & wireLine=(fst inWire.wireLine, snd outWire.wireLine)},
         {newWire & wireLine=(fst newWire.wireLine, LassoInConnection centre)},
         {inWire & wireLine=(LassoOutConnection centre, snd newWire.wireLine), wireType=newWire.wireType},
         {lassoWire & wireLine=(start, LassoLeftInConnection centre)}
        )

ChangeWires UnitER centre newWire inWire outWire lassoWire=:{wireLine=(start,_)}
  | IsUserType inWire.wireType
      = ({outWire & wireLine=(fst inWire.wireLine, snd outWire.wireLine), wireType=inWire.wireType},
         {newWire & wireLine=(fst newWire.wireLine, LassoInConnection centre)},
         {inWire & wireLine=(LassoOutConnection centre, snd newWire.wireLine), wireType=newWire.wireType},
         {lassoWire & wireLine=(start, LassoRightInConnection centre)}
        )
  | otherwise
      = ({outWire & wireLine=(fst inWire.wireLine, snd outWire.wireLine)},
         {newWire & wireLine=(fst newWire.wireLine, LassoInConnection centre)},
         {inWire & wireLine=(LassoOutConnection centre, snd newWire.wireLine), wireType=newWire.wireType},
         {lassoWire & wireLine=(start, LassoRightInConnection centre)}
        )

ChangeWires CounitIL centre newWire inWire outWire lassoWire=:{wireLine=(_, end)}
  | IsUserType inWire.wireType
      = ({outWire & wireLine=(fst inWire.wireLine, snd outWire.wireLine), wireType=inWire.wireType},
         {inWire & wireLine=(fst newWire.wireLine, LassoInConnection centre), wireType=newWire.wireType},
         {newWire & wireLine=(LassoOutConnection centre, snd newWire.wireLine)},
         {lassoWire & wireLine=(LassoLeftOutConnection centre, end)}
        )
  | otherwise
      = ({outWire & wireLine=(fst inWire.wireLine, snd outWire.wireLine)},
         {inWire & wireLine=(fst newWire.wireLine, LassoInConnection centre), wireType=newWire.wireType},
         {newWire & wireLine=(LassoOutConnection centre, snd newWire.wireLine)},
         {lassoWire & wireLine=(LassoLeftOutConnection centre, end)}
        )

ChangeWires CounitIR centre newWire inWire outWire lassoWire=:{wireLine=(_, end)}
  | IsUserType inWire.wireType
      = ({outWire & wireLine=(fst inWire.wireLine, snd outWire.wireLine), wireType=inWire.wireType},
         {inWire & wireLine=(fst newWire.wireLine, LassoInConnection centre), wireType=newWire.wireType},
         {newWire & wireLine=(LassoOutConnection centre, snd newWire.wireLine)},
         {lassoWire & wireLine=(LassoRightOutConnection centre, end)}
        )
  | otherwise
      = ({outWire & wireLine=(fst inWire.wireLine, snd outWire.wireLine)},
         {inWire & wireLine=(fst newWire.wireLine, LassoInConnection centre), wireType=newWire.wireType},
         {newWire & wireLine=(LassoOutConnection centre, snd newWire.wireLine)},
         {lassoWire & wireLine=(LassoRightOutConnection centre, end)}
        )

///////// SetConnection
// Changes the wire ID on a connection in the list from the old wire ID to the new wire ID.
SetConnection :: WireID WireID ![Connection] -> (Bool, [Connection])
SetConnection oldWireID newWireID connects
  | isEmpty backConnects = (False, connects)
  | otherwise            = (True, frontConnects ++ [{(hd backConnects) & cWireID=newWireID} : tl backConnects])
where
  (frontConnects, backConnects) = span (((<>) oldWireID) o GetConnectWireID) connects

///////// CheckRewiring
// This verifies that a rewiring can take place from the first wire to the second
// in the given circuit by sequentializing the circuit as far as possible and then
// checking that the two wires are both in the same sequent box.
CheckRewiring :: WireID WireID !Circuit -> Bool
CheckRewiring wireID1 wireID2 circuit
  = SameEmpire wireID1 wireID2 seqCircuit
where
  (_, seqCircuit) = FILLSequentAux [] circuit

///////// SameEmpire
// Checks that two wires are both in the same sequent box of a circuit that has
// been sequentialized as far as possible.
SameEmpire :: WireID WireID !Circuit -> Bool
SameEmpire wireID1 wireID2 [seqBox=:{spec=Sequent wireIDs, inputs, outputs} : seqCircuit]
  = (((any ((==) wireID1) wireIDs) ||
      (any (((==) wireID1) o GetConnectWireID) inputs) ||
      (any (((==) wireID1) o GetConnectWireID) outputs)
     ) &&
     ((any ((==) wireID2) wireIDs) ||
      (any (((==) wireID2) o GetConnectWireID) inputs) ||
      (any (((==) wireID2) o GetConnectWireID) outputs)
     )
    ) ||
    (SameEmpire wireID1 wireID2 seqCircuit)

SameEmpire wireID1 wireID2 [_ : seqCircuit] = SameEmpire wireID1 wireID2 seqCircuit

SameEmpire _ _ [] = False
