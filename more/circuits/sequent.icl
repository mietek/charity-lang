// Contains the code used to check whether a circuit sequentializes, and also to 
// produce a string used to show what a circuit sequentialized to.

//*** NOTE : Could use NotFullyWired (in utilities) to check wiring before removing terminals
//***        and checking for terminals in boxes.

//*** Could use minimal list of wires when doing insertion.

implementation module sequent

import StdEnv, circuitDefs, utilities, insert, typeDefs

///////// SequentString
// Takes a sequentialized circuit (should contain exactly one sequent box) and
// a list of wires, and returns a string of the form "<inputTypes> |- <outputTypes>"
// with the input and output types for the circuit sorted according to the
// horizontal position of the terminal endpoints of the corresponding wires.
SequentString :: !Circuit [Wire] -> String
SequentString [{spec=Sequent _, inputs, outputs}] wires
  = (ConnectsString (GetInputPositionPairs wires inputs)) +++ "  |-  "
    +++ (ConnectsString (GetOutputPositionPairs wires outputs))

///////// ConnectsString
ConnectsString :: ![(Int, Connection)] -> String
ConnectsString [(_, {cWireType})] = TypeString (GetType cWireType)
ConnectsString [(_, {cWireType}) : connects]
  = (TypeString (GetType cWireType)) +++ ", " +++ (ConnectsString connects)
ConnectsString [] = ""

////////// FILLSequentialize
// Sequentializes a circuit, returning True if it was successful with 
// the sequentialized circuit.
FILLSequentialize :: !Circuit -> (Bool, Circuit)
FILLSequentialize circuit
  | not allConnected
      = (False, [])
  | sequentDone   // A wire was discovered connecting a start terminal and an end terminal, and was made into a sequent box.
      = if (IsSequentialized newCircuit)
           (True, newCircuit)
           (False, [])
  | otherwise
      = (noProblem && (IsSequentialized sequentCirc), sequentCirc)
where
  (allConnected, sequentDone, newCircuit) = CheckTerminals circuit []
  (noProblem, sequentCirc)                = FILLSequentAux [] newCircuit

////////// CheckTerminals
// Removes all start and end terminals from the circuit, while checking that
// all components in the circuit are connected and that there are no terminals
// in any boxes.  Also checks to see if any start terminal is connected directly
// to an end terminal.  If so, it replaces the two terminals with a sequent box
// with an input wire and an output wire corresponding to the wire between the
// terminals, and returns True for its second argument.  (The first argument will
// be True if everything encountered was connected and there were no terminals
// discovered in boxes.)
CheckTerminals :: !Circuit [WireID] -> (Bool, Bool, Circuit)

CheckTerminals [comp=:{spec=StartTerminal, outputs=[{cWireID=outWireID}]} : circuit] outCs
  | outWireID==(-1) = (False, False, [])
  | otherwise       = CheckTerminals circuit [outWireID : outCs]

CheckTerminals [comp=:{spec=EndTerminal, inputs=[inC]} : circuit] outCs
  | inC.cWireID==(-1)
      = (False, False, [])
  | isMember inC.cWireID outCs
      = (True, True, [{comp & spec=Sequent [], inputs=[inC], outputs=[inC]} : circuit])
  | otherwise
      = CheckTerminals circuit outCs

CheckTerminals [box=:{spec=Box boxCircuit, inputs, outputs} : circuit] outCs
  | (any (((==) (-1)) o GetConnectWireID) inputs) ||
    (any (((==) (-1)) o GetConnectWireID) outputs) ||
    (TerminalsOrDisconnectsInBox boxCircuit)
      = (False, False, [])
  | otherwise
      = (success, sequentDone, [box : newCircuit])
where
  (success, sequentDone, newCircuit) = CheckTerminals circuit outCs

CheckTerminals [comp=:{inputs, outputs} : circuit] outCs
  | (any (((==) (-1)) o GetConnectWireID) inputs) ||
    (any (((==) (-1)) o GetConnectWireID) outputs)
      = (False, False, [])
  | otherwise
      = (success, sequentDone, [comp : newCircuit])
where
  (success, sequentDone, newCircuit) = CheckTerminals circuit outCs //*^* Could filter outCs

CheckTerminals [] _ = (True, False, [])

////////// TerminalsOrDisconnectsInBox
TerminalsOrDisconnectsInBox :: !Circuit -> Bool
TerminalsOrDisconnectsInBox [comp=:{spec=StartTerminal} : _] = True
TerminalsOrDisconnectsInBox [comp=:{spec=EndTerminal} : _]   = True
TerminalsOrDisconnectsInBox [box=:{spec=Box boxCircuit, inputs, outputs} : circuit]
  = (any (((==) (-1)) o GetConnectWireID) inputs) || (any (((==) (-1)) o GetConnectWireID) outputs) ||
    (TerminalsOrDisconnectsInBox boxCircuit) || (TerminalsOrDisconnectsInBox circuit)
TerminalsOrDisconnectsInBox [comp=:{inputs, outputs} : circuit]
  = (any (((==) (-1)) o GetConnectWireID) inputs) || (any (((==) (-1)) o GetConnectWireID) outputs) ||
    (TerminalsOrDisconnectsInBox circuit)
TerminalsOrDisconnectsInBox [] = False

////////// IsSequentialized
// A fully sequentialized circuit consists of one sequent box.
IsSequentialized :: !Circuit -> Bool
IsSequentialized [comp=:{spec=Sequent _}] = True
IsSequentialized _ = False

////////// FILLSequentAux
// Takes the front part of a circuit (in reverse order) that has been sequentialized as far as possible
// as its first argument, and the remainder of the circuit as its second, and returns True if no fatal
// errors were encountered (e.g. two sequent boxes connected by more than one wire, a box with wires
// exiting its interior, or a TensorE connected directly to a SumI), with the circuit sequentialized
// as far as possible.
FILLSequentAux :: Circuit !Circuit -> (Bool, Circuit)
FILLSequentAux frontCirc [comp=:{spec=Box boxCircuit, inputs=[inC], outputs=[outC1, outC2]} : circuit]
  | inC.cWireID==outC1.cWireID  // The box's internal input may be connected directly to its internal output.
      = if (isEmpty boxCircuit) // If so, make a sequent box and push it back into frontCirc.
           (if pushedBack1
               (FILLSequentAux newFrontCirc1 circuit)
               (False, [])
           )
           (False, [])
  | isEmpty boxCircuit // The box can only be empty if its internal input is connected to its internal output.
      = (False, [])
  | noProblem && (IsSequentialized newBoxCircuit)
           // The box's internal connections must connect into its internal circuit.
      = if ((isEmpty intoBoxList) || (isEmpty fromBoxList) || (not (isEmpty newOutputs)))  // Last condition for FILL
           (False, [])
           (if pushedBack2
               (FILLSequentAux newFrontCirc2 circuit)
               (False, [])
           )
  | otherwise = (False, [])
where
  (pushedBack1, newFrontCirc1)
    = PushBackSequent {comp & spec=Sequent [], inputs=[], outputs=[outC2]} frontCirc [] []
  (noProblem, newBoxCircuit=:[boxSequent : _]) = FILLSequentAux [] boxCircuit
  (intoBoxList, newOutputs) = filterInOut (((==) (inC.cWireID)) o GetConnectWireID) (boxSequent.outputs)
  (fromBoxList, newInputs)  = filterInOut (((==) (outC1.cWireID)) o GetConnectWireID) (boxSequent.inputs)
  (pushedBack2, newFrontCirc2)
    = PushBackSequent {boxSequent & spec=Sequent [inC.cWireID, outC1.cWireID : (InternalWireIDs boxSequent)],
                                    inputs=newInputs, outputs=[outC2]
                      } frontCirc [] (map GetConnectWireID newInputs)

FILLSequentAux frontCirc [comp=:{spec=TensorE} : circuit]
  = FILLSequentAux [comp : frontCirc] circuit // The TensorE is put into frontCirc to be absorbed by a sequent box later.

FILLSequentAux frontCirc [comp=:{spec=SumI} : circuit]
  | noProblem1
      = if pushedBack
           (FILLSequentAux newFrontCirc1 circuit)
           (if noProblem2
               (FILLSequentAux newFrontCirc3 (tl sumCircuit))
               (False, [])
           )
  | otherwise = (False, [])
where
  // See if SumI can be absorbed by a sequent box.
  (noProblem1, pushedBack, newFrontCirc1) = TryPushBackSum comp frontCirc

  // If not, get components that don't connect down from the SumI (directly or indirectly), and sequentialize them
  // before trying again to sequentialize the SumI and all the components that do connect down from it.
  (sumCircuit, restCircuit)               = SplitFromAbove circuit [comp] [] // Get components that don't connect down
  (noProblem2, newFrontCirc2)             = FILLSequentAux frontCirc restCircuit

  // Push the SumI into frontCirc, absorbing it if possible.
  newFrontCirc3                           = PushBackSum comp newFrontCirc2

// Any other component just becomes a sequent box.
FILLSequentAux frontCirc [comp : circuit]
  | pushedBack = FILLSequentAux newFrontCirc circuit
  | otherwise  = (False, [])
where
  (pushedBack, newFrontCirc) = PushBackSequent {comp & spec=Sequent []} frontCirc [] (map GetConnectWireID comp.inputs)

FILLSequentAux frontCirc [] = (True, frontCirc)

////////// InternalWireIDs
InternalWireIDs :: !Component -> [WireID]
InternalWireIDs {spec=Sequent wireIDs} = wireIDs

////////// PushBackSequent  //*^* Modify to use minimal list of wireIDs for checking (but still keep inbetweens)
// Takes a sequent box, a reversed circuit that has been sequentialized as far as possible, an ordered list of
// components that connect down to the sequent box (directly or indirectly), and a list of the sequent box's wires
// which might be used in a cut (all the original sequent box's wires plus any new input wires acquired by
// absorbing TensorE components).  It pushes the sequent box into the circuit, absorbing other sequent boxes and
// TensorE components when possible.  It returns False if it encounters a sequent box that is connected to the
// new sequent box by more than one cut wire or that is connected by a cut wire and also has an indirect 
// connection to the box.
PushBackSequent :: !Component !Circuit Circuit [WireID] -> (Bool, Circuit)

PushBackSequent comp1=:{spec=Sequent wireIDs, inputs}
                [comp2=:{spec=TensorE, inputs=[inC], outputs=[{cWireID=out1ID}, {cWireID=out2ID}]} : circuit]
                inbetweens cutWires
  | any (((==) out1ID) o GetConnectWireID) inputs
      = if (any (((==) out2ID) o GetConnectWireID) inputs)
           (success1, newCircuit1)
           (PushBackSequent comp1 circuit [comp2 : inbetweens] cutWires)
  | (any (((==) out2ID) o GetConnectWireID) inputs) || (any (ConnectsDown comp2) inbetweens)
      = PushBackSequent comp1 circuit [comp2 : inbetweens] cutWires
  | otherwise
      = (success2, [comp2 : newCircuit2])
where
  (success1, newCircuit1)
    = PushBackSequent {comp1 & spec=Sequent [out1ID, out2ID : wireIDs],
                       inputs=[inC : filter (not o (MemberOf [out1ID, out2ID]) o GetConnectWireID) inputs]
                      } circuit inbetweens [inC.cWireID : cutWires]
  (success2, newCircuit2) = PushBackSequent comp1 circuit inbetweens cutWires

PushBackSequent comp1 [comp2=:{spec=SumI} : circuit] inbetweens cutWires
  | (ConnectsDown comp2 comp1) || (any (ConnectsDown comp2) inbetweens)
      = PushBackSequent comp1 circuit [comp2 : inbetweens] cutWires
  | otherwise
      = (success, [comp2 : newCircuit])
where
  (success, newCircuit) = PushBackSequent comp1 circuit inbetweens cutWires

PushBackSequent comp1=:{spec=Sequent wireIDs1} [comp2=:{spec=Sequent wireIDs2} : circuit] inbetweens cutWires
  | not (isEmpty betweenWires)
      = if ((isEmpty (tl betweenWires)) && (not (any (ConnectsDown comp2) inbetweens)))
           (success1, newCircuit1)
           (False, [])
  | any (ConnectsDown comp2) inbetweens
      = PushBackSequent comp1 circuit [comp2 : inbetweens] cutWires
  | otherwise
      = (success2, [comp2 : newCircuit2])
where
  (betweenWires, otherWires) = filterInOut ((MemberOf cutWires) o GetConnectWireID) comp2.outputs
  (success1, newCircuit1) = PushBackSequent newComp circuit inbetweens cutWires
  newComp = {comp1 & spec=Sequent [GetConnectWireID (hd betweenWires) : (wireIDs1 ++ wireIDs2)],
                     inputs=newInputs, outputs=newOutputs}
  newInputs = (filter (((<>) (GetConnectWireID (hd betweenWires))) o GetConnectWireID) comp1.inputs) ++ comp2.inputs
  newOutputs = comp1.outputs ++ otherWires
  (success2, newCircuit2) = PushBackSequent comp1 circuit inbetweens cutWires

PushBackSequent comp1 [] inbetweens _ = (True, [comp1 : reverse inbetweens])

////////// TryPushBackSum
// Checks to see if a SumI can be absorbed into a sequent box in a reversed circuit that
// has been sequentialized as fully as possible.  Returns False for its first
// argument if it finds that the SumI is directly connected to a TensorE.  The
// second argument is True if the SumI has been put into the circuit (if the SumI
// was absorbed into a sequent box or can never be absorbed into a sequent box
// because it has one or both connections free of anything in the circuit or is
// connected to another SumI that has been left in the circuit for the same reason).
// Returns the newly sequentialized circuit if no problem occurred and the SumI
// was put into it.
TryPushBackSum :: !Component !Circuit -> (Bool, Bool, Circuit)
TryPushBackSum comp1=:{inputs=[{cWireID=in1ID}, {cWireID=in2ID}], outputs=[outC]}
               [comp2=:{spec=Sequent wireIDs, outputs} : circuit]
  | any (((==) in1ID) o GetConnectWireID) outputs
      = if (any (((==) in2ID) o GetConnectWireID) outputs)
           (True, True, newCircuit1)
           (if (IsTensorE otherComp1)
               (False, False, [])
               (if (IsStartTerminal otherComp1)  // True if the SumI is not connected to anything else in the circuit
                   (True, True, newCircuit2)
                   (True, False, [])
               )
           )
  | any (((==) in2ID) o GetConnectWireID) outputs
      = if (IsTensorE otherComp2)
           (False, False, [])
           (if (IsStartTerminal otherComp2)  // True if the SumI is not connected to anything else in the circuit
               (True, True, newCircuit2)
               (True, False, [])
           )
  | otherwise
      = (noProblem, pushedBack, [comp2 : newCircuit3])
where
  newCircuit1    = [{comp2 & spec=Sequent [in1ID, in2ID : wireIDs],
                     outputs=[outC : filter (not o (MemberOf [in1ID, in2ID]) o GetConnectWireID) outputs]}
                    : circuit]
  otherComp1     = FindCompFromOutput in2ID circuit
  otherComp2     = FindCompFromOutput in1ID circuit
  newCircuit2    = [comp1, comp2 : circuit]
  (noProblem, pushedBack, newCircuit3) = TryPushBackSum comp1 circuit

TryPushBackSum comp1 [comp2=:{spec=TensorE} : circuit]
  | ConnectsDown comp2 comp1
       = (False, False, [])
  | otherwise
       = (noProblem, pushedBack, [comp2 : newCircuit])
where
  (noProblem, pushedBack, newCircuit) = TryPushBackSum comp1 circuit

TryPushBackSum comp1 [comp2 : circuit]  // comp2 must be a SumI
  | ConnectsDown comp2 comp1
      = (True, True, [comp1, comp2 : circuit])
  | otherwise
      = (noProblem, pushedBack, [comp2 : newCircuit])
where
  (noProblem, pushedBack, newCircuit) = TryPushBackSum comp1 circuit

TryPushBackSum comp1 [] = (True, True, [comp1])  // New SumI is not connected to anything in the circuit.

////////// FindCompFromOutput
FindCompFromOutput :: WireID !Circuit -> Component
FindCompFromOutput wireID [comp : circuit]
  | any (((==) wireID) o GetConnectWireID) comp.outputs = comp
  | otherwise                                           = FindCompFromOutput wireID circuit

FindCompFromOutput _ [] = {dummyComponent & spec=StartTerminal}

////////// PushBackSum
// Pushes a SumI as back into a reversed circuit that has been sequentialized as
// far as possible, absorbing it into a sequent box if possible.
PushBackSum :: !Component !Circuit -> Circuit
PushBackSum comp1=:{inputs=[{cWireID=in1ID}, {cWireID=in2ID}], outputs=[outC]}
            [comp2=:{spec=Sequent wireIDs, outputs} : circuit]
  | any (((==) in1ID) o GetConnectWireID) outputs
      = if (any (((==) in2ID) o GetConnectWireID) outputs)
           [{comp2 & spec=Sequent [in1ID, in2ID : wireIDs],
                          outputs=[outC : filter (not o (MemberOf [in1ID, in2ID]) o GetConnectWireID) outputs]
            } : circuit
           ]
           [comp1, comp2 : circuit]
  | any (((==) in2ID) o GetConnectWireID) outputs
      = [comp1, comp2 : circuit]
  | otherwise
      = [comp2 : PushBackSum comp1 circuit]

PushBackSum comp1 [comp2 : circuit]
  | ConnectsDown comp2 comp1
      = [comp1, comp2 : circuit]
  | otherwise
      = [comp2 : PushBackSum comp1 circuit]

PushBackSum comp1 [] = [comp1]

////////// SplitFromAbove
// Sorts components from the first circuit into connectedCirc if they connect down from it
// or into remainingCirc if not.
SplitFromAbove :: !Circuit Circuit Circuit -> (Circuit, Circuit)
SplitFromAbove [comp : circuit] connectedCirc remainingCirc
  | any ((flip ConnectsDown) comp) connectedCirc
      = SplitFromAbove circuit [comp : connectedCirc] remainingCirc
  | otherwise
      = SplitFromAbove circuit connectedCirc [comp : remainingCirc]

SplitFromAbove [] connectedCirc remainingCirc = (reverse connectedCirc, reverse remainingCirc)

////////// IsTensorE
IsTensorE :: !Component -> Bool
IsTensorE {spec=TensorE} = True
IsTensorE _              = False
