
implementation module debug

import StdEnv, circuitDefs, utilities

:: Debug = DEBUG (CompSpecifics Debug) ComponentID [Debug_Connect] [Debug_Connect] Placement

:: Debug_Connect = CONNECT WireID WireType

:: Debug_Wire = WIRE WireID Line WireType

CheckConnections :: !Circuit [WireID] -> (Bool, [WireID])
CheckConnections [comp=:{spec=Box boxCircuit, inputs=[inC], outputs=[outC1, outC2]} : circuit] outWireIDs
  | not noBoxLeftovers
      = (False, [])
  | inCWireID <> (-1) && not (any ((==) inCWireID) outWireIDs2)
      = (False, [])
  | otherwise
      = CheckConnections circuit (if (outC2WireID==(-1)) outWireIDs3 [outC2WireID : outWireIDs3])
where
  outC1WireID = GetConnectWireID outC1
  outC2WireID = GetConnectWireID outC2
  inCWireID   = GetConnectWireID inC
  (noBoxLeftovers, outWireIDs2)
    = if (outC1WireID==(-1))
         (CheckConnections boxCircuit outWireIDs)
         (CheckConnections boxCircuit [outC1WireID : outWireIDs])
  outWireIDs3
    = if (inCWireID==(-1))
         outWireIDs2
         (filter ((<>) inCWireID) outWireIDs2)

CheckConnections [comp=:{inputs, outputs} : circuit] outWireIDs
  | isEmpty leftovers
      = CheckConnections circuit ((filter ((<>) (-1)) (map GetConnectWireID outputs)) ++ newOutWireIDs)
  | otherwise
      = (False, [])
where
  inputWireIDs  = map GetConnectWireID inputs
  leftovers     = filter (not o (MemberOf [(-1) : outWireIDs])) inputWireIDs
  newOutWireIDs = filter (not o (MemberOf inputWireIDs)) outWireIDs

CheckConnections [] outWireIDs = (True, outWireIDs)

///////////////////////////

instance == Component
where
  (==) :: !Component !Component -> Bool
  (==) comp1=:{spec=spec1, id=id1, inputs=inputs1, outputs=outputs1, pos=pos1}
       comp2=:{spec=spec2, id=id2, inputs=inputs2, outputs=outputs2, pos=pos2}
    = spec1==spec2 && id1==id2 && inputs1==inputs2 && outputs1==outputs2 && pos1==pos2

instance == CompSpecifics a  | == a
where
  (==) :: !(CompSpecifics a) !(CompSpecifics a) -> Bool | == a
  (==) StartTerminal StartTerminal           = True
  (==) EndTerminal EndTerminal               = True
  (==) TensorE TensorE                       = True
  (==) TensorI TensorI                       = True
  (==) SumE SumE                             = True
  (==) SumI SumI                             = True
  (==) Lolly Lolly                           = True
  (==) UnitI UnitI                           = True
  (==) UnitEL UnitEL                         = True
  (==) UnitER UnitER                         = True
  (==) CounitE CounitE                       = True
  (==) CounitIL CounitIL                     = True
  (==) CounitIR CounitIR                     = True
  (==) (Box boxCircuit1) (Box boxCircuit2)   = boxCircuit1==boxCircuit2
  (==) (Sequent wireIDs1) (Sequent wireIDs2) = wireIDs1==wireIDs2
  (==) _ _ = False

instance == Placement
where
  (==) :: !Placement !Placement -> Bool
  (==) (PT pt1) (PT pt2)     = pt1==pt2
  (==) (RCT rct1) (RCT rct2) = rct1==rct2
  (==) _ _ = False

instance == Connection
where
  (==) :: !Connection !Connection -> Bool
  (==) {cWireID=cWireID1, cWireType=cWireType1} {cWireID=cWireID2, cWireType=cWireType2}
    = cWireID1==cWireID2 && cWireType1==cWireType2

ConvertToCirc :: ![Debug] -> [Component]
ConvertToCirc debug = map ConvertToComp debug

ConvertToComp :: !Debug -> Component
ConvertToComp (DEBUG spec id inputs outputs pos)
  = {spec=ConvertSpec spec, id=id, inputs=map ConvertToConnect inputs, outputs=map ConvertToConnect outputs, pos=pos}

ConvertSpec :: !(CompSpecifics Debug) -> (CompSpecifics Component)
ConvertSpec StartTerminal = StartTerminal
ConvertSpec EndTerminal = EndTerminal
ConvertSpec TensorE = TensorE 
ConvertSpec TensorI = TensorI
ConvertSpec SumE = SumE
ConvertSpec SumI = SumI
ConvertSpec Lolly = Lolly
ConvertSpec (Box circ) = Box (map ConvertToComp circ)
ConvertSpec UnitI = UnitI
ConvertSpec UnitEL = UnitEL
ConvertSpec UnitER = UnitER
ConvertSpec CounitE = CounitE
ConvertSpec CounitIL = CounitIL
ConvertSpec CounitIR = CounitIR
ConvertSpec (Sequent wireIDs) = Sequent wireIDs
ConvertSpec (Generic str inputTypes outputTypes) = Generic str inputTypes outputTypes

ConvertToConnect :: !Debug_Connect -> Connection
ConvertToConnect (CONNECT wireID wireType) = {cWireID=wireID, cWireType=wireType}

ConvertToWires :: ![Debug_Wire] -> [Wire]
ConvertToWires wires = map ConvertToWire wires

ConvertToWire :: !Debug_Wire -> Wire
ConvertToWire (WIRE wireID wireLine wireType) = {wireID=wireID, wireLine=wireLine, wireType=wireType}

instance == Debug
where
  (==) :: !Debug !Debug -> Bool
  (==) (DEBUG spec1 id1 inputs1 outputs1 pos1)
       (DEBUG spec2 id2 inputs2 outputs2 pos2)
    = spec1==spec2 && id1==id2 && inputs1==inputs2 && outputs1==outputs2 && pos1==pos2

instance == Debug_Connect
where
  (==) :: !Debug_Connect !Debug_Connect -> Bool
  (==) (CONNECT wireID1 wireType1) (CONNECT wireID2 wireType2)
    = wireID1==wireID2 && wireType1==wireType2

instance == Wire
where
  (==) :: !Wire !Wire -> Bool
  (==) {wireID=wireID1, wireLine=wireLine1, wireType=wireType1}
       {wireID=wireID2, wireLine=wireLine2, wireType=wireType2}
    = wireID1==wireID2 && wireLine1==wireLine2 && wireType1==wireType2

/////////// instance == WireType
instance == WireType
where
  (==) :: !WireType !WireType -> Bool
  (==) (User typeA) (User typeB)
    = (typeA==typeB)
  (==) (Free typeA) (Free typeB)
    = (typeA==typeB)
  (==) _ _ = False

/////////// instance == Type
instance == Type
where
  (==) :: !Type !Type -> Bool
  (==) (Var a) (Var b)
    = a==b
  (==) (UserVar1 a) (UserVar1 b)
    = a==b
  (==) (UserVar2 a) (UserVar2 b)
    = a==b
  (==) (Const a) (Const b)
    = a==b
  (==) (Product (type1A, type2A)) (Product (type1B, type2B))
    = (type1A==type1B) && (type2A==type2B)
  (==) (Sum (type1A, type2A)) (Sum (type1B, type2B))
    = (type1A==type1B) && (type2A==type2B)
  (==) (Then (type1A, type2A)) (Then (type1B, type2B))
    = (type1A==type1B) && (type2A==type2B)
  (==) (UserFunc name1 types1) (UserFunc name2 types2)
    = (name1==name2) && (types1==types2)
  (==) Unit Unit = True
  (==) Counit Counit = True
  (==) _ _ = False

