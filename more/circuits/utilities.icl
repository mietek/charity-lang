// Contains a variety of functions used by multiple modules, including functions used to draw components
// and wires and to find component connection points.

implementation module utilities

import StdEnv, circuitDefs, unify

///////// < Connection
instance < Connection
where
  (<) :: !Connection !Connection -> Bool
  (<) {cWireID=id1} {cWireID=id2} = id1 < id2

/////////// NotFullyWired
// Checks whether any component in the circuit has a connection that hasn't been connected to a wire.
NotFullyWired :: !Circuit -> Bool
NotFullyWired [box=:{spec=Box boxCircuit, inputs, outputs} : circuit]
  = (Disconnected inputs) || (Disconnected outputs) || (NotFullyWired boxCircuit) || (NotFullyWired circuit)
NotFullyWired [comp=:{inputs, outputs} : circuit]
  = (Disconnected inputs) || (Disconnected outputs) || (NotFullyWired circuit)
NotFullyWired [] = False

/////////// Disconnected
Disconnected :: ![Connection] -> Bool
Disconnected [{cWireID=(-1)} : _] = True
Disconnected [_ : connects]       = Disconnected connects
Disconnected []                   = False

//////// IsNotError
IsNotError :: !ErrorVal -> Bool
IsNotError NoError = True
IsNotError _       = False

//////// GetInWireIDs
// Gets wire IDs for all a component's external input wires (for most components, just their own input wires,
// and for boxes, all input wires entering the box).
//*^* Note that it would be more efficient to proceed down the box's circuit, keeping a list of of wire IDs for
//*^* wires whose lower connection point hasn't been found yet and adding input wire IDs to the list of the box's
//*^* external input wire IDs when they aren't found in the first list.
GetInWireIDs :: !Component -> [WireID]
GetInWireIDs box=:{spec=Box boxCircuit, inputs=[{cWireID=inWireID}], outputs=[{cWireID=outWireID1}, _]}
  | inWireID==(-1)
      = NotConnectedToBox inWires
  | otherwise
      = NotConnectedToBox [inWireID : inWires]
where
  inWires = flatten (map GetInWireIDs boxCircuit)
  NotConnectedToBox = filter (not o (MemberOf (GetAllOutWireIDs box)))

GetInWireIDs {inputs} = GetInWireIDsAux inputs
where
  GetInWireIDsAux [{cWireID} : inCs]
    | cWireID==(-1) = GetInWireIDsAux inCs
    | otherwise     = [cWireID : GetInWireIDsAux inCs]
  GetInWireIDsAux [] = []

////////// GetAllInWireIDs
// Gets wire IDs for all a component's input wires (for most components, just their own input wires,
// and for a box, the wire into its own input plus all input wires for all components in the box circuit).
GetAllInWireIDs :: !Component -> [WireID]
GetAllInWireIDs {spec=Box boxCircuit, inputs=:[{cWireID}]}
  | cWireID==(-1) = flatten (map GetAllInWireIDs boxCircuit)
  | otherwise    = [cWireID : flatten (map GetAllInWireIDs boxCircuit)]
GetAllInWireIDs {inputs} = GetAllInWireIDsAux inputs
where
  GetAllInWireIDsAux [{cWireID} : inCs]
    | cWireID==(-1) = GetAllInWireIDsAux inCs
    | otherwise    = [cWireID : GetAllInWireIDsAux inCs]
  GetAllInWireIDsAux [] = []

////////// GetOutWireIDs
// Gets wire IDs for all a component's external output wires (for most components, just their own output wires,
// and for boxes, all output wires exiting the box plus the wire from the box's external output).
//*^* Note that it would be more efficient to proceed down the box's circuit, keeping a list of of wire IDs for
//*^* wires whose lower connection point hasn't been found yet.
GetOutWireIDs :: !Component -> [WireID]
GetOutWireIDs box=:{spec=Box boxCircuit, outputs=[{cWireID=outWireID1}, {cWireID=outWireID2}]}
  | outWireID1==(-1)
      = if (outWireID2==(-1))
           (NotConnectedToBox outWires)
           ([outWireID2 : NotConnectedToBox outWires])
  | outWireID2==(-1)
      = NotConnectedToBox [outWireID1 : outWires]
  | otherwise
      = [outWireID2 : NotConnectedToBox [outWireID1 : outWires]]
where
  outWires = flatten (map GetOutWireIDs boxCircuit)
  NotConnectedToBox = filter (not o (MemberOf (GetAllInWireIDs box)))

GetOutWireIDs {outputs} = GetOutWireIDsAux outputs
where
  GetOutWireIDsAux [{cWireID} : outCs]
    | cWireID==(-1) = GetOutWireIDsAux outCs
    | otherwise    = [cWireID : GetOutWireIDsAux outCs]
  GetOutWireIDsAux [] = []

////////// GetAllOutWireIDs
// Gets wire IDs for all a component's input wires (for most components, just their own output wires,
// and for a box, its own output wires plus all output wires for all components in the box circuit).
GetAllOutWireIDs :: !Component -> [WireID]
GetAllOutWireIDs {spec=Box boxCircuit, outputs=:[{cWireID=cWireID1}, {cWireID=cWireID2}]}
  | cWireID1==(-1)
      = if (cWireID2==(-1))
           outWires
           [cWireID2 : outWires]
  | cWireID2==(-1) = [cWireID1 : outWires]
  | otherwise      = [cWireID1, cWireID2 : outWires]
where
  outWires = (flatten (map GetAllOutWireIDs boxCircuit))

GetAllOutWireIDs {outputs} = GetAllOutWireIDsAux outputs
where
  GetAllOutWireIDsAux [{cWireID} : outCs]
    | cWireID==(-1) = GetAllOutWireIDsAux outCs
    | otherwise    = [cWireID : GetAllOutWireIDsAux outCs]
  GetAllOutWireIDsAux [] = []

////////// MoveComponent
// Changes the position of the component by adding the given vector offset to its position
// coordinates.
MoveComponent :: !Vector !Component -> Component
MoveComponent (dx,dy) comp=:{spec=Box boxCircuit, pos=RCT ((leftX, topY), (rightX, bottomY))}
  = {comp & pos=RCT ((leftX+dx, topY+dy), (rightX+dx, bottomY+dy)),
            spec=Box (map (MoveComponent (dx,dy)) boxCircuit)}
MoveComponent (dx,dy) comp=:{pos=RCT ((leftX, topY), (rightX, bottomY))}
  = {comp & pos=RCT ((leftX+dx, topY+dy), (rightX+dx, bottomY+dy))}
MoveComponent (dx,dy) comp=:{pos=PT (x,y)}
  = {comp & pos=PT (x+dx, y+dy)}

////////// RemoveWholeComp
// Removes a component (identified by the component ID of the component
// passed as an argument) from the circuit - in the case of boxes,
// the box and its box circuit are removed.
//*^* Note that it would be more efficient to take the component's ID
//*^* and component rectangle as arguments.
RemoveWholeComp :: !Component !Circuit -> Circuit

RemoveWholeComp comp1=:{id=compID} [comp2=:{spec=Box boxCircuit, id=boxID, pos=RCT rectangle} : circuit]
  | (compID==boxID) = circuit
  | Encloses rectangle (ComponentRectangle comp1)
      = [{comp2 & spec=Box (RemoveWholeComp comp1 boxCircuit)} : circuit]
  | otherwise
      = [comp2 : RemoveWholeComp comp1 circuit]

RemoveWholeComp comp1=:{id=compID} [comp2=:{id} : circuit]
  | compID==id = circuit
  | otherwise  = [comp2 : RemoveWholeComp comp1 circuit]

////////// IsInRectangle
// Returns True if the point is in the rectangle (or on its border),
// False otherwise.
IsInRectangle :: !Point !Rectangle -> Bool
IsInRectangle (x,y) ((leftX, topY), (rightX, bottomY))
  = (leftX <= x) && (x <= rightX) && (topY <= y) && (y <= bottomY)

////////// Encloses
// Returns True if the first rectangle completely encloses the first, False
// otherwise.  Their borders should not overlap.
Encloses :: !Rectangle !Rectangle -> Bool
Encloses ((left1, top1), (right1, bottom1)) ((left2, top2), (right2, bottom2))
  = left1 < left2 && left2 < right1  && left1 < right2  && right2  < right1 &&
    top1  < top2  && top2  < bottom1 && top1  < bottom2 && bottom2 < bottom1

////////// OverlapCheck
// Checks whether the component's enclosing rectangle overlaps any component in
// the circuit.  The component may be positioned inside a box in the circuit,
// but must not overlap its borders or any component in its box circuit.
OverlapCheck :: Component !Circuit -> Bool
OverlapCheck comp circuit
  = any (Overlaps (ComponentRectangle comp)) (flatten (map ComponentRectangles circuit))

////////// Overlaps
// Returns True if the two rectangles overlap each other (including borders), False otherwise.
Overlaps :: !Rectangle !Rectangle -> Bool
Overlaps rectangle1 rectangle2
  = IsInRectangle (leftX1 , topY1   ) rectangle2 ||
    IsInRectangle (rightX1, bottomY1) rectangle2 ||
    IsInRectangle (leftX2 , bottomY2) rectangle1 ||
    IsInRectangle (rightX2, topY2   ) rectangle1 ||
    (leftX2 <= leftX1 && leftX1 <= rightX2 && topY1 <= topY2 && topY2 <= bottomY1) ||
    (leftX1 <= leftX2 && leftX2 <= rightX1 && topY2 <= topY1 && topY1 <= bottomY2)
where
  ((leftX1, topY1), (rightX1, bottomY1)) = rectangle1
  ((leftX2, topY2), (rightX2, bottomY2)) = rectangle2

////////// RectangleCentre
// Returns the centre point of a rectangle.
RectangleCentre :: !Rectangle -> Point
RectangleCentre ((x1,y1), (x2,y2)) = ((x1+x2)/2, (y1+y2)/2)

////////// GetConnectWireID
// Returns the wire ID of a connection.
GetConnectWireID :: !Connection -> WireID
GetConnectWireID {cWireID} = cWireID

////////// GetConnectWireType
// Returns the wire type of a connection.
GetConnectWireType :: !Connection -> WireType
GetConnectWireType {cWireType} = cWireType

////////// GetWireID
// Returns the wire ID of a wire.
GetWireID :: !Wire -> WireID
GetWireID {wireID} = wireID

////////// GetWireType
// Returns the wire type of a wire.
GetWireType :: !Wire -> WireType
GetWireType {wireType} = wireType

///////////////////////////////////////////////////////

////////// OutConnectionPoint
// Given a component specification, the component's placement, and an
// integer specifying the desired connection, returns the output connection
// point.  For boxes, the internal output connection is 0 and the external
// output connection is 1.  For other components, output connections are
// numbered from left to right, starting at 0.
//*^*OutConnectionPoint :: !CompSpecifics !Placement !Int -> Point
OutConnectionPoint :: !(CompSpecifics Component) !Placement !Int -> Point
OutConnectionPoint TensorE (PT centre) 0 = Out1ConnectionPoint centre
OutConnectionPoint TensorE (PT centre) 1 = Out2ConnectionPoint centre
OutConnectionPoint TensorI (PT centre) 0 = CentreOutConnection centre
OutConnectionPoint SumE (PT centre) 0 = Out1ConnectionPoint centre
OutConnectionPoint SumE (PT centre) 1 = Out2ConnectionPoint centre
OutConnectionPoint SumI (PT centre) 0 = CentreOutConnection centre
OutConnectionPoint Lolly (PT centre) 0 = CentreOutConnection centre
OutConnectionPoint StartTerminal (PT centre) 0 = TerminalOutConnection centre
OutConnectionPoint UnitI (PT centre) 0 = CentreOutConnection centre
OutConnectionPoint UnitEL (PT centre) 0 = LassoOutConnection centre
OutConnectionPoint UnitER (PT centre) 0 = LassoOutConnection centre
OutConnectionPoint CounitIL (PT centre) 0 = LassoLeftOutConnection centre
OutConnectionPoint CounitIL (PT centre) 1 = LassoOutConnection centre
OutConnectionPoint CounitIR (PT centre) 0 = LassoOutConnection centre
OutConnectionPoint CounitIR (PT centre) 1 = LassoRightOutConnection centre
OutConnectionPoint (Box _) (RCT rectangle) 0 = BoxInternalOutConnection rectangle
OutConnectionPoint (Box _) (RCT rectangle) 1 = BoxExternalOutConnection rectangle
OutConnectionPoint (Generic _ _ outputTypes) (RCT ((boxLeftX, _), (boxRightX, boxBottomY))) n
  = (boxLeftX + (toInt (connectWidth/2.0 + (toReal n)*connectWidth)), boxBottomY + connectLength)
where
  connectWidth = (toReal (boxRightX - boxLeftX)) / (toReal (length outputTypes))

////////// IsOutConnection
// Given a component's specification and placement, a mouse position that
// might be near a connection, and an integer identifying the output
// connection to be checked, returns True if the mouse position is close
// enough to the connection, and False otherwise.  //*^* Note:  it might
// be a good idea to move the individual functions into this main function.
// Also, what is "near enough" varies depending on the component, which is
// possibly not such a good idea.  (In most cases, the component is divided
// into rectangular regions, one for each connection, and the point is
// considered to be near enough to a connection if it falls in its region
// - regions vary in size.
//*^*IsOutConnection :: !CompSpecifics !Placement Point Int -> Bool
IsOutConnection :: !(CompSpecifics Component) !Placement Point Int -> Bool
IsOutConnection TensorE (PT centre) nearPoint 0
  = IsOut1Connection nearPoint centre
IsOutConnection TensorE (PT centre) nearPoint 1
  = IsOut2Connection nearPoint centre
IsOutConnection TensorI (PT centre) nearPoint 0
  = IsCentreOutConnection nearPoint centre
IsOutConnection SumE (PT centre) nearPoint 0
  = IsOut1Connection nearPoint centre
IsOutConnection SumE (PT centre) nearPoint 1
  = IsOut2Connection nearPoint centre
IsOutConnection SumI (PT centre) nearPoint 0
  = IsCentreOutConnection nearPoint centre
IsOutConnection Lolly (PT centre) nearPoint 0
  = IsCentreOutConnection nearPoint centre
IsOutConnection StartTerminal (PT centre) nearPoint 0
  = IsTerminalOutConnection nearPoint centre
IsOutConnection UnitI (PT centre) nearPoint 0
  = IsCentreOutConnection nearPoint centre
IsOutConnection UnitEL (PT centre) nearPoint 0
  = IsLassoOutConnection nearPoint centre
IsOutConnection UnitER (PT centre) nearPoint 0
  = IsLassoOutConnection nearPoint centre
IsOutConnection CounitIL (PT centre) nearPoint 0
  = IsLassoLeftOutConnection nearPoint centre
IsOutConnection CounitIL (PT centre) nearPoint 1
  = IsLassoOutConnection nearPoint centre
IsOutConnection CounitIR (PT centre) nearPoint 0
  = IsLassoOutConnection nearPoint centre
IsOutConnection CounitIR (PT centre) nearPoint 1
  = IsLassoRightOutConnection nearPoint centre
IsOutConnection (Box _) (RCT rectangle) nearPoint 0
  = IsBoxInternalOutConnection nearPoint rectangle
IsOutConnection (Box _) (RCT rectangle) nearPoint 1
  = IsBoxExternalOutConnection nearPoint rectangle
IsOutConnection (Generic _ _ outputTypes) (RCT ((leftX, _), (rightX, bottomY))) (nearX, nearY) n
  = (bottomY <= nearY) && (nearY <= (bottomY + connectLength + connectMargin)) &&
    (leftBoundX <= nearX) && (nearX <= rightBoundX)
where
  leftBoundX     = toInt realLeftBoundX
  rightBoundX    = toInt (realLeftBoundX + connectWidth)
  realLeftBoundX = (toReal leftX) + (toReal n) * connectWidth
  connectWidth   = (toReal (rightX - leftX)) / (toReal (length outputTypes))

////////// InConnectionPoint
// Given a component specification, the component's placement, and an
// integer specifying the desired connection, returns the input connection
// point.  Input connections are numbered from left to right, starting
// at 0.  (A box's internal input connection is numbered 0.)
//*^*InConnectionPoint :: !CompSpecifics !Placement !Int -> Point
InConnectionPoint :: !(CompSpecifics Component) !Placement !Int -> Point
InConnectionPoint TensorE (PT centre) 0 = CentreInConnection centre
InConnectionPoint TensorI (PT centre) 0 = In1ConnectionPoint centre
InConnectionPoint TensorI (PT centre) 1 = In2ConnectionPoint centre
InConnectionPoint SumE (PT centre) 0 = CentreInConnection centre
InConnectionPoint SumI (PT centre) 0 = In1ConnectionPoint centre
InConnectionPoint SumI (PT centre) 1 = In2ConnectionPoint centre
InConnectionPoint Lolly (PT centre) 0 = In1ConnectionPoint centre
InConnectionPoint Lolly (PT centre) 1 = In2ConnectionPoint centre
InConnectionPoint EndTerminal (PT centre) 0 = TerminalInConnection centre
InConnectionPoint UnitEL (PT centre) 0 = LassoLeftInConnection centre
InConnectionPoint UnitEL (PT centre) 1 = LassoInConnection centre
InConnectionPoint UnitER (PT centre) 0 = LassoInConnection centre
InConnectionPoint UnitER (PT centre) 1 = LassoRightInConnection centre
InConnectionPoint CounitE (PT centre) 0 = CentreInConnection centre
InConnectionPoint CounitIL (PT centre) 0 = LassoInConnection centre
InConnectionPoint CounitIR (PT centre) 0 = LassoInConnection centre
InConnectionPoint (Box boxCircuit) (RCT rectangle) 0 = BoxInputConnection rectangle
InConnectionPoint (Generic _ inputTypes _) (RCT ((boxLeftX, boxTopY), (boxRightX, _))) n
  = (boxLeftX + (toInt (connectWidth/2.0 + (toReal n)*connectWidth)), boxTopY - connectLength)
where
  connectWidth = (toReal (boxRightX - boxLeftX)) / (toReal (length inputTypes))

////////// IsInConnection
// Given a component's specification and placement, a mouse position that
// might be near a connection, and an integer identifying the input
// connection to be checked, returns True if the mouse position is close
// enough to the connection, and False otherwise.  //*^* Note:  it might
// be a good idea to move the individual functions into this main function.
// Also, what is "near enough" varies depending on the component, which is
// possibly not such a good idea.  (In most cases, the component is divided
// into rectangular regions, one for each connection, and the point is
// considered to be near enough to a connection if it falls in its region
// - regions vary in size.
//*^*IsInConnection :: !CompSpecifics !Placement Point Int -> Bool
IsInConnection :: !(CompSpecifics Component) !Placement Point Int -> Bool
IsInConnection TensorE (PT centre) nearPoint 0
  = IsCentreInConnection nearPoint centre
IsInConnection TensorI (PT centre) nearPoint 0
  = IsIn1Connection nearPoint centre
IsInConnection TensorI (PT centre) nearPoint 1
  = IsIn2Connection nearPoint centre
IsInConnection SumE (PT centre) nearPoint 0
  = IsCentreInConnection nearPoint centre
IsInConnection SumI (PT centre) nearPoint 0
  = IsIn1Connection nearPoint centre
IsInConnection SumI (PT centre) nearPoint 1
  = IsIn2Connection nearPoint centre
IsInConnection Lolly (PT centre) nearPoint 0
  = IsIn1Connection nearPoint centre
IsInConnection Lolly (PT centre) nearPoint 1
  = IsIn2Connection nearPoint centre
IsInConnection EndTerminal (PT centre) nearPoint 0
  = IsTerminalInConnection nearPoint centre
IsInConnection UnitEL (PT centre) nearPoint 0
  = IsLassoLeftInConnection nearPoint centre
IsInConnection UnitEL (PT centre) nearPoint 1
  = IsLassoInConnection nearPoint centre
IsInConnection UnitER (PT centre) nearPoint 0
  = IsLassoInConnection nearPoint centre
IsInConnection UnitER (PT centre) nearPoint 1
  = IsLassoRightInConnection nearPoint centre
IsInConnection CounitE (PT centre) nearPoint 0
  = IsCentreInConnection nearPoint centre
IsInConnection CounitIL (PT centre) nearPoint 0
  = IsLassoInConnection nearPoint centre
IsInConnection CounitIR (PT centre) nearPoint 0
  = IsLassoInConnection nearPoint centre
IsInConnection (Box _) (RCT rectangle) nearPoint 0
  = IsBoxInputConnection nearPoint rectangle
IsInConnection (Generic _ inputTypes _) (RCT ((leftX, topY), (rightX, _))) (nearX, nearY) n
  = ((topY - connectLength - connectMargin) <= nearY) && (nearY <= topY) &&
    (leftBoundX <= nearX) && (nearX <= rightBoundX)
where
  leftBoundX     = toInt realLeftBoundX
  rightBoundX    = toInt (realLeftBoundX + connectWidth)
  realLeftBoundX = (toReal leftX) + (toReal n) * connectWidth
  connectWidth   = (toReal (rightX - leftX)) / (toReal (length inputTypes))

////////// GetConnectionPoint
// Given a mouse position and a circuit, returns the connection point of any connection
// that the mouse position is near enough to (that is not already connected to a wire),
// and a four-tuple giving other information about the connection - what kind of
// connection was found (NONE, IN (input), or OUT (output)), the component ID for the
// component, an integer (from 0 up) identifying which of the component's input or
// output connections was selected, and the wire type of the connection.
GetConnectionPoint :: Point !Circuit -> (Point, ConnectionInfo)

GetConnectionPoint nearPoint [{spec=Box boxCircuit, inputs, outputs, pos=RCT rectangle, id} : circuit]
  | isInput && (inConnectID <> (-1))
       = (InConnectionPoint (Box []) (RCT rectangle) inConnectID, (IN, id, inConnectID, inConnectWireType))
  | isOutput && (outConnectID <> (-1))
       = (OutConnectionPoint (Box []) (RCT rectangle) outConnectID, (OUT, id, outConnectID, outConnectWireType))
  | IsInRectangle nearPoint rectangle
      = GetConnectionPoint nearPoint boxCircuit
  | otherwise
      = GetConnectionPoint nearPoint circuit
where
  // inConnectID is an integer identifying which input connection was selected - it is -1 when the connection
  // is already connected to a wire.
  (isInput, inConnectID, inConnectWireType) = GetInputConnection (Box []) (RCT rectangle) nearPoint inputs 0

  // Similarly...
  (isOutput, outConnectID, outConnectWireType) = GetOutputConnection (Box []) (RCT rectangle) nearPoint outputs 0

GetConnectionPoint nearPoint [{spec, inputs, outputs, pos, id} : circuit]
  | isInput && (inConnectID <> (-1))
      = (InConnectionPoint spec pos inConnectID, (IN, id, inConnectID, inConnectWireType))
  | isOutput && (outConnectID <> (-1))
      = (OutConnectionPoint spec pos outConnectID, (OUT, id, outConnectID, outConnectWireType))
  | otherwise = GetConnectionPoint nearPoint circuit
where
  (isInput, inConnectID, inConnectWireType) = GetInputConnection spec pos nearPoint inputs 0
  (isOutput, outConnectID, outConnectWireType) = GetOutputConnection spec pos nearPoint outputs 0

GetConnectionPoint nearPoint [] = ((-1,-1), (NONE, -1, -1, dummyWireType))

////////// IsNotConnected 
// Used to determine if the information returned from GetConnectionPoint
// indicates that a connection point was found for the mouse position.
IsNotConnected :: !ConnectionInfo -> Bool
IsNotConnected (NONE, _, _, _) = True
IsNotConnected _               = False

////////// GetInputConnection
// Takes the specification and placement of a component, a mouse position that might be near one
// of the component's input connections, a list of the component's input connections, and an
// integer identifying the input connection at the front of the list.  Returns True if the mouse
// position was found to be near any of the input connections (False otherwise), a connection ID
// which identifies the connection if it is not connected to a wire and is -1 otherwise, and the
// connection's wire type.
//*^*GetInputConnection :: CompSpecifics Placement Point ![Connection] Int -> (Bool, ConnectionID, WireType)
GetInputConnection :: (CompSpecifics Component) Placement Point ![Connection] Int -> (Bool, ConnectionID, WireType)

GetInputConnection spec pos nearPoint [{cWireType, cWireID} : inCs] n
  | IsInConnection spec pos nearPoint n
      = if (cWireID==(-1))
           (True, n, cWireType)
           (True, -1, cWireType)  //*^* -1 indicates that the input is already connected
  | otherwise
      = GetInputConnection spec pos nearPoint inCs (n+1)

GetInputConnection _ _ _ [] _ = (False, -1, dummyWireType)

////////// GetOutputConnection
// Takes the specification and placement of a component, a mouse position that might be near one
// of the component's output connections, a list of the component's output connections, and an
// integer identifying the output connection at the front of the list.  Returns True if the mouse
// position was found to be near any of the output connections (False otherwise), a connection ID
// which identifies the connection if it is not connected to a wire and is -1 otherwise, and the
// connection's wire type.
//*^*GetOutputConnection :: CompSpecifics Placement Point ![Connection] Int -> (Bool, ConnectionID, WireType)
GetOutputConnection :: (CompSpecifics Component) Placement Point ![Connection] Int -> (Bool, ConnectionID, WireType)

GetOutputConnection spec pos nearPoint [{cWireType, cWireID} : outCs] n
  | IsOutConnection spec pos nearPoint n
      = if (cWireID==(-1))
           (True, n, cWireType)
           (True, -1, cWireType)  //*^* -1 indicates that the output is already connected
  | otherwise
      = GetOutputConnection spec pos nearPoint outCs (n+1)

GetOutputConnection _ _ _ [] _ = (False, -1, dummyWireType)

////////// GetSelectedType
// Takes a mouse position, a circuit and a list of the circuit's wires, and returns
// True if the mouse position was near to a connection point or wire in the circuit
// (False otherwise) and the wire type of the connection or wire.
GetSelectedType :: Point !Circuit [Wire] -> (Bool, WireType)
GetSelectedType point circuit wires
  | connectWireTypeFound = (True, connectWireType)
  | otherwise            = GetTypeOnWire point wires
where
  (connectWireTypeFound, connectWireType) = GetTypeOnConnect point circuit

////////// GetTypeOnConnect
// Takes a mouse position and a circuit, and returns True if the mouse position
// was near a connection in the circuit (False otherwise) and the wire type of
// the connection if so.
GetTypeOnConnect :: Point !Circuit -> (Bool, WireType)

GetTypeOnConnect nearPoint [{spec=Box boxCircuit, inputs, outputs, pos=RCT rectangle} : circuit]
  | isInput
      = (True, inWireType)
  | isOutput
      = (True, outWireType)
  | IsInRectangle nearPoint rectangle
      = GetTypeOnConnect nearPoint boxCircuit
  | otherwise
      = GetTypeOnConnect nearPoint circuit
where
  (isInput, _, inWireType) = GetInputConnection (Box []) (RCT rectangle) nearPoint inputs 0
  (isOutput, _, outWireType) = GetOutputConnection (Box []) (RCT rectangle) nearPoint outputs 0

GetTypeOnConnect nearPoint [{spec, inputs, outputs, pos, id} : circuit]
  | isInput   = (True, inWireType)
  | isOutput  = (True, outWireType)
  | otherwise = GetTypeOnConnect nearPoint circuit
where
  (isInput, _, inWireType) = GetInputConnection spec pos nearPoint inputs 0
  (isOutput, _, outWireType) = GetOutputConnection spec pos nearPoint outputs 0

GetTypeOnConnect nearPoint [] = (False, dummyWireType)

//////////  GetTypeOnWire
// Takes a mouse position and a list of wires, and returns True if the 
// mouse position is near one of the wires (False otherwise) and the
// wire type of the wire if so.
GetTypeOnWire :: Point ![Wire] -> (Bool, WireType)
GetTypeOnWire _ [] = (False, Free Unit)
GetTypeOnWire nearPoint [wire=:{wireLine, wireType} : wires]
  | IsOnLine nearPoint wireLine = (True, wireType)
  | otherwise                   = GetTypeOnWire nearPoint wires

////////// IsOnLine
// Returns True if the point is on the line, within a small margin, False otherwise.
IsOnLine :: !Point !Line -> Bool
IsOnLine (x,y) ((x1,y1), (x2,y2))
  = (0 <= testvalue1) && (testvalue2 <= 0) && (~margin <= testvalue3) && (testvalue3 <= margin)
where
  testvalue1 = pointDotProd - (DotProduct (ux,uy) (x1,y1))
  testvalue2 = pointDotProd - (DotProduct (ux,uy) (x2,y2))
  testvalue3 = toReal ((DotProduct norm (x,y)) - (DotProduct norm (x1,y1)))
  pointDotProd = DotProduct (ux, uy) (x,y)
  norm = (uy, ~ux)
  (ux, uy) = (x2-x1, y2-y1)
  margin = 2.0 * (VectorLength (toRealVector norm))

////////// ComponentConnections
// Returns the connection type (IN for input, OUT for output), wire ID
// and connection point for all the component's connections that are
// connected to wires.  For boxes, this includes all such connections
// for components in its box circuit.
ComponentConnections :: !Component -> [(ConnectionType, WireID, Point)]

ComponentConnections {spec=Box boxCircuit, inputs, outputs, pos}
  = (InConnections (Box []) pos inputs 0) ++ (OutConnections (Box []) pos outputs 0)
    ++ (flatten (map ComponentConnections boxCircuit))

ComponentConnections {spec, inputs, outputs, pos}
  = (InConnections spec pos inputs 0) ++ (OutConnections spec pos outputs 0)

////////// InConnections
// Given a component's specification and placement, a list of its input connections, and an
// integer identifying which input connection is at the front of the list, returns a list
// giving the connection type (always IN), wire ID and connection point for every input
// connection that is connected to a wire.
//*^*InConnections :: CompSpecifics Placement ![Connection] Int -> [(ConnectionType, WireID, Point)]
InConnections :: (CompSpecifics Component) Placement ![Connection] Int -> [(ConnectionType, WireID, Point)]
InConnections spec pos [{cWireID} : inCs] n
  | cWireID==(-1) = InConnections spec pos inCs (n+1)
  | otherwise     = [(IN, cWireID, InConnectionPoint spec pos n) : InConnections spec pos inCs (n+1)]
InConnections _ _ [] _ = []

////////// OutConnections
// Given a component's specification and placement, a list of its output connections, and an
// integer identifying which output connection is at the front of the list, returns a list
// giving the connection type (always IN), wire ID and connection point for every output
// connection that is connected to a wire.
//*^*OutConnections :: CompSpecifics Placement ![Connection] Int -> [(ConnectionType, WireID, Point)]
OutConnections :: (CompSpecifics Component) Placement ![Connection] Int -> [(ConnectionType, WireID, Point)]
OutConnections spec pos [{cWireID} : outCs] n
  | cWireID==(-1) = OutConnections spec pos outCs (n+1)
  | otherwise     = [(OUT, cWireID, OutConnectionPoint spec pos n) : OutConnections spec pos outCs (n+1)]
OutConnections _ _ [] _ = []

////////// IsCentreInConnection
// Takes a mouse position and the centre point of a TensorE, SumE or CounitE component, and
// returns True if the mouse position is near enough to the component's input connection.
IsCentreInConnection :: !Point !Point -> Bool
IsCentreInConnection (nearX, nearY) (centreX, centreY)
  = (xDiff >= ~sideXOffset1) && (xDiff <= sideXOffset1) && (yDiff >= 0) && (yDiff <= centreLineOffset1 + connectMargin)
where
  xDiff = centreX - nearX
  yDiff = centreY - nearY

////////// CentreInConnection
// Takes the centre point of a TensorE, SumE or CounitE component and returns the connection
// point of its input connection.
CentreInConnection :: !Point -> Point
CentreInConnection (centreX, centreY) = (centreX, centreY - centreLineOffset1)

////////// IsIn1Connection
// Takes a mouse position and the centre point of a TensorI, SumI or Lolly component and
// returns True if the mouse position is near enough to the component's left input
// connection.
IsIn1Connection :: !Point !Point -> Bool
IsIn1Connection (nearX, nearY) (centreX, centreY)
  = (xDiff >= 0) && (xDiff <= sideXOffset1 + connectMargin) && (yDiff >= 0) && (yDiff <= centreLineOffset1 + connectMargin)
where
  xDiff = centreX - nearX
  yDiff = centreY - nearY

////////// In1ConnectionPoint
// Takes the centre point of a TensorI, SumI or Lolly component and returns its left
// input connection point.
In1ConnectionPoint :: !Point -> Point
In1ConnectionPoint (centreX, centreY) = (centreX - sideXOffset1, centreY - sideYOffset1)

////////// IsIn2Connection
// Takes a mouse position and the centre point of a TensorI, SumI or Lolly component and
// returns True if the mouse position is near enough to the component's right input
// connection.
IsIn2Connection :: !Point !Point -> Bool
IsIn2Connection (nearX, nearY) (centreX, centreY)
  = (xDiff <= 0) && (xDiff >= ~sideXOffset1 - connectMargin) && (yDiff >= 0) && (yDiff <= centreLineOffset1 + connectMargin)
where
  xDiff = centreX - nearX
  yDiff = centreY - nearY

////////// In2ConnectionPoint
// Takes the centre point of a TensorI, SumI or Lolly component and returns its right
// input connection point.
In2ConnectionPoint :: !Point -> Point
In2ConnectionPoint (centreX, centreY) = (centreX + sideXOffset1, centreY - sideYOffset1)

////////// IsCentreOutConnection
// Takes a mouse position and the centre point of a TensorI, SumI, Lolly or UnitI component
// and returns True if the mouse position is near enough to the component's output connection.
IsCentreOutConnection :: !Point !Point -> Bool
IsCentreOutConnection (nearX, nearY) (centreX, centreY)
  = (xDiff >= ~sideXOffset1) && (xDiff <= sideXOffset1) && (yDiff <= 0) && (yDiff >= ~centreLineOffset1 - connectMargin)
where
  xDiff = centreX - nearX
  yDiff = centreY - nearY

////////// CentreOutConnection
// Takes the centre point of a TensorI, SumI, Lolly, or UnitI component and returns the
// component's output connection point.
CentreOutConnection :: !Point -> Point
CentreOutConnection (centreX, centreY) = (centreX, centreY + centreLineOffset1)

////////// IsOut1Connection
// Takes a mouse position and the centre point of a TensorE or SumE component and returns
// True if the mouse position is near enough to the component's left output connection.
IsOut1Connection :: !Point !Point -> Bool
IsOut1Connection (nearX, nearY) (centreX, centreY)
  = (xDiff >= 0) && (xDiff <= sideXOffset1 + connectMargin) && (yDiff <= 0) && (yDiff >= ~centreLineOffset1 - connectMargin)
where
  xDiff = centreX - nearX
  yDiff = centreY - nearY

////////// Out1ConnectionPoint
// Takes the centre point of a TensorE or SumE component and returns the component's left
// output connection point.
Out1ConnectionPoint :: !Point -> Point
Out1ConnectionPoint (centreX, centreY) = (centreX - sideXOffset1, centreY + sideYOffset1)

////////// IsOut2Connection
// Takes a mouse position and the centre point of a TensorE or SumE component and returns
// True if the mouse position is near enough to the component's right output connection.
IsOut2Connection :: !Point !Point -> Bool
IsOut2Connection (nearX, nearY) (centreX, centreY)
  = (xDiff <= 0) && (xDiff >= ~sideXOffset1 - connectMargin) && (yDiff <= 0) && (yDiff >= ~centreLineOffset1 - connectMargin)
where
  xDiff = centreX - nearX
  yDiff = centreY - nearY

////////// Out2ConnectionPoint
// Takes the centre point of a TensorE or SumE component and returns the component's right
// output connection point.
Out2ConnectionPoint :: !Point -> Point
Out2ConnectionPoint (centreX, centreY) = (centreX + sideXOffset1, centreY + sideYOffset1)

////////// IsTerminalOutConnection
// Takes a mouse position and the point at the centre of the base of a StartTerminal and
// returns True if the mouse position is near enough to the component's output connection.
IsTerminalOutConnection :: !Point !Point -> Bool
IsTerminalOutConnection (nearX, nearY) (x,y)
  = (xDiff <= terminalRadius) && (xDiff >= ~terminalRadius) && (yDiff <= 0) && (yDiff >= ~2*terminalRadius - connectMargin)
where
  xDiff = x - nearX
  yDiff = y - nearY

////////// TerminalOutConnection
// Takes the point at the centre of the base of a StartTerminal and returns the component's
// output connection point.
TerminalOutConnection :: !Point -> Point
TerminalOutConnection (x, y) = (x, y + 2*terminalRadius)

////////// IsTerminalInConnection
// Takes a mouse position and the point at the centre of the base of a EndTerminal and
// returns True if the mouse position is near enough to the component's input connection.
IsTerminalInConnection :: !Point !Point -> Bool
IsTerminalInConnection (nearX, nearY) (x,y)
  = (xDiff <= terminalRadius) && (xDiff >= ~terminalRadius) && (yDiff <= 2*terminalRadius + connectMargin) && (yDiff >= 0)
where
  xDiff = x - nearX
  yDiff = y - nearY

////////// TerminalInConnection
// Takes the point at the centre of the base of a EndTerminal and returns the component's
// input connection point.
TerminalInConnection :: !Point -> Point
TerminalInConnection (x, y) = (x, y - 2*terminalRadius)

////////// IsLassoLeftInConnection
// Takes a mouse position and the point at the centre of a UnitEL's lasso and returns
// True if the mouse position is near enough to the component's left input.
IsLassoLeftInConnection :: !Point !Point -> Bool
IsLassoLeftInConnection (nearX, nearY) (x,y)
  = (xDiff <= gateRadius) && (xDiff >= ~gateRadius) && (yDiff <= centreLineOffset1 + connectMargin) && (yDiff >= 0)
where
  xDiff = x - lassoCordRadius - nearX
  yDiff = y - lassoCordRadius - gateRadius - nearY

////////// LassoLeftInConnection
// Takes the point at the centre of a UnitEL's lasso and returns the component's left
// input connection point.
LassoLeftInConnection :: !Point -> Point
LassoLeftInConnection (x,y) = (x - lassoCordRadius, y - lassoCordRadius - gateRadius - centreLineOffset1)

////////// IsLassoLeftOutConnection
// Takes a mouse position and the point at the centre of a CounitIL's lasso and returns
// True if the mouse position is near enough to the component's left output.
IsLassoLeftOutConnection :: !Point !Point -> Bool
IsLassoLeftOutConnection (nearX, nearY) (x,y)
  = (xDiff <= gateRadius) && (xDiff >= ~gateRadius) && (yDiff <= 0) && (yDiff >= ~centreLineOffset1 - connectMargin)
where
  xDiff = x - lassoCordRadius - nearX
  yDiff = y + lassoCordRadius + gateRadius - nearY

////////// LassoLeftOutConnection
// Takes the point at the centre of a CounitIL's lasso and returns the component's left
// output connection point.
LassoLeftOutConnection :: !Point -> Point
LassoLeftOutConnection (x,y) = (x - lassoCordRadius, y + lassoCordRadius + gateRadius + centreLineOffset1)

////////// IsLassoRightInConnection
// Takes a mouse position and the point at the centre of a UnitEL's lasso and returns
// True if the mouse position is near enough to the component's right input.
IsLassoRightInConnection :: !Point !Point -> Bool
IsLassoRightInConnection (nearX, nearY) (x,y)
  = (xDiff <= gateRadius) && (xDiff >= ~gateRadius) && (yDiff <= centreLineOffset1 + connectMargin) && (yDiff >= 0)
where
  xDiff = x + lassoCordRadius - nearX
  yDiff = y - lassoCordRadius - gateRadius - nearY

////////// LassoRightInConnection
// Takes the point at the centre of a UnitEL's lasso and returns the component's right
// input connection point.
LassoRightInConnection :: !Point -> Point
LassoRightInConnection (x,y) = (x + lassoCordRadius, y - lassoCordRadius - gateRadius - centreLineOffset1)

////////// IsLassoRightOutConnection
// Takes a mouse position and the point at the centre of a CounitIL's lasso and returns
// True if the mouse position is near enough to the component's right output.
IsLassoRightOutConnection :: !Point !Point -> Bool
IsLassoRightOutConnection (nearX, nearY) (x,y)
  = (xDiff <= gateRadius) && (xDiff >= ~gateRadius) && (yDiff <= 0) && (yDiff >= ~centreLineOffset1 - connectMargin)
where
  xDiff = x + lassoCordRadius - nearX
  yDiff = y + lassoCordRadius + gateRadius - nearY

////////// LassoRightOutConnection
// Takes the point at the centre of a CounitIL's lasso and returns the component's right
// output connection point.
LassoRightOutConnection :: !Point -> Point
LassoRightOutConnection (x,y) = (x + lassoCordRadius, y + lassoCordRadius + gateRadius + centreLineOffset1)

////////// IsLassoInConnection
// Takes a mouse position and the point at the centre of a UnitEL, UnitER, CounitIL or CounitIR,
// and returns True if the mouse position is near enough to the connection into the component's
// lasso loop.
IsLassoInConnection :: !Point !Point -> Bool
IsLassoInConnection (nearX, nearY) (x,y)
  = (xDiff <= halfLassoWidth) && (xDiff >= ~halfLassoWidth) && (yDiff <= halfLassoLineLength + connectMargin) && (yDiff >= 0)
where
  xDiff = x - nearX
  yDiff = y - nearY

////////// LassoInConnection
// Takes the point at the centre of a UnitEL, UnitER, CounitIL or CounitIR, and returns the
// input connection point of the component's lasso loop.
LassoInConnection :: !Point -> Point
LassoInConnection (x,y) = (x,y) //*^* (x, y - halfLassoLineLength)

////////// IsLassoOutConnection
// Takes a mouse position and the point at the centre of a UnitEL, UnitER, CounitIL or CounitIR,
// and returns True if the mouse position is near enough to the connection out of the component's
// lasso loop.
IsLassoOutConnection :: !Point !Point -> Bool
IsLassoOutConnection (nearX, nearY) (x,y)
  = (xDiff <= halfLassoWidth) && (xDiff >= ~halfLassoWidth) && (yDiff <= 0) && (yDiff >= ~halfLassoLineLength - connectMargin)
where
  xDiff = x - nearX
  yDiff = y - nearY

////////// LassoOutConnection
// Takes the point at the centre of a UnitEL, UnitER, CounitIL or CounitIR, and returns the
// output connection point of the component's lasso loop.
LassoOutConnection :: !Point -> Point
LassoOutConnection (x,y) = (x,y) //*^* (x, y + halfLassoLineLength)

////////// IsBoxInternalOutConnection
// Takes a mouse position and the border rectangle of a Box, and returns True if the
// mouse position is near enough to the box's internal output connection.
IsBoxInternalOutConnection :: !Point !Rectangle -> Bool
IsBoxInternalOutConnection nearPoint ((left, top), (right, _))
  = IsTerminalOutConnection nearPoint ((left + right)/2, top)

////////// BoxInternalOutConnection
// Takes the border rectangle of a box and returns the box's internal output connection point.
BoxInternalOutConnection :: !Rectangle -> Point
BoxInternalOutConnection ((left, top), (right, _))
  = TerminalOutConnection ((left + right)/2, top)

////////// IsBoxInputConnection
// Takes a mouse position and the border rectangle of a Box, and returns True if the
// mouse position is near enough to the box's internal input connection.
IsBoxInputConnection :: !Point !Rectangle -> Bool
IsBoxInputConnection (nearX, nearY) ((left, _), (right, bottom))
  = (~gateRadius <= xDiff) && (xDiff <= gateRadius) && (0 <= yDiff) && (yDiff <= centreLineOffset1 + connectMargin)
where
  xDiff = (left + right)/2 - nearX
  yDiff = bottom - nearY

////////// BoxInputConnection
// Takes the border rectangle of a box and returns the box's internal input connection point.
BoxInputConnection :: !Rectangle -> Point
BoxInputConnection ((left, _), (right, bottom))
  = ((left + right)/2, bottom - centreLineOffset1)

////////// IsBoxExternalOutConnection
// Takes a mouse position and the border rectangle of a Box, and returns True if the
// mouse position is near enough to the box's external output connection.
IsBoxExternalOutConnection :: !Point !Rectangle -> Bool
IsBoxExternalOutConnection (nearX, nearY) ((left, _), (right, bottom))
  = (~gateRadius <= xDiff) && (xDiff <= gateRadius) && (~centreLineOffset1 - connectMargin <= yDiff) && (yDiff <= 0)
where
  xDiff = (left + right)/2 - nearX
  yDiff = bottom - nearY

////////// BoxExternalOutConnection
// Takes the border rectangle of a box and returns the box's external output connection point.
BoxExternalOutConnection :: !Rectangle -> Point
BoxExternalOutConnection ((left, _), (right, bottom))
  = ((left + right)/2, bottom + centreLineOffset1)

////////// ComponentRectangles
// Returns the rectangles of a component's parts.  For a box, these are its
// border lines, the rectangles around its connection areas, and the component
// rectangles (defined the same way) of the components in its box circuit.
// For other components, this returns only the rectangle enclosing the
// component.
ComponentRectangles :: !Component -> [Rectangle]

ComponentRectangles {spec=Box boxCircuit, pos=RCT rectangle}
  = BoxEdgeRectangles rectangle ++ (flatten (map ComponentRectangles boxCircuit))

ComponentRectangles comp = [ComponentRectangle comp]

////////// ComponentRectangle
// Returns the rectangle that completely encloses a component (excluding
// connections).
ComponentRectangle :: !Component -> Rectangle
ComponentRectangle {spec=Box _, pos=(RCT ((leftX, topY), (rightX, bottomY)))}
  = ((leftX, topY), (rightX, bottomY + gateRadius))
ComponentRectangle {spec=Generic _ _ _, pos=(RCT compRect)}
  = compRect
ComponentRectangle {spec=StartTerminal, pos=(PT (x,y))}
  = ((x - terminalRadius, y), (x + terminalRadius, y + 2*terminalRadius))
ComponentRectangle {spec=EndTerminal, pos=(PT (x,y))}
  = ((x - terminalRadius, y - 2*terminalRadius), (x + terminalRadius, y))
ComponentRectangle {spec=UnitEL, pos=(PT (x,y))}
  = ((x - lassoCordRadius - gateRadius, y - lassoCordRadius - 2*gateRadius),
     (x + halfLassoWidth, y + halfLassoHeight))
ComponentRectangle {spec=UnitER, pos=(PT (x,y))}
  = ((x - halfLassoWidth, y - lassoCordRadius - 2*gateRadius),
     (x + lassoCordRadius + gateRadius, y + halfLassoHeight))
ComponentRectangle {spec=CounitIL, pos=(PT (x,y))}
  = ((x - lassoCordRadius - gateRadius, y - halfLassoHeight),
     (x + halfLassoWidth, y + lassoCordRadius + 2*gateRadius))
ComponentRectangle {spec=CounitIR, pos=(PT (x,y))}
  = ((x - halfLassoWidth, y - halfLassoHeight),
     (x + lassoCordRadius + gateRadius, y + lassoCordRadius + 2*gateRadius))
ComponentRectangle {pos=(PT (x,y))}
  = ((x - gateRadius, y - gateRadius), (x + gateRadius, y + gateRadius))

////////// BoxEdgeRectangles
// Takes the border rectangle of a Box and returns the rectangles enclosing
// the sides of its border and the connection areas.
BoxEdgeRectangles :: !Rectangle -> [Rectangle]
BoxEdgeRectangles ((leftX, topY), (rightX, bottomY))
  = [((leftX - 1, topY), (leftX + 1, bottomY)), ((leftX, topY - 1), (rightX, topY + 1)), 
     ((rightX - 1, topY), (rightX + 1, bottomY)), ((leftX, bottomY - 1), (rightX, bottomY + 1)), 
     ((horizCentre - gateRadius, bottomY - gateRadius), (horizCentre + gateRadius, bottomY + gateRadius)),
     ((horizCentre - terminalRadius, topY), (horizCentre + terminalRadius, topY + 2*terminalRadius))
    ]
where
  horizCentre = (leftX + rightX)/2

////////// DrawButtonIcon
// Takes an EditType indicating what component is to be drawn, and returns
// the drawing functions needed to draw that component on a button in the
// toolbox.
DrawButtonIcon :: !EditType -> [DrawFunction]
DrawButtonIcon StartTerminalDraw = DrawStartTerminal (PT buttonCentre)
DrawButtonIcon EndTerminalDraw   = DrawEndTerminal (PT buttonCentre)
DrawButtonIcon TensorEDraw       = DrawTensorE (PT buttonCentre)
DrawButtonIcon TensorIDraw       = DrawTensorI (PT buttonCentre)
DrawButtonIcon SumEDraw          = DrawSumE (PT buttonCentre)
DrawButtonIcon SumIDraw          = DrawSumI (PT buttonCentre)
DrawButtonIcon LollyDraw         = DrawLolly (PT buttonCentre)
DrawButtonIcon UnitIDraw         = DrawUnitI (PT buttonCentre)
DrawButtonIcon UnitELDraw        = DrawUnitEL (PT (fst buttonCentre, (snd buttonCentre) + lassoCordRadius))
DrawButtonIcon UnitERDraw        = DrawUnitER (PT (fst buttonCentre, (snd buttonCentre) + lassoCordRadius))
DrawButtonIcon CounitEDraw       = DrawCounitE (PT buttonCentre)
DrawButtonIcon CounitILDraw      = DrawCounitIL (PT (fst buttonCentre, (snd buttonCentre) - lassoCordRadius))
DrawButtonIcon CounitIRDraw      = DrawCounitIR (PT (fst buttonCentre, (snd buttonCentre) - lassoCordRadius))
DrawButtonIcon BoxDraw           = DrawBox (RCT boxRectangle)
where
  boxRectangle = ((firstX, firstY), (secondX, secondY))
  firstX       = centreX - halfBoxWidth
  firstY       = (centreY - centreLineOffset2) - halfBoxHeight
  secondX      = centreX + halfBoxWidth
  secondY      = (centreY - centreLineOffset2) + halfBoxHeight
  (centreX, centreY) = buttonCentre
  halfBoxWidth  = 12
  halfBoxHeight = 10
DrawButtonIcon GenericDraw       = DrawGeneric "" [Var 0] [Var 0] (RCT ((leftX, topY), (rightX, bottomY)))
where
  leftX              = centreX - terminalWidth
  rightX             = centreX + terminalWidth
  topY               = centreY - currentFont.padding
  bottomY            = centreY + currentFont.padding
  terminalWidth      = 2*terminalRadius
  (centreX, centreY) = buttonCentre
DrawButtonIcon WireDraw          = [DrawLine wireLine]
where
  wireLine           = ((centreX, centreY - yOffset), (centreX, centreY + yOffset))
  (centreX, centreY) = buttonCentre
  yOffset            = 12
DrawButtonIcon Selection         = [DrawRectangle selectRectangle]
where
  ((leftX, topY), (rightX, bottomY)) = buttonDomain
  selectRectangle = ((leftX + 5, topY + 5), (rightX - 5, bottomY - 5))
DrawButtonIcon Rewiring          = [DrawLine centreArrowLine, DrawLine topArrowLine, DrawLine bottomArrowLine
                                    : DrawButtonIcon UnitELDraw]
where
  centreArrowLine = ((arrowLeft, centre),   (arrowRight, centre))
  topArrowLine    = ((arrowMiddle, top),    (arrowRight, centre))
  bottomArrowLine = ((arrowMiddle, bottom), (arrowRight, centre))
  arrowLeft       = (leftX + rightX)/2
  arrowRight      = rightX - 3
  arrowMiddle     = rightX - 7
  centre          = (topY + bottomY)/2
  top             = centre - 4
  bottom          = centre + 4
  ((leftX, topY), (rightX, bottomY)) = buttonDomain

/////////// DrawComponent
// Returns a list of drawing functions necessary to draw the component.
//*^* Note:  might be sensible to move the code from the individual
//*^* drawing functions into this function.
DrawComponent :: !Component -> [DrawFunction]
DrawComponent {spec=TensorE, pos}          = DrawTensorE pos
DrawComponent {spec=TensorI, pos}          = DrawTensorI pos
DrawComponent {spec=SumE, pos}             = DrawSumE pos
DrawComponent {spec=SumI, pos}             = DrawSumI pos
DrawComponent {spec=Lolly, pos}            = DrawLolly pos
DrawComponent {spec=Box boxCircuit, pos}   = DrawBox pos ++ (flatten (map DrawComponent boxCircuit))
DrawComponent {spec=StartTerminal, pos}    = DrawStartTerminal pos
DrawComponent {spec=EndTerminal, pos}      = DrawEndTerminal pos
DrawComponent {spec=UnitI, pos}            = DrawUnitI pos
DrawComponent {spec=UnitEL, pos}           = DrawUnitEL pos
DrawComponent {spec=UnitER, pos}           = DrawUnitER pos
DrawComponent {spec=CounitE, pos}          = DrawCounitE pos
DrawComponent {spec=CounitIL, pos}         = DrawCounitIL pos
DrawComponent {spec=CounitIR, pos}         = DrawCounitIR pos
DrawComponent {spec=Generic name inputTypes outputTypes, pos}
  = DrawGeneric name inputTypes outputTypes pos

////////// DrawComponentWires
// Takes a component and a list of the circuit's wires and returns the list of drawing
// functions necessary to draw the wires connected to the component.
DrawComponentWires :: !Component ![Wire] -> [DrawFunction]
DrawComponentWires comp=:{inputs, outputs} wires
  = flatten (map DrawWire (filter (MemberOf (map GetConnectWireID (inputs ++ outputs)) o GetWireID) wires))

////////// DrawWire
// Takes a wire and returns a list of drawing functions necessary to draw the wire.
DrawWire :: !Wire -> [DrawFunction]
DrawWire wire=:{wireLine, wireType=Free _}  = [DrawLine wireLine]
DrawWire wire=:{wireLine=((x1,y1),(x2,y2))} = [DrawLine crossLine, DrawLine ((x1,y1),(x2,y2))]
where
  crossLine = ((toInt (midX - dx), toInt (midY - dy)), (toInt (midX + dx), toInt (midY + dy)))
  midX      = (toReal (x1+x2))/2.0
  midY      = (toReal (y1+y2))/2.0
  (dx,dy)   = ScaleVector (5.0/(VectorLength norm)) norm
  norm      = toRealVector (uy, ~ux)
  (ux, uy)  = (x2-x1, y2-y1)

////////// DrawStartTerminal
// Returns the list of drawing functions necessary to draw a StartTerminal with its base centred at
// the given point.
DrawStartTerminal :: !Placement -> [DrawFunction]
DrawStartTerminal (PT (x,y)) = [DrawCurve (terminalCircle, 180, 360), DrawLine terminalLine]
where
  terminalCircle = ((x - terminalRadius, y - terminalRadius), (x + terminalRadius, y + terminalRadius))
  terminalLine = ((x, y + terminalRadius), TerminalOutConnection (x,y))

////////// DrawEndTerminal
// Returns the list of drawing functions necessary to draw an EndTerminal with its base centred at
// the given point.
DrawEndTerminal :: !Placement -> [DrawFunction]
DrawEndTerminal (PT (x,y)) = [DrawCurve (terminalCircle, 0, 180), DrawLine terminalLine]
where
  terminalCircle = ((x - terminalRadius, y - terminalRadius), (x + terminalRadius, y + terminalRadius))
  terminalLine = ((x, y - terminalRadius), TerminalInConnection (x,y))

////////// DrawTensorE
// Returns the list of drawing functions necessary to draw a TensorE centred at the given point.
DrawTensorE :: !Placement -> [DrawFunction]
DrawTensorE (PT centre)
  = [DrawCircle (centre, gateRadius),
     DrawLine inCentreLine,
     DrawLine outLeftLine,
     DrawLine outRightLine,
     DrawLine firstCrossLine,
     DrawLine secondCrossLine
    ]
where
  inCentreLine  = (CentreInConnection (centreX, centreY), (centreX, centreY - centreLineOffset2))
  outLeftLine   = (Out1ConnectionPoint (centreX, centreY), (centreX - sideXOffset2, y2`))
  outRightLine  = (Out2ConnectionPoint (centreX, centreY), (centreX + sideXOffset2, y2`))
  y2` = centreY + sideYOffset2

  firstCrossLine  = ((x1, y1), (x2, y2))
  secondCrossLine = ((x1, y2), (x2, y1))
  x1 = centreX - crossLineOffset
  y1 = centreY - crossLineOffset
  x2 = centreX + crossLineOffset
  y2 = centreY + crossLineOffset

  crossLineOffset    = 3
  (centreX, centreY) = centre

////////// DrawTensorI
// Returns the list of drawing functions necessary to draw a TensorI centred at the given point.
DrawTensorI :: !Placement -> [DrawFunction]
DrawTensorI (PT centre)
  = [DrawCircle (centre, gateRadius),
     DrawLine inLeftLine,
     DrawLine inRightLine,
     DrawLine outCentreLine,
     DrawLine firstCrossLine,
     DrawLine secondCrossLine
    ]
where
  outCentreLine = (CentreOutConnection (centreX, centreY), (centreX, centreY + centreLineOffset2))
  inLeftLine    = (In1ConnectionPoint (centreX, centreY), (centreX - sideXOffset2, y2`))
  inRightLine   = (In2ConnectionPoint (centreX, centreY), (centreX + sideXOffset2, y2`))
  y2` = centreY - sideYOffset2

  firstCrossLine  = ((x1, y1), (x2, y2))
  secondCrossLine = ((x1, y2), (x2, y1))
  x1 = centreX - crossLineOffset
  y1 = centreY - crossLineOffset
  x2 = centreX + crossLineOffset
  y2 = centreY + crossLineOffset

  (centreX, centreY) = centre
  crossLineOffset    = 3

////////// DrawSumE
// Returns the list of drawing functions necessary to draw a SumE centred at the given point.
DrawSumE :: !Placement -> [DrawFunction]
DrawSumE (PT centre)
  = [DrawCircle (centre, gateRadius),
     DrawLine inCentreLine,
     DrawLine outLeftLine,
     DrawLine outRightLine,
     DrawLine firstCrossLine,
     DrawLine secondCrossLine
    ]
where
  inCentreLine  = (CentreInConnection (centreX, centreY), (centreX, centreY - centreLineOffset2))
  outLeftLine   = (Out1ConnectionPoint (centreX, centreY), (centreX - sideXOffset2, y2`))
  outRightLine  = (Out2ConnectionPoint (centreX, centreY), (centreX + sideXOffset2, y2`))
  y2` = centreY + sideYOffset2

  firstCrossLine  = ((centreX, centreY - gateRadius + 1), (centreX, centreY + gateRadius - 1))
  secondCrossLine = ((centreX - gateRadius + 1, centreY), (centreX + gateRadius - 1, centreY))
  (centreX, centreY) = centre

////////// DrawSumI
// Returns the list of drawing functions necessary to draw a SumI centred at the given point.
DrawSumI :: !Placement -> [DrawFunction]
DrawSumI (PT centre)
  = [DrawCircle (centre, gateRadius),
     DrawLine inLeftLine,
     DrawLine inRightLine,
     DrawLine outCentreLine,
     DrawLine firstCrossLine,
     DrawLine secondCrossLine
    ]
where
  outCentreLine = (CentreOutConnection (centreX, centreY), (centreX, centreY + centreLineOffset2))
  inLeftLine    = (In1ConnectionPoint (centreX, centreY), (centreX - sideXOffset2, y2`))
  inRightLine   = (In2ConnectionPoint (centreX, centreY), (centreX + sideXOffset2, y2`))
  y2` = centreY - sideYOffset2

  firstCrossLine  = ((centreX, centreY - gateRadius + 1), (centreX, centreY + gateRadius - 1))
  secondCrossLine = ((centreX - gateRadius + 1, centreY), (centreX + gateRadius - 1, centreY))
  (centreX, centreY) = centre

////////// DrawLolly
// Returns the list of drawing functions necessary to draw a Lolly centred at the given point.
DrawLolly :: !Placement -> [DrawFunction]
DrawLolly (PT centre)
  = [DrawCircle (centre, gateRadius),
     DrawLine inLeftLine,
     DrawLine inRightLine,
     DrawLine outCentreLine,
     DrawLine lollyLine,
     DrawOval lollyOval
    ]
where
  outCentreLine = (CentreOutConnection (centreX, centreY), (centreX, centreY + centreLineOffset2))
  inLeftLine    = (In1ConnectionPoint (centreX, centreY), (centreX - sideXOffset2, y2`))
  inRightLine   = (In2ConnectionPoint (centreX, centreY), (centreX + sideXOffset2, y2`))
  y2` = centreY - sideYOffset2

  lollyLine = ((centreX - xOffset, centreY), (lollyJoinX, centreY))
  lollyOval = ((lollyJoinX, centreY - halfOvalHeight), (centreX + xOffset, centreY + halfOvalHeight))
  lollyJoinX     = centreX + 2
  xOffset        = 4
  halfOvalHeight = 1

  (centreX, centreY) = centre

////////// DrawUnitI
// Returns the list of drawing functions necessary to draw a UnitI with its centre at the given point.
DrawUnitI :: !Placement -> [DrawFunction]
DrawUnitI (PT centre)
  = [DrawCircle (centre, gateRadius),
     DrawLine outCentreLine,
     DrawLine stem,
     DrawLine crossbar
    ]
where
  outCentreLine = (CentreOutConnection (centreX, centreY), (centreX, centreY + centreLineOffset2))
  stem          = ((centreX, stemTop), (centreX, centreY + gateRadius - 3))
  crossbar      = ((centreX - gateRadius/2, stemTop), (centreX + gateRadius/2, stemTop))
  stemTop       = centreY - gateRadius/2
  (centreX, centreY) = centre

////////// DrawUnitEL
// Returns the list of drawing functions necessary to draw a UnitEL with its lasso centre at the given point.
DrawUnitEL :: !Placement -> [DrawFunction]
DrawUnitEL (PT (centreX, centreY))
  = [DrawOval lassoOval,
     DrawCurve (lassoCurve, 180, 255),
//*^*     DrawLine lassoLine,
     DrawCircle (unitCentre, gateRadius),
     DrawLine stem,
     DrawLine crossbar,
     DrawLine unitLine
    ]
where
  lassoOval   = ((centreX - halfLassoWidth, centreY - halfLassoHeight), (centreX + halfLassoWidth, centreY + halfLassoHeight))
  lassoCurve  = ((centreX - lassoCordRadius, centreY - 2*lassoCordRadius), (centreX + lassoCordRadius, centreY))
//*^*  lassoLine   = ((centreX, centreY - halfLassoLineLength), (centreX, centreY + halfLassoLineLength))
  unitCentre  = (unitCentreX, unitCentreY)
  stem        = ((unitCentreX, stemTop), (unitCentreX, unitCentreY + gateRadius - 3))
  crossbar    = ((unitCentreX - gateRadius/2, stemTop), (unitCentreX + gateRadius/2, stemTop))
  stemTop     = unitCentreY - gateRadius/2
  unitLine    = (CentreInConnection (unitCentreX, unitCentreY), (unitCentreX, unitCentreY - centreLineOffset2))
  unitCentreX = centreX - lassoCordRadius
  unitCentreY = centreY - lassoCordRadius - gateRadius

////////// DrawUnitER
// Returns the list of drawing functions necessary to draw a UnitER with its lasso centre at the given point.
DrawUnitER :: !Placement -> [DrawFunction]
DrawUnitER (PT (centreX, centreY))
  = [DrawOval lassoOval,
     DrawCurve (lassoCurve, 285, 360),
//*^*     DrawLine lassoLine,
     DrawCircle (unitCentre, gateRadius),
     DrawLine stem,
     DrawLine crossbar,
     DrawLine unitLine
    ]
where
  lassoOval   = ((centreX - halfLassoWidth, centreY - halfLassoHeight), (centreX + halfLassoWidth, centreY + halfLassoHeight))
  lassoCurve  = ((centreX - lassoCordRadius, centreY - 2*lassoCordRadius), (centreX + lassoCordRadius, centreY))
//*^*  lassoLine   = ((centreX, centreY - halfLassoLineLength), (centreX, centreY + halfLassoLineLength))
  unitCentre  = (unitCentreX, unitCentreY)
  stem        = ((unitCentreX, stemTop), (unitCentreX, unitCentreY + gateRadius - 3))
  crossbar    = ((unitCentreX - gateRadius/2, stemTop), (unitCentreX + gateRadius/2, stemTop))
  stemTop     = unitCentreY - gateRadius/2
  unitLine    = (CentreInConnection (unitCentreX, unitCentreY), (unitCentreX, unitCentreY - centreLineOffset2))
  unitCentreX = centreX + lassoCordRadius
  unitCentreY = centreY - lassoCordRadius - gateRadius

////////// DrawCounitE
// Returns the list of drawing functions necessary to draw a CounitE with its centre at the given point.
DrawCounitE :: !Placement -> [DrawFunction]
DrawCounitE (PT centre)
  = [DrawCircle (centre, gateRadius),
     DrawLine inCentreLine,
     DrawLine stem,
     DrawLine crossbar
    ]
where
  inCentreLine  = (CentreInConnection (centreX, centreY), (centreX, centreY - centreLineOffset2))
  stem          = ((centreX, stemTop), (centreX, stemBottom))
  crossbar      = ((centreX - gateRadius/2, stemBottom), (centreX + gateRadius/2, stemBottom))
  stemTop       = centreY - gateRadius + 2
  stemBottom    = centreY + gateRadius/2 - 1
  (centreX, centreY) = centre

////////// DrawCounitIL
// Returns the list of drawing functions necessary to draw a CounitIL with its lasso centre at the given point.
DrawCounitIL :: !Placement -> [DrawFunction]
DrawCounitIL (PT (centreX, centreY))
  = [DrawOval lassoOval,
     DrawCurve (lassoCurve, 105, 180),
     DrawCircle (unitCentre, gateRadius),
     DrawLine stem,
     DrawLine crossbar,
     DrawLine unitLine
    ]
where
  lassoOval   = ((centreX - halfLassoWidth, centreY - halfLassoHeight), (centreX + halfLassoWidth, centreY + halfLassoHeight))
  lassoCurve  = ((centreX - lassoCordRadius, centreY), (centreX + lassoCordRadius, centreY + 2*lassoCordRadius))
  unitCentre  = (unitCentreX, unitCentreY)
  stem        = ((unitCentreX, stemTop), (unitCentreX, stemBottom))
  crossbar    = ((unitCentreX - gateRadius/2, stemBottom), (unitCentreX + gateRadius/2, stemBottom))
  stemTop     = unitCentreY - gateRadius + 2
  stemBottom  = unitCentreY + gateRadius/2 - 1
  unitLine    = (CentreOutConnection (unitCentreX, unitCentreY), (unitCentreX, unitCentreY + centreLineOffset2))
  unitCentreX = centreX - lassoCordRadius
  unitCentreY = centreY + lassoCordRadius + gateRadius

////////// DrawCounitIR
// Returns the list of drawing functions necessary to draw a CounitIR with its lasso centre at the given point.
DrawCounitIR :: !Placement -> [DrawFunction]
DrawCounitIR (PT (centreX, centreY))
  = [DrawOval lassoOval,
     DrawCurve (lassoCurve, 0, 75),
     DrawCircle (unitCentre, gateRadius),
     DrawLine stem,
     DrawLine crossbar,
     DrawLine unitLine
    ]
where
  lassoOval   = ((centreX - halfLassoWidth, centreY - halfLassoHeight), (centreX + halfLassoWidth, centreY + halfLassoHeight))
  lassoCurve  = ((centreX - lassoCordRadius, centreY), (centreX + lassoCordRadius, centreY + 2*lassoCordRadius))
  unitCentre  = (unitCentreX, unitCentreY)
  stem        = ((unitCentreX, stemTop), (unitCentreX, stemBottom))
  crossbar    = ((unitCentreX - gateRadius/2, stemBottom), (unitCentreX + gateRadius/2, stemBottom))
  stemTop     = unitCentreY - gateRadius + 2
  stemBottom  = unitCentreY + gateRadius/2 - 1
  unitLine    = (CentreOutConnection (unitCentreX, unitCentreY), (unitCentreX, unitCentreY + centreLineOffset2))
  unitCentreX = centreX + lassoCordRadius
  unitCentreY = centreY + lassoCordRadius + gateRadius

////////// DrawBox
// Returns the list of drawing functions necessary to draw a Box with the given rectangle as a border.
DrawBox :: !Placement -> [DrawFunction]
DrawBox (RCT ((firstX, firstY), (secondX, secondY)))
  = [DrawLine (startPoint, (firstX, secondY)),
     DrawLine ((firstX, secondY), (firstX, firstY)),
     DrawLine ((firstX, firstY), (secondX, firstY)),
     DrawLine ((secondX, firstY), (secondX, secondY)),
     DrawLine ((secondX, secondY), endPoint),
     DrawCircle (lollyCentre, gateRadius),
     DrawLine inCentreLine,
     DrawLine outCentreLine,
     DrawLine lollyLine,
     DrawOval lollyOval
     : DrawStartTerminal (PT ((firstX + secondX)/2, firstY))
    ]
where
  startPoint         = (lollyCentreX - gateRadius, lollyCentreY)
  endPoint           = (lollyCentreX + gateRadius, lollyCentreY)
  inCentreLine       = (CentreInConnection (lollyCentreX, lollyCentreY), (lollyCentreX, lollyCentreY - centreLineOffset2))
  outCentreLine      = (CentreOutConnection (lollyCentreX, lollyCentreY), (lollyCentreX, lollyCentreY + centreLineOffset2))

  lollyLine          = ((lollyCentreX - xOffset, lollyCentreY), (lollyJoinX, lollyCentreY))
  lollyOval          = ((lollyJoinX, lollyCentreY - halfOvalHeight), (lollyCentreX + xOffset, lollyCentreY + halfOvalHeight))
  lollyJoinX         = lollyCentreX + 2
  xOffset            = 4
  halfOvalHeight     = 1

  lollyCentre        = (lollyCentreX, lollyCentreY)
  lollyCentreX       = (firstX + secondX) / 2
  lollyCentreY       = secondY

///////// DrawGeneric
// Returns the list of drawing functions necessary to draw a Generic component with the given name, inputs,
// outputs and border rectangle.
//*^* NOTE:  This would be more efficient if generic components had lists giving the offset of each of
//*^*        their inputs and outputs from the left edge - the floating-point calculations done by
//*^*        InConnectionPoint and OutConnectionPoint would not have to be done repeatedly when dragging
//*^*        the component.
DrawGeneric :: String [Type] [Type] !Placement -> [DrawFunction]
DrawGeneric name inputTypes outputTypes (RCT ((leftX, topY), (rightX, bottomY)))
  = [DrawRectangle ((leftX, topY), (rightX, bottomY)),
     MovePenTo nameStart,
     DrawString name :
     (map DrawLine (inConnectLines ++ outConnectLines))
    ]
where
  nameStart = ((leftX + rightX - nameWidth)/2, topY + currentFont.padding + currentFont.ascent)
  nameWidth = FontStringWidth name currentFont.font
  inConnectLines
    = [((x,y), (x, y + connectLength))
       \\ (x,y) <- map (InConnectionPoint (Generic "" inputTypes []) (RCT ((leftX, topY), (rightX, bottomY))))
                       [0..((length inputTypes)-1)]
      ]
  outConnectLines
    = [((x,y), (x, y - connectLength))
       \\ (x,y) <- map (OutConnectionPoint (Generic "" [] outputTypes) (RCT ((leftX, topY), (rightX, bottomY))))
                       [0..((length outputTypes)-1)]
      ]

////////// TooSmallBox
// Returns True if the rectangle is "too small" to be the border of a Box.
TooSmallBox :: !Rectangle -> Bool
TooSmallBox ((leftX, topY), (rightX, bottomY))
  = (horizontalDist < 2*(gateRadius + boxMargin)) ||
    (verticalDist < (3*gateRadius + 3*centreLineOffset1 + 2*terminalRadius + boxMargin))
where
  horizontalDist = rightX - leftX
  verticalDist   = bottomY - topY
  boxMargin      = 4

////////// DrawSelection
// Given a component, a wire ID and a list of wires, returns the 
// list of drawing functions necessary to draw a "selection
// indicator" (which shows that the component or wire is currently
// selected) around the component (if the wire ID is -1) or wire.
DrawSelection :: !Component !WireID [Wire] -> [DrawFunction]

DrawSelection comp=:{spec=Box boxCircuit} (-1) _
  = [DrawRectangle ((leftX - 1, topY - 1), (rightX + 2, bottomY + 2)),
     FillRectangle (TopLeftRectangle compRect),
     FillRectangle (BottomLeftRectangle compRect),
     FillRectangle (TopRightRectangle compRect),
     FillRectangle (BottomRightRectangle compRect)
    ]
where
  compRect = ComponentRectangle comp
  ((leftX, topY), (rightX, bottomY)) = compRect

DrawSelection comp (-1) _
  = [DrawRectangle ((leftX - 1, topY - 1), (rightX + 2, bottomY + 2))]
where
  ((leftX, topY), (rightX, bottomY)) = ComponentRectangle comp

DrawSelection _ n wires
  = [DrawLine firstLine, DrawLine secondLine]
where
  selectedWire = GetFirst (((==) n) o GetWireID) wires
  ((x1, y1), (x2, y2)) = selectedWire.wireLine
  (norm1X, norm1Y) = toRealVector ((y2 - y1), (x1 - x2))
  norm1Length = VectorLength (norm1X, norm1Y)
  (norm2X, norm2Y) = (toInt (norm1X / norm1Length), toInt (norm1Y / norm1Length))
  firstLine = ((x1 + norm2X, y1 + norm2Y), (x2 + norm2X, y2 + norm2Y))
  secondLine = ((x1 - norm2X, y1 - norm2Y), (x2 - norm2X, y2 - norm2Y))

////////// TopLeftRectangle
// Returns the rectangle at the top left of a box's border that can 
// be selected for sizing the box from this corner.
TopLeftRectangle :: !Rectangle -> !Rectangle
TopLeftRectangle ((leftX, topY), _) = ((leftX - 6, topY - 6), (leftX + 4, topY + 4))

////////// TopRightRectangle
// Returns the rectangle at the top right of a box's border that can 
// be selected for sizing the box from this corner.
TopRightRectangle :: !Rectangle -> !Rectangle
TopRightRectangle ((_, topY), (rightX, _)) = ((rightX - 3, topY - 6), (rightX + 7, topY + 4))

////////// BottomRightRectangle
// Returns the rectangle at the bottom left of a box's border that can 
// be selected for sizing the box from this corner.
BottomRightRectangle :: !Rectangle -> !Rectangle
BottomRightRectangle (_, (rightX, bottomY)) = ((rightX - 3, bottomY - 3), (rightX + 7, bottomY + 7))

////////// BottomLeftRectangle
// Returns the rectangle at the bottom left of a box's border that can 
// be selected for sizing the box from this corner.
BottomLeftRectangle :: !Rectangle -> !Rectangle
BottomLeftRectangle ((leftX, _), (_, bottomY)) = ((leftX - 6, bottomY - 3), (leftX + 4, bottomY + 7))

/////////

// currentFont stores information about the default font - padding is the border that
// should be put around the name of a generic component (leading is the distance between
// lines of the font), ascent is the maximum letter height from the baseline, and descent
// is the maximum distance from the baseline to the bottom of a letter.  Total line
// height for a font is ascent+descent+leading.
:: FontRec = {font :: Font, padding :: Int, ascent :: Int, descent :: Int}
currentFont :== {font = defaultFont, padding = max leading 5, ascent=ascent, descent=descent}
where
  defaultFont                      = snd (SelectFont fontName fontStyles fontSize)
  (fontName, fontStyles, fontSize) = DefaultFont
  (ascent, descent, _, leading)    = FontMetrics defaultFont

// This gives the size of the buttons in the toolbox.
buttonDomain :== ((0,0),(40,40))
// This gives the centre of each button's rectangle (for convenience).
buttonCentre  :== (20,20)

// The radius of the circle in the TensorI, TensorE, SumI, SumE, Lolly, UnitI and CounitE.
gateRadius       :== 6
// The radius of the half-circle base of the StartTerminal and EndTerminal.
terminalRadius   :== 4
//*^*gateRadius       :== 9
//*^*terminalRadius   :== 6

// The length of a connection line on a generic component.
connectLength :== 6

// Half the width of the lasso loop in the UnitEL, UnitER, CounitIL and CounitIR.
halfLassoWidth      :== 5
// Half the height of the lasso loop in the UnitEL, UnitER, CounitIL and CounitIR.
halfLassoHeight     :== 3
// Gives the vertical distance from the lasso loop centre to the outside edge of
// the input and output connection regions for the lasso loop in the UnitEL, 
// UnitER, CounitIL and CounitIR.
halfLassoLineLength :== 5
// Gives the radius of the lasso "cord" (the arc connecting the unit or counit 
// with the lasso loop) in the UnitEL, UnitER, CounitIL and CounitIR.
lassoCordRadius     :== 12
//*^*lassoCordRadius     :== 18

// Gives the distance from the centre of a TensorI, TensorE, SumI, SumE, Lolly, UnitI or
// CounitE to the outer endpoint of its centre input or output connection.
centreLineOffset1 :== 12
//*^*centreLineOffset1 :== 18
// Gives the vertical distance from the centre of a TensorI, TensorE, SumI, SumE, Lolly,
// UnitI or CounitE to the inner endpoint of its centre input or output connection.
centreLineOffset2 :== 6
//*^*centreLineOffset2 :== 9
// Gives the horizontal distance from the centre of a TensorI, TensorE, SumI, SumE, Lolly,
// UnitI or CounitE to the outer endpoint of its left input or output connection.
sideXOffset1  :== 10
//*^*sideXOffset1  :== 15
// Gives the horizontal distance from the centre of a TensorI, TensorE, SumI, SumE, Lolly,
// UnitI or CounitE to the inner endpoint of its left input or output connection.
sideXOffset2  :== 3
//*^*sideXOffset2  :== 5
// Gives the vertical distance from the centre of a TensorI, TensorE, SumI, SumE, Lolly,
// UnitI or CounitE to the outer endpoint of its left input or output connection.
sideYOffset1  :== 12
//*^*sideYOffset1  :== 18
// Gives the vertical distance from the centre of a TensorI, TensorE, SumI, SumE, Lolly,
// UnitI or CounitE to the inner endpoint of its left input or output connection.
sideYOffset2  :== 5
//*^*sideYOffset2  :== 7

// Gives a margin for error in selecting one of the connection regions of a component.
connectMargin :== 3

/////////////////////////////////////////////////

////////// GetType
// Given a wire type, returns just the type.
GetType :: !WireType -> Type
GetType (User type) = type
GetType (Free type) = type

////////// NewConnection
// Creates a new connection with the wire ID set to -1 (to indicate that it is
// not connected to a wire) and the wire type free (rather than user-specified),
// with the given type.
NewConnection :: Type -> Connection
NewConnection type = {cWireID=(-1), cWireType=(Free type)}

////////// SubsIntoCircuit
// Takes a list of variable substitutions and a circuit, and makes the variable
// substitutions in all free wire types of the circuit.  (Assumes that the 
// variables being substituted in are independent of the variables being replaced.)
SubsIntoCircuit :: [Substitution] !Circuit -> Circuit
SubsIntoCircuit subs [comp : circuit] = [SubsIntoComponent subs comp : SubsIntoCircuit subs circuit]
SubsIntoCircuit _ [] = []

////////// SubsIntoComponent
// Takes a list of variable substitutions and a component, and makes the variable
// substitutions in all free wire types of the component.  (Assumes that the 
// variables being substituted in are independent of the variables being replaced.)
SubsIntoComponent :: [Substitution] !Component -> Component
SubsIntoComponent subs comp=:{spec=Box boxCircuit, inputs, outputs}
  = {comp & spec=Box (SubsIntoCircuit subs boxCircuit),
            inputs=(map (SubsIntoConnect subs) inputs),
            outputs=(map (SubsIntoConnect subs) outputs)}
SubsIntoComponent subs comp=:{inputs, outputs}
  = {comp & inputs=(map (SubsIntoConnect subs) inputs), outputs=(map (SubsIntoConnect subs) outputs)}

////////// SubsIntoConnect
// Takes a list of variable substitutions and a connection, and makes the variable
// substitutions in all free wire types of the connection.  (Assumes that the 
// variables being substituted in are independent of the variables being replaced.)
SubsIntoConnect :: [Substitution] !Connection -> Connection
SubsIntoConnect subs connect=:{cWireType}
  = {connect & cWireType = SubsIntoWireType subs cWireType}

///////// SubsIntoWires
// Takes a list of variable substitutions and a list of wires, and makes the
// variable substitutions in all free wire types of the wires.  (Assumes that
// the  variables being substituted in are independent of the variables being
// replaced.)
SubsIntoWires :: [Substitution] ![Wire] -> [Wire]
SubsIntoWires subs [wire=:{wireType} : wires]
  = [{wire & wireType = SubsIntoWireType subs wireType} : SubsIntoWires subs wires]
SubsIntoWires subs [] = []

////////// SubsIntoWireType
// Takes a list of variable substitutions and a wire type, and makes the variable
// substitutions in the wire type if it is free, but not if it is user-specified.
// (Assumes that the  variables being substituted in are independent of the
// variables being replaced.)
SubsIntoWireType :: [Substitution] !WireType -> WireType
SubsIntoWireType _ (User type)
  = User type
SubsIntoWireType subs (Free type)
  = Free (SubsIntoType subs type)

///////// filterInOut
// Takes a predicate and a list, and returns two lists, with everything that
// satisfies the predicate in the first list and everything else in the other.
filterInOut :: (a -> Bool) ![a] -> ([a],[a])
filterInOut pred [] = ([], [])
filterInOut pred [a : as]
  | pred a    = ([a : as1], as2)
  | otherwise = (as1, [a : as2])
where
  (as1, as2) = filterInOut pred as

///////// MemberOf
// Returns True if the second item is a member of the first list,
// False otherwise (flips the arguments of isMember).
MemberOf :: ![a] a -> Bool | == a
MemberOf list x = isMember x list

///////// Replace
// Finds the item in the list indexed by the integer (starting from 0)
// and replaces it with the third argument.
Replace :: !Int ![a] a -> [a]
Replace 0 [x : xs] y = [y : xs]
Replace n [x : xs] y = [x : Replace (n-1) xs y]

//////// RemoveAndReturn
// Finds the first item in the list that satisfies the predicate
// and returns it with the remainder of the list, still in order.
RemoveAndReturn :: !(a -> Bool) ![a] -> (a, [a])
RemoveAndReturn pred [x : xs]
  | pred x    = (x, xs)
  | otherwise = (removedItem, [x : newXs])
where
  (removedItem, newXs) = RemoveAndReturn pred xs

//////// GetFirst
// Finds the first item in the list that satisfies the predicate
// and returns it.
GetFirst :: !(a -> Bool) ![a] -> a
GetFirst pred [x : xs]
  | pred x    = x
  | otherwise = GetFirst pred xs

//////// ReverseAppend
// Reverses and appends the first list to the second.
ReverseAppend :: ![a] [a] -> [a]
ReverseAppend [x : xs] ys = ReverseAppend xs [x : ys]
ReverseAppend [] ys = ys

//////// RemoveIntersection
// Given two lists (each with no duplicate items), 
// returns the two lists in their original orders,
// with all items that they have in common removed.
RemoveIntersection :: [a] [a] -> ([a],[a]) | == a
RemoveIntersection [x : xs] ys
  | isEmpty intersects = ([x : xs2], ys3)
  | otherwise          = (xs2, ys3)
where
  (intersects, ys2) = filterInOut ((==) x) ys
  (xs2, ys3)        = RemoveIntersection xs ys2

RemoveIntersection [] ys = ([], ys)

//////// DotProduct
DotProduct :: !Vector !Vector -> Int
DotProduct (x1,y1) (x2,y2) = x1*x2 + y1*y2

:: RealVector :== (Real, Real)

//////// VectorLength
// Returns the length of the given vector.
VectorLength :: !RealVector -> Real
VectorLength (x,y) = sqrt (x ^ 2.0 + y ^ 2.0)

//////// ScaleVector
ScaleVector :: Real !RealVector -> RealVector
ScaleVector r (x,y) = (r*x, r*y)

//////// toRealVector
// Converts a vector of integers to a vector of reals.
toRealVector :: !Vector -> RealVector
toRealVector (x,y) = (toReal x, toReal y)

//////// VectorDifference
VectorDifference :: !Vector !Vector -> Vector
VectorDifference (x1,y1) (x2,y2)
  = (x1-x2, y1-y2)

////////// IsBox
IsBox :: !Component -> Bool
IsBox {spec=Box _} = True
IsBox _            = False

////////// BoxCircuit
BoxCircuit :: !Component -> Circuit
BoxCircuit {spec=Box boxCircuit} = boxCircuit

////////// IsLolly
IsLolly :: !Component -> Bool
IsLolly {spec=Lolly} = True
IsLolly _            = False

////////// IsStartTerminal
IsStartTerminal :: !Component -> Bool
IsStartTerminal comp=:{spec=StartTerminal} = True
IsStartTerminal _ = False

////////// IsEndTerminal
IsEndTerminal :: !Component -> Bool
IsEndTerminal comp=:{spec=EndTerminal} = True
IsEndTerminal _ = False

////////// IsThinningLink
IsThinningLink :: !Component -> Bool
IsThinningLink {spec=UnitEL}   = True
IsThinningLink {spec=UnitER}   = True
IsThinningLink {spec=CounitIL} = True
IsThinningLink {spec=CounitIR} = True
IsThinningLink _               = False

///////// GetLassoWireIDs
// Returns the wire ID of the wire that connects down into the lasso loop, the wire ID of the wire
// that connects down from the lasso loop, and the wire ID of the wire into the unit or counit (the
// lasso wire) of a thinning link.
GetLassoWireIDs :: !Component -> (WireID, WireID, WireID)
GetLassoWireIDs comp=:{spec=UnitEL, inputs=[{cWireID=lassoID}, {cWireID=inID}], outputs=[{cWireID=outID}]}
  = (inID, outID, lassoID)
GetLassoWireIDs comp=:{spec=UnitER, inputs=[{cWireID=inID}, {cWireID=lassoID}], outputs=[{cWireID=outID}]}
  = (inID, outID, lassoID)
GetLassoWireIDs comp=:{spec=CounitIL, inputs=[{cWireID=inID}], outputs=[{cWireID=lassoID}, {cWireID=outID}]}
  = (inID, outID, lassoID)
GetLassoWireIDs comp=:{spec=CounitIR, inputs=[{cWireID=inID}], outputs=[{cWireID=outID}, {cWireID=lassoID}]}
  = (inID, outID, lassoID)

////////// IsDummy
// Returns True if the component is a dummy component, False otherwise.
IsDummy :: !Component -> Bool
IsDummy comp=:{id=(-1)} = True
IsDummy _ = False

dummyComponent :== {spec=EndTerminal, id=(-1), pos=PT (-1,-1), inputs=[], outputs=[]}

dummyWireType :== Free Unit

////////// GetInputPositionPairs
// Returns a list pairing the horizontal position of each of the
// circuit's external input connections with the connection,
// sorted by horizontal position and connection wire ID.
GetInputPositionPairs :: [Wire] ![Connection] -> [(Int, Connection)]
GetInputPositionPairs wires inputs = sort (map (InputPair wires) inputs)

////////// InputPair
// Given a list of wires and a connection, returns a pair of the
// connections upper horizontal position with the connection.
InputPair :: [Wire] !Connection -> (Int, Connection)
InputPair wires connect=:{cWireID} = (fst (fst connectWire.wireLine), connect)
where
  connectWire = GetFirst (((==) cWireID) o GetWireID) wires

////////// GetOutputPositionPairs
// Returns a list pairing the horizontal position of each of the
// circuit's external output connections with the connection,
// sorted by horizontal position and connection wire ID.
GetOutputPositionPairs :: [Wire] ![Connection] -> [(Int, Connection)]
GetOutputPositionPairs wires outputs = sort (map (OutputPair wires) outputs)

////////// OutputPair
// Given a list of wires and a connection, returns a pair of the
// connections lower horizontal position with the connection.
OutputPair :: [Wire] !Connection -> (Int, Connection)
OutputPair wires connect=:{cWireID} = (fst (snd connectWire.wireLine), connect)
where
  connectWire = GetFirst (((==) cWireID) o GetWireID) wires

////////// ChangePositionPair
// Given a StartTerminal or EndTerminal that has been moved, a list of input
// position pairs and a list of output position pairs, returns a new list
// of input position pairs and a new list of output position pairs in which
// the horizontal position for the terminal's connection has been changed
// to match the terminal's new position.
ChangePositionPair :: !Component [(Int, Connection)] [(Int, Connection)] -> ([(Int, Connection)], [(Int, Connection)])
ChangePositionPair {spec=StartTerminal, outputs=[outC], pos=PT (x,_)} inputPairs outputPairs
  = (ChangePositionPairAux outC.cWireID (x, outC) inputPairs, outputPairs)
ChangePositionPair {spec=EndTerminal, inputs=[inC], pos=PT (x,_)} inputPairs outputPairs
  = (inputPairs, ChangePositionPairAux inC.cWireID (x, inC) outputPairs)

//
ChangePositionPairAux :: !WireID (Int, Connection) ![(Int, Connection)] -> [(Int, Connection)]
ChangePositionPairAux wireID newPair [pair=:(_, {cWireID}) : positionPairs]
  | wireID==cWireID = [newPair : positionPairs]
  | otherwise       = [pair : ChangePositionPairAux wireID newPair positionPairs]

////////// ChangePositionPairID
// Given an old wire ID, a new wire ID, and a list of position pairs, returns a
// new list of position pairs in which the wire ID of the connection identified
// by the old wire ID has been changed to the new wire ID, or returns the list
// unchanged if no such connection is found.
ChangePositionPairID :: !WireID WireID ![(Int, Connection)] -> [(Int, Connection)]
ChangePositionPairID oldWireID newWireID [(x, connect=:{cWireID}) : positionPairs]
  | cWireID==oldWireID = [(x, {connect & cWireID=newWireID}) : positionPairs]
  | otherwise          = [(x, connect) : ChangePositionPairID oldWireID newWireID positionPairs]
ChangePositionPairID _ _ [] = []

////////// GetBounds
// Given a wire ID identifying an external circuit wire, a list of position pairs,
// and a maximum horizontal position, returns the smallest horizontal position (no
// smaller than 0) and the largest horizontal position (no greater than the maximum
// given) to which the external connection point of the wire may be moved, as
// determined by the horizontal positions of the external connections on either side
// of it (if any).
GetBounds :: !WireID ![(Int, Connection)] Int -> (Int, Int)
GetBounds wireID
          [pair1=:(x1, {cWireID=cWireID1}), pair2=:(x2, {cWireID=cWireID2}), pair3=:(x3,_) : positionPairs]
          maxX
  | wireID==cWireID2
      = if (x1==x2)
           (if (x2==x3)
               (x1, x3)
               (x1, x3-1)
           )
           (if (x2==x3)
               (x1+1, x3)
               (x1+1, x3-1)
           )
  | wireID==cWireID1
      = if (x1==x2)
           (0, x2)
           (0, x2-1)
  | otherwise = GetBounds wireID [pair2, pair3 : positionPairs] maxX

GetBounds wireID [pair1=:(x1, {cWireID=cWireID1}), pair2=:(x2, {cWireID=cWireID2})] maxX
  | wireID==cWireID2
      = if (x1==x2)
           (x1, maxX)
           (x1+1, maxX)
  | wireID==cWireID1    //*^* Should always hold
      = if (x1==x2)
           (0, x2)
           (0, x2-1)

GetBounds wireID [pair=:(x, {cWireID})] maxX
  | wireID==cWireID     //*^* Should always hold
      = (0, maxX)

////////// TerminalXPosition
// Given a StartTerminal or EndTerminal, returns the horizontal position of its
// connection.
TerminalXPosition :: !Component -> Int
TerminalXPosition {pos=PT (x,_)} = x

/////////// IsUserType
IsUserType :: !WireType -> Bool
IsUserType (User _) = True
IsUserType _        = False

/////////// MakeUserType
MakeUserType :: !WireType -> WireType
MakeUserType (Free type) = User type
MakeUserType wireType    = wireType

/////////// MakeFree
MakeFree :: !WireType -> WireType
MakeFree (User type) = Free type
MakeFree wireType    = wireType

/////////// ChangeWireType
// Takes a wire ID identifying a wire, the wire's position line in the circuit,
// a new wire type for the wire, and a circuit, and changes the wire's wire
// type on its two connection points in the circuit.
ChangeWireType :: WireID !Line WireType !Circuit -> Circuit
ChangeWireType wireID (start, end) newType circuit
  = ChangeTypeOnOutput wireID start newType (ChangeTypeOnInput wireID end newType circuit)

///////// ChangeTypeOnInput
// Takes a wire ID identifying a wire, the wire's end connection point, its new wire
// type and a circuit, and changes the wire's type on its input connection.
ChangeTypeOnInput :: WireID Point WireType !Circuit -> Circuit
ChangeTypeOnInput wireID endPoint newType [box=:{spec=Box boxCircuit, inputs=[inC], pos=RCT boxRect} : circuit]
  | inC.cWireID==wireID            = [{box & inputs=[{inC & cWireType=newType}]} : circuit]
  | IsInRectangle endPoint boxRect = [{box & spec=Box (ChangeTypeOnInput wireID endPoint newType boxCircuit)} : circuit]
  | otherwise                      = [box : ChangeTypeOnInput wireID endPoint newType circuit]
ChangeTypeOnInput wireID endPoint newType [comp=:{inputs} : circuit]
  | isEmpty backInputs = [comp : ChangeTypeOnInput wireID endPoint newType circuit]
  | otherwise          = [{comp & inputs=frontInputs ++ [{(hd backInputs) & cWireType=newType} : tl backInputs]} : circuit]
where
  (frontInputs, backInputs) = span (((<>) wireID) o GetConnectWireID) inputs

///////// ChangeTypeOnOutput
// Takes a wire ID identifying a wire, the wire's start connection point, its new wire
// type and a circuit, and changes the wire's type on its output connection.
ChangeTypeOnOutput :: WireID Point WireType !Circuit -> Circuit
ChangeTypeOnOutput wireID endPoint newType [box=:{spec=Box boxCircuit, outputs=[outC1, outC2], pos=RCT boxRect} : circuit]
  | outC1.cWireID==wireID          = [{box & outputs=[{outC1 & cWireType=newType}, outC2]} : circuit]
  | outC2.cWireID==wireID          = [{box & outputs=[outC1, {outC2 & cWireType=newType}]} : circuit]
  | IsInRectangle endPoint boxRect = [{box & spec=Box (ChangeTypeOnOutput wireID endPoint newType boxCircuit)} : circuit]
  | otherwise                      = [box : ChangeTypeOnOutput wireID endPoint newType circuit]
ChangeTypeOnOutput wireID endPoint newType [comp=:{outputs} : circuit]
  | isEmpty backOutputs = [comp : ChangeTypeOnOutput wireID endPoint newType circuit]
  | otherwise           = [{comp & outputs=frontOutputs ++ [{(hd backOutputs) & cWireType=newType} : tl backOutputs]}
                           : circuit]
where
  (frontOutputs, backOutputs) = span (((<>) wireID) o GetConnectWireID) outputs

///////// FindMaxVarInTypes
// Finds the maximum of all the variable numbers used in the list of types
// and the given variable number.
FindMaxVarInTypes :: ![Type] Int -> Int
FindMaxVarInTypes [type : types] maxVar
  = seq [FindMaxVarInType type, FindMaxVarInTypes types] maxVar
FindMaxVarInTypes [] maxVar = maxVar

///////// FindMaxVarInType
// Finds the maximum of all the variable numbers used in the type
// and the given variable number.
FindMaxVarInType :: !Type Int -> Int

FindMaxVarInType (Product (type1, type2)) maxVar
  = seq [FindMaxVarInType type1, FindMaxVarInType type2] maxVar

FindMaxVarInType (Sum (type1, type2)) maxVar
  = seq [FindMaxVarInType type1, FindMaxVarInType type2] maxVar

FindMaxVarInType (Then (type1, type2)) maxVar
  = seq [FindMaxVarInType type1, FindMaxVarInType type2] maxVar

FindMaxVarInType (UserFunc _ types) maxVar
  = seq (map FindMaxVarInType types) maxVar

FindMaxVarInType (Var x) maxVar = max x maxVar

FindMaxVarInType (UserVar2 x) maxVar = max x maxVar

FindMaxVarInType _ maxVar = maxVar

///////// GetCircuitWires
// Takes a circuit and returns a list of the circuit's wires,
// by proceeding down the circuit, storing the wire ID and
// connection point of every output encountered and then
// using this to create wires when the inputs are encountered.
GetCircuitWires :: !Circuit -> [Wire]
GetCircuitWires circuit = fst (GetCircuitWiresAux circuit [] [])
where
  GetCircuitWiresAux :: Circuit [Wire] [(WireID, Point)] -> ([Wire], [(WireID, Point)])
  GetCircuitWiresAux [box=:{spec=Box boxCirc, inputs=[inC], outputs=[outC1, outC2], pos} : circuit] wires1 wireInfo1
    = GetCircuitWiresAux circuit wires3 wireInfo5
  where
    wireInfo2           = MakeWireInfo (Box []) pos 0 [outC1] wireInfo1
    (wires2, wireInfo3) = GetCircuitWiresAux boxCirc wires1 wireInfo2
    (wires3, wireInfo4) = MakeWires (Box []) pos 0 [inC] wires2 wireInfo3
    wireInfo5           = MakeWireInfo (Box []) pos 1 [outC2] wireInfo4

  GetCircuitWiresAux [comp=:{spec, inputs, outputs, pos} : circuit] wires1 wireInfo1
    = GetCircuitWiresAux circuit wires2 wireInfo3
  where
    (wires2, wireInfo2) = MakeWires spec pos 0 inputs wires1 wireInfo1
    wireInfo3           = MakeWireInfo spec pos 0 outputs wireInfo2

  GetCircuitWiresAux [] wires wireInfo = (wires, wireInfo)

///////// MakeWires
// Takes a component's specification and placement, an integer identifying the current connection,
// a list of input connections (the first one is the current connection), a list of wires, and a
// list giving wire IDs and connection points from output connections earlier in the circuit, and
// returns a new list of wires with wires added for each input connection and a new list pairing
// wire IDs and connection points for output connections whose corresponding input connection still
// hasn't been found.
//*^*MakeWires :: CompSpecifics Placement Int ![Connection] [Wire] [(WireID, Point)] -> ([Wire], [(WireID, Point)])
MakeWires :: (CompSpecifics Component) Placement Int ![Connection] [Wire] [(WireID, Point)] -> ([Wire], [(WireID, Point)])

MakeWires spec pos n [{cWireID=(-1)} : connects] wires wireInfo
  = MakeWires spec pos (n+1) connects wires wireInfo

MakeWires spec pos n [connect : connects] wires wireInfo1
  = MakeWires spec pos (n+1) connects [newWire : wires] wireInfo2
where
  (newWire, wireInfo2) = MakeWire spec pos n connect wireInfo1

MakeWires _ _ _ [] wires wireInfo = (wires, wireInfo)

///////// MakeWire
// Takes a component's specification and placement, an integer identifying the current connection,
// an input connection (the current connection), and a list pairing wire IDs and connection points
// for all output connections encountered in the circuit so far, and returns the wire corresponding
// to the input connection and the new list pairing wire IDs and connection points, with the pair
// for the output connection that matched the input connection removed.
//*^*MakeWire :: CompSpecifics Placement Int !Connection [(WireID, Point)] -> (Wire, [(WireID, Point)])
MakeWire :: (CompSpecifics Component) Placement Int !Connection [(WireID, Point)] -> (Wire, [(WireID, Point)])
MakeWire spec pos n {cWireID, cWireType} wireInfo
  = ({wireID=cWireID, wireLine=(snd (hd backWireInfo), InConnectionPoint spec pos n), wireType=cWireType},
     frontWireInfo ++ (tl backWireInfo)
    )
where
  // backWireInfo will start with the matching output connection info.
  (frontWireInfo, backWireInfo) = span (((<>) cWireID) o fst) wireInfo

///////// MakeWireInfo
// Takes a component's specification and placement, an integer identifying the current connection,
// a list of output connections (the first one is the current connection), and a list pairing
// wire IDs with connection points for all output connections found so far that haven't been matched
// with input connections, and returns a new list pairing wire IDs with connection points for output
// connections.
//*^*MakeWireInfo :: CompSpecifics Placement Int ![Connection] [(WireID, Point)] -> [(WireID, Point)]
MakeWireInfo :: (CompSpecifics Component) Placement Int ![Connection] [(WireID, Point)] -> [(WireID, Point)]

MakeWireInfo spec pos n [{cWireID=(-1)} : connects] wireInfo
  = MakeWireInfo spec pos (n+1) connects wireInfo

MakeWireInfo spec pos n [{cWireID} : connects] wireInfo
  = MakeWireInfo spec pos (n+1) connects [(cWireID, OutConnectionPoint spec pos n) : wireInfo]

MakeWireInfo _ _ _ [] wireInfo = wireInfo

///////// GetCircuitNextVals
// Takes a circuit and returns the next free component ID, the next free wire ID
// and the next free variable number for the circuit (1 more than the maximum
// already in use for each of these).
GetCircuitNextVals :: !Circuit -> (ComponentID, WireID, Int)
GetCircuitNextVals circuit = (maxCompID+1, maxWireID+1, maxVar+1)
where
  (maxCompID, maxWireID, maxVar) = GetCircuitMaxVals circuit 0 0 1

///////// GetCircuitMaxVals
// Takes a circuit, a component ID, a wire ID and a variable number and returns 
// a maximum component ID, wire ID and variable number (using the given values
// as the initial maximums).
GetCircuitMaxVals :: !Circuit ComponentID WireID Int -> (ComponentID, WireID, Int)

GetCircuitMaxVals [box=:{spec=Box boxCirc, id, inputs, outputs} : circuit] maxCompID1 maxWireID1 maxVar1
  = GetCircuitMaxVals circuit maxCompID4 maxWireID4 maxVar4
where
  (maxCompID2, maxWireID2, maxVar2) = GetCircuitMaxVals boxCirc maxCompID1 maxWireID1 maxVar1
  (maxCompID3, maxWireID3, maxVar3) = GetCircuitMaxVals circuit maxCompID2 maxWireID2 maxVar2
  maxCompID4 = max maxCompID3 id
  maxWireID4 = maxList [maxWireID3 : ((map GetConnectWireID inputs) ++ (map GetConnectWireID outputs))]
  maxVar4    = seq [FindMaxVarInTypes (map (GetType o GetConnectWireType) inputs),
                    FindMaxVarInTypes (map (GetType o GetConnectWireType) outputs)
                   ] maxVar3

GetCircuitMaxVals [comp=:{id, inputs, outputs} : circuit] maxCompID1 maxWireID1 maxVar1
  = GetCircuitMaxVals circuit maxCompID2 maxWireID2 maxVar2
where
  maxCompID2 = max maxCompID1 id
  maxWireID2 = maxList [maxWireID1 : ((map GetConnectWireID inputs) ++ (map GetConnectWireID outputs))]
  maxVar2    = seq [FindMaxVarInTypes (map (GetType o GetConnectWireType) inputs),
                    FindMaxVarInTypes (map (GetType o GetConnectWireType) outputs)
                   ] maxVar1

GetCircuitMaxVals [] maxCompID maxWireID maxVar = (maxCompID, maxWireID, maxVar)

//////// AddToVarsInTypes
// Takes an integer n, a list of types and an initial maximum variable number,
// and returns the list of types with n added to all the variable numbers and
// the new maximum variable number.
AddToVarsInTypes :: Int ![Type] Int -> ([Type], Int)
AddToVarsInTypes n [type : types] max1 = ([newType : newTypes], newMax)
where
  (newType, max2)    = AddToVarsInType n type max1
  (newTypes, newMax) = AddToVarsInTypes n types max2

AddToVarsInTypes _ [] max1 = ([], max1)

//////// AddToVarsInType
// Takes an integer n, a types and an initial maximum variable number, and
// returns the type with n added to all the variable numbers and the new
// maximum variable number.
AddToVarsInType :: Int !Type Int -> (Type, Int)

AddToVarsInType n (Var m) max1     = (Var newVar, max max1 newVar) where newVar = n+m
AddToVarsInType _ (Const s) max1   = (Const s, max1)
AddToVarsInType _ Unit max1        = (Unit, max1)
AddToVarsInType _ Counit max1      = (Counit, max1)

AddToVarsInType n (Product (type1, type2)) max1 = (Product (newType1, newType2), max3)
where
  (newType1, max2) = AddToVarsInType n type1 max1
  (newType2, max3) = AddToVarsInType n type2 max2

AddToVarsInType n (Sum (type1, type2)) max1 = (Sum (newType1, newType2), max3)
where
  (newType1, max2) = AddToVarsInType n type1 max1
  (newType2, max3) = AddToVarsInType n type2 max2

AddToVarsInType n (Then (type1, type2)) max1 = (Then (newType1, newType2), max3)
where
  (newType1, max2) = AddToVarsInType n type1 max1
  (newType2, max3) = AddToVarsInType n type2 max2

AddToVarsInType n (UserFunc name types) max1 = (UserFunc name newTypes, max2)
where
  (newTypes, max2) = AddToVarsInTypes n types max1

////////// EquivalentSpecs
// Returns True if the two component specifications are equivalent, False
// otherwise.  Any two Boxes are considered to be equivalent (even if they
// have different internal circuits).  Two Generics must have the same
// name, input types and output types to be equivalent.
//*^*EquivalentSpecs :: !CompSpecifics !CompSpecifics -> Bool
EquivalentSpecs :: !(CompSpecifics Component) !(CompSpecifics Component) -> Bool
EquivalentSpecs TensorI  TensorI  = True
EquivalentSpecs TensorE  TensorE  = True
EquivalentSpecs SumI     SumI     = True
EquivalentSpecs SumE     SumE     = True
EquivalentSpecs Lolly    Lolly    = True
EquivalentSpecs UnitI    UnitI    = True
EquivalentSpecs UnitEL   UnitEL   = True
EquivalentSpecs UnitER   UnitER   = True
EquivalentSpecs CounitE  CounitE  = True
EquivalentSpecs CounitIL CounitIL = True
EquivalentSpecs CounitIR CounitIR = True
EquivalentSpecs (Box _)  (Box _)  = True
EquivalentSpecs (Generic name1 inputTypes1 outputTypes1) (Generic name2 inputTypes2 outputTypes2)
  = (name1==name2) && ((length inputTypes1)==(length inputTypes2)) && ((length outputTypes1)==(length outputTypes2))
    && (and inputTypePairsEqual) && (and outputTypePairsEqual)
where
  inputTypePairsEqual  = [EqualTypes type1 type2 \\ type1 <- inputTypes1 & type2 <- inputTypes2]
  outputTypePairsEqual = [EqualTypes type1 type2 \\ type1 <- outputTypes1 & type2 <- outputTypes2]
EquivalentSpecs StartTerminal StartTerminal = True
EquivalentSpecs EndTerminal EndTerminal = True
EquivalentSpecs _        _        = False

