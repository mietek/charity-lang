// Contains a variety of functions used by multiple modules, including functions used to draw components
// and wires and to find component connection points.

definition module utilities

import StdEnv, circuitDefs, unify

 // Allows connections to be sorted by wire ID.
instance < Connection

 // Checks whether any component in the circuit has a connection that hasn't been connected to a wire.
NotFullyWired :: !Circuit -> Bool

 // Returns True if the error value is NoError, False otherwise.
IsNotError :: !ErrorVal -> Bool

 // Gets wire IDs for all a component's external input wires (for most components, just their own input wires,
 // and for boxes, all input wires entering the box).
GetInWireIDs :: !Component -> [WireID]

 // Gets wire IDs for all a component's external output wires (for most components, just their own output wires,
 // and for boxes, all output wires exiting the box plus the wire from the box's external output).
GetOutWireIDs :: !Component -> [WireID]

 // Changes the position of the component by adding the given vector offset to its position
 // coordinates.
MoveComponent :: !Vector !Component -> Component

 // Removes a component (identified by the component ID of the component
 // passed as an argument) from the circuit - in the case of boxes,
 // the box and its box circuit are removed.
RemoveWholeComp :: !Component !Circuit -> Circuit

 // Returns True if the point is in the rectangle (or on its border),
 // False otherwise.
IsInRectangle :: !Point !Rectangle -> Bool

 // Returns True if the first rectangle completely encloses the first, False
 // otherwise.  Their borders should not overlap.
Encloses :: !Rectangle !Rectangle -> Bool

 // Checks whether the component's enclosing rectangle overlaps any component in
 // the circuit.  The component may be positioned inside a box in the circuit,
 // but must not overlap its borders or any component in its box circuit.
OverlapCheck :: Component !Circuit -> Bool

 // Returns True if the two rectangles overlap each other (including borders), False otherwise.
Overlaps :: !Rectangle !Rectangle -> Bool

 // Returns the centre point of a rectangle.
RectangleCentre :: !Rectangle -> Point

 // Returns the wire ID of a connection.
GetConnectWireID :: !Connection -> WireID
 // Returns the wire type of a connection.
GetConnectWireType :: !Connection -> WireType
 // Returns the wire ID of a wire.
GetWireID :: !Wire -> WireID
 // Returns the wire type of a wire.
GetWireType :: !Wire -> WireType


 // Given a component specification, the component's placement, and an
 // integer specifying the desired connection, returns the output connection
 // point.  For boxes, the internal output connection is 0 and the external
 // output connection is 1.  For other components, output connections are
 // numbered from left to right, starting at 0.
//*^*OutConnectionPoint :: !CompSpecifics !Placement !Int -> Point
OutConnectionPoint :: !(CompSpecifics Component) !Placement !Int -> Point

 // Given a component specification, the component's placement, and an
 // integer specifying the desired connection, returns the input connection
 // point.  Input connections are numbered from left to right, starting
 // at 0.  (A box's internal input connection is numbered 0.)
//*^*InConnectionPoint :: !CompSpecifics !Placement !Int -> Point
InConnectionPoint :: !(CompSpecifics Component) !Placement !Int -> Point

 // Given a mouse position and a circuit, returns the connection point of any connection
 // that the mouse position is near enough to (that is not already connected to a wire),
 // and a four-tuple giving other information about the connection - what kind of
 // connection was found (NONE, IN (input), or OUT (output)), the component ID for the
 // component, an integer (from 0 up) identifying which of the component's input or
 // output connections was selected, and the wire type of the connection.
GetConnectionPoint :: Point !Circuit -> (Point, ConnectionInfo)

 // Used to determine if the information returned from GetConnectionPoint
 // indicates that a connection point was found for the mouse position.
IsNotConnected :: !ConnectionInfo -> Bool

 // Takes a mouse position, a circuit and a list of the circuit's wires, and returns
 // True if the mouse position was near to a connection point or wire in the circuit
 // (False otherwise) and the wire type of the connection or wire.
GetSelectedType :: Point !Circuit [Wire] -> (Bool, WireType)

 // Returns True if the point is on the line, within a small margin, False otherwise.
IsOnLine :: !Point !Line -> Bool

 // Returns the connection type (IN for input, OUT for output), wire ID
 // and connection point for all the component's connections that are
 // connected to wires.  For boxes, this includes all such connections
 // for components in its box circuit.
ComponentConnections :: !Component -> [(ConnectionType, WireID, Point)]

 // Takes the point at the centre of a UnitEL's lasso and returns the component's left
 // input connection point.
LassoLeftInConnection :: !Point -> Point
 // Takes the point at the centre of a CounitIL's lasso and returns the component's left
 // output connection point.
LassoLeftOutConnection :: !Point -> Point
 // Takes the point at the centre of a UnitEL's lasso and returns the component's right
 // input connection point.
LassoRightInConnection :: !Point -> Point
 // Takes the point at the centre of a CounitIL's lasso and returns the component's right
 // output connection point.
LassoRightOutConnection :: !Point -> Point
 // Takes the point at the centre of a UnitEL, UnitER, CounitIL or CounitIR, and returns the
 // input connection point of the component's lasso loop.
LassoInConnection :: !Point -> Point
 // Takes the point at the centre of a UnitEL, UnitER, CounitIL or CounitIR, and returns the
 // output connection point of the component's lasso loop.
LassoOutConnection :: !Point -> Point

 // Returns the rectangles of a component's parts.  For a box, these are its
 // border lines, the rectangles around its connection areas, and the component
 // rectangles (defined the same way) of the components in its box circuit.
 // For other components, this returns only the rectangle enclosing the
 // component.
ComponentRectangles :: !Component -> [Rectangle]

 // Returns the rectangle that completely encloses a component (excluding
 // connections).
ComponentRectangle :: !Component -> Rectangle

 // Takes the border rectangle of a Box and returns the rectangles enclosing
 // the sides of its border and the connection areas.
BoxEdgeRectangles :: !Rectangle -> [Rectangle]

 // Takes an EditType indicating what component is to be drawn, and returns
 // the drawing functions needed to draw that component on a button in the
 // toolbox.
DrawButtonIcon :: !EditType -> [DrawFunction]

 // Returns a list of drawing functions necessary to draw the component.
DrawComponent :: !Component -> [DrawFunction]

 // Takes a component and a list of the circuit's wires and returns the list of drawing
 // functions necessary to draw the wires connected to the component.
DrawComponentWires :: !Component ![Wire] -> [DrawFunction]

 // Takes a wire and returns a list of drawing functions necessary to draw the wire.
DrawWire :: !Wire -> [DrawFunction]

 // Given a component, a wire ID and a list of wires, returns the
 // list of drawing functions necessary to draw a "selection
 // indicator" (which shows that the component or wire is currently
 // selected) around the component (if the wire ID is -1) or wire.
DrawSelection :: !Component !WireID [Wire] -> [DrawFunction]

 // Returns the rectangle at the top left of a box's border that can
 // be selected for sizing the box from this corner.
TopLeftRectangle :: !Rectangle -> !Rectangle

 // Returns the rectangle at the top right of a box's border that can
 // be selected for sizing the box from this corner.
TopRightRectangle :: !Rectangle -> !Rectangle

 // Returns the rectangle at the bottom left of a box's border that can
 // be selected for sizing the box from this corner.
BottomRightRectangle :: !Rectangle -> !Rectangle

 // Returns the rectangle at the bottom left of a box's border that can
 // be selected for sizing the box from this corner.
BottomLeftRectangle :: !Rectangle -> !Rectangle

 // Returns True if the rectangle is "too small" to be the border of a Box.
TooSmallBox :: !Rectangle -> Bool

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

 // The radius of the half-circle base of the StartTerminal and EndTerminal.
terminalRadius :== 4

 // Given a wire type, returns just the type.
GetType :: !WireType -> Type

 // Creates a new connection with the wire ID set to -1 (to indicate that it is
 // not connected to a wire) and the wire type free (rather than user-specified),
 // with the given type.
NewConnection :: Type -> Connection

 // Takes a list of variable substitutions and a circuit, and makes the variable
 // substitutions in all free wire types of the circuit.  (Assumes that the
 // variables being substituted in are independent of the variables being replaced.)
SubsIntoCircuit :: [Substitution] !Circuit -> Circuit

 // Takes a list of variable substitutions and a connection, and makes the variable
 // substitutions in all free wire types of the connection.  (Assumes that the
 // variables being substituted in are independent of the variables being replaced.)
SubsIntoConnect :: [Substitution] !Connection -> Connection

 // Takes a list of variable substitutions and a list of wires, and makes the
 // variable substitutions in all free wire types of the wires.  (Assumes that
 // the  variables being substituted in are independent of the variables being
 // replaced.)
SubsIntoWires :: [Substitution] ![Wire] -> [Wire]

 // Takes a list of variable substitutions and a wire type, and makes the variable
 // substitutions in the wire type if it is free, but not if it is user-specified.
 // (Assumes that the  variables being substituted in are independent of the
 // variables being replaced.)
SubsIntoWireType :: [Substitution] !WireType -> WireType


 // Takes a predicate and a list, and returns two lists, with everything that
 // satisfies the predicate in the first list and everything else in the other.
filterInOut :: (a -> Bool) ![a] -> ([a],[a])

 // Returns True if the second item is a member of the first list,
 // False otherwise (flips the arguments of isMember).
MemberOf :: ![a] a -> Bool | == a

 // Finds the item in the list indexed by the integer (starting from 0)
 // and replaces it with the third argument.
Replace :: !Int ![a] a -> [a]

 // Finds the first item in the list that satisfies the predicate
 // and returns it with the remainder of the list, still in order.
RemoveAndReturn :: !(a -> Bool) ![a] -> (a, [a])

 // Finds the first item in the list that satisfies the predicate
 // and returns it.
GetFirst :: !(a -> Bool) ![a] -> a

 // Reverses and appends the first list to the second.
ReverseAppend :: ![a] [a] -> [a]

 // Given two lists (each with no duplicate items),
 // returns the two lists in their original orders,
 // with all items that they have in common removed.
RemoveIntersection :: [a] [a] -> ([a],[a]) | == a

 // Returns the vector from the first to the second (the difference)
VectorDifference :: !Vector !Vector -> Vector

 // Returns True if the component is a Box, False otherwise.
IsBox :: !Component -> Bool

 // Returns the internal circuit of a Box.
BoxCircuit :: !Component -> Circuit

 // Returns True if the component is a Lolly, False otherwise.
IsLolly :: !Component -> Bool

 // Returns True if the component is a StartTerminal, False otherwise.
IsStartTerminal :: !Component -> Bool

 // Returns True if the component is a EndTerminal, False otherwise.
IsEndTerminal :: !Component -> Bool

 // Returns True if the component is a UnitEL, UnitER, CounitIL or CounitIR,
 // False otherwise.
IsThinningLink :: !Component -> Bool

 // Returns the wire ID of the wire that connects down into the lasso loop, the wire ID of the wire
 // that connects down from the lasso loop, and the wire ID of the wire into the unit or counit (the
 // lasso wire) of a thinning link.
GetLassoWireIDs :: !Component -> (WireID, WireID, WireID)

 // Returns True if the component is a dummy component, False otherwise.
IsDummy :: !Component -> Bool
dummyComponent :== {spec=EndTerminal, id=(-1), pos=PT (-1,-1), inputs=[], outputs=[]}


 // Returns a list pairing the horizontal position of each of the
 // circuit's external input connections with the connection,
 // sorted by horizontal position and connection wire ID.
GetInputPositionPairs :: [Wire] ![Connection] -> [(Int, Connection)]
 // Returns a list pairing the horizontal position of each of the
 // circuit's external output connections with the connection,
 // sorted by horizontal position and connection wire ID.
GetOutputPositionPairs :: [Wire] ![Connection] -> [(Int, Connection)]

 // Given a StartTerminal or EndTerminal that has been moved, a list of input
 // position pairs and a list of output position pairs, returns a new list
 // of input position pairs and a new list of output position pairs in which
 // the horizontal position for the terminal's connection has been changed
 // to match the terminal's new position.
ChangePositionPair :: !Component [(Int, Connection)] [(Int, Connection)] -> ([(Int, Connection)], [(Int, Connection)])
 // Given an old wire ID, a new wire ID, and a list of position pairs, returns a
 // new list of position pairs in which the wire ID of the connection identified
 // by the old wire ID has been changed to the new wire ID, or returns the list
 // unchanged if no such connection is found.
ChangePositionPairID :: !WireID WireID ![(Int, Connection)] -> [(Int, Connection)]

 // Given a wire ID identifying an external circuit wire, a list of position pairs,
 // and a maximum horizontal position, returns the smallest horizontal position (no
 // smaller than 0) and the largest horizontal position (no greater than the maximum
 // given) to which the external connection point of the wire may be moved, as
 // determined by the horizontal positions of the external connections on either side
 // of it (if any).
GetBounds :: !WireID ![(Int, Connection)] Int -> (Int, Int)

 // Given a StartTerminal or EndTerminal, returns the horizontal position of its
 // connection.
TerminalXPosition :: !Component -> Int


 // Returns True if the wire type is user-specified, False otherwise.
IsUserType :: !WireType -> Bool
 // If the wire type is free, this makes it user-specified without changing its type.
MakeUserType :: !WireType -> WireType
 // If the wire type is user-specified, this makes it free without changing its type.
MakeFree :: !WireType -> WireType

 // Takes a wire ID identifying a wire, the wire's position line in the circuit,
 // a new wire type for the wire, and a circuit, and changes the wire's wire
 // type on its two connection points in the circuit.
ChangeWireType :: WireID !Line WireType !Circuit -> Circuit
ChangeTypeOnInput :: WireID Point WireType !Circuit -> Circuit
ChangeTypeOnOutput :: WireID Point WireType !Circuit -> Circuit

 // Finds the maximum of all the variable numbers used in the list of types
 // and the given variable number.
FindMaxVarInTypes :: ![Type] Int -> Int
 // Finds the maximum of all the variable numbers used in the type
 // and the given variable number.
FindMaxVarInType :: !Type Int -> Int

 // Takes a circuit and returns a list of the circuit's wires,
 // by proceeding down the circuit, storing the wire ID and
 // connection point of every output encountered and then
 // using this to create wires when the inputs are encountered.
GetCircuitWires :: !Circuit -> [Wire]

 // Takes a circuit and returns the next free component ID, the next free wire ID
 // and the next free variable number for the circuit (1 more than the maximum
 // already in use for each of these).
GetCircuitNextVals :: !Circuit -> (ComponentID, WireID, Int)

 // Takes an integer n, a list of types and an initial maximum variable number,
 // and returns the list of types with n added to all the variable numbers and
 // the new maximum variable number.
AddToVarsInTypes :: Int ![Type] Int -> ([Type], Int)
 // Takes an integer n, a types and an initial maximum variable number, and
 // returns the type with n added to all the variable numbers and the new
 // maximum variable number.
AddToVarsInType :: Int !Type Int -> (Type, Int)

 // Returns True if the two component specifications are equivalent, False
 // otherwise.  Any two Boxes are considered to be equivalent (even if they
 // have different internal circuits).  Two Generics must have the same
 // name, input types and output types to be equivalent.
//*^*EquivalentSpecs :: !CompSpecifics !CompSpecifics -> Bool
EquivalentSpecs :: !(CompSpecifics Component) !(CompSpecifics Component) -> Bool
