// This module contains some type definitions common to the main module and other modules, including those for
// circuits and components.

implementation module circuitDefs

import deltaPicture, typeDefs

// The circuit is always ordered, i.e. if one component connects down (directly or indirectly) from a second component
// then it will be later in the list than the second component.  (Cycles are not allowed.)
:: Circuit :== [Component]

// spec identifies the kind of component and includes any extra information specific to that kind of component,
// id uniquely identifies the component, inputs gives the list of input connections in order from left to right,
// outputs gives the list of output connections (also ordered), and pos gives information about the position
// and dimensions of the component.
//*^*:: Component = {spec :: CompSpecifics,
//*^*                id :: ComponentID,
//*^*                inputs :: [Connection],
//*^*                outputs :: [Connection],
//*^*                pos :: Placement
//*^*               }
:: Component = {spec    :: CompSpecifics Component,   //*^* This version is currently in use to allow
                id      :: ComponentID,               //*^* (CompSpecifics Debug) to be used in debugging.
                inputs  :: [Connection],
                outputs :: [Connection],
                pos     :: Placement
               }

// StartTerminal and EndTerminal are used to anchor input and output wires for a circuit, and the pos field in each case
// gives a point at the base of the terminal.  TensorE is the tensor elimination, TensorI is tensor introduction, and SumE
// and SumI are similar.  For these and Lolly, the pos field gives a point at the centre of the component's circle.  UnitI
// is unit introduction, UnitEL is unit elimination left, UnitER is unit elimination right, and CounitE, CounitIL, and
// CounitIR are similar.  For these, the pos field gives a point centred in the lasso on the wire.  Box is an abstraction
// box with its internal circuit.  Its pos field gives the box's border rectangle, its inputs field gives the box's internal
// input, and its outputs field gives the box's internal and external outputs, in that order.  Sequent is a sequent box
// used only in sequentialization, and its position is irrelevant.  It also has a list of wire IDs identifying the
// wires that are internal to the sequentialized circuit (external wires are identified by the inputs and outputs to
// the sequent box).  Generic is used for user-defined components.  It gives the name of the component (as a string)
// and two lists of types giving (in order) the most general types for the inputs and outputs to the component, with
// variables starting at zero so that the component can be instantiated by adding the next variable number to all variables
// in its types and using these for the component's inputs and outputs.
//*^*:: CompSpecifics = StartTerminal | EndTerminal | TensorE | TensorI | SumE | SumI | Lolly | Box [Circuit] |
//*^*                   UnitI | UnitEL | UnitER | CounitE | CounitIL | CounitIR | Sequent [WireID] |
//*^*                   Generic String [Type] [Type]
:: CompSpecifics s = StartTerminal | EndTerminal | TensorE | TensorI | SumE | SumI | Lolly | Box [s] |
                     UnitI | UnitEL | UnitER | CounitE | CounitIL | CounitIR | Sequent [WireID] |
                     Generic String [Type] [Type] //*^* For efficiency, it might be a good idea to add lists giving
                                                  //*^* the offsets of the connections from the left side of the
                                                  //*^* component and using them in DrawGeneric - this would avoid doing
                                                  //*^* floating-point arithmetic repeatedly when the component is being
                                                  //*^* moved.

:: Placement = PT Point | RCT Rectangle

// This is used when connecting two components by a wire - it gives information about one of the two connection points.
// ConnectionType gives the type of connection, ComponentID identifies the component, ConnectionID identifies the
// input or output connection on the component, and WireType gives the connection's wire type.
:: ConnectionInfo :== (ConnectionType, ComponentID, ConnectionID, WireType)

// IN indicates an input connection, OUT indicates an output connection, and NONE indicates that no connection was made.
:: ConnectionType = IN | OUT | NONE

// cWireID identifies the wire attached to that connection (-1 means it is not connected), and cWireType gives
// the type information for that connection (including whether the type was specified by the user for the connecting
// wire), duplicating the type information on the connecting wire and the other connection.
:: Connection = {cWireID :: WireID, cWireType :: WireType}

// wireID identifies the wire uniquely, wireLine gives the position of the wire, with the first point at the output
// connection point of the wire, and wireType gives the wire's type information, including whether the type was
// specified by the user.
:: Wire = {wireID :: WireID, wireLine :: Line, wireType :: WireType}

:: ComponentID :== Int
:: ConnectionID :== Int
:: WireID :== Int

// Gives the current edit mode
:: EditMode = EditingCircuit | ProofMode

// These indicate which button in the toolbox is currently selected, which determines the behaviour of the mouse.
// The "Draw" edit types are for adding wires or components of the given type, Selection is for selecting components
// and wires (used with both EditingCircuit and ProofMode), and Rewiring is for rewiring thinning links in ProofMode.
:: EditType = StartTerminalDraw | EndTerminalDraw | TensorEDraw | TensorIDraw | SumEDraw | SumIDraw |
              LollyDraw | UnitIDraw | UnitELDraw | UnitERDraw | CounitEDraw | CounitILDraw | CounitIRDraw |
              BoxDraw | GenericDraw | WireDraw | Selection | Rewiring

// Values returned by various functions to indicate what error, if any, occurred.
:: ErrorVal = NoError | NoSelection | BadWire | NewCompOverlaps | BadRewiring |
              LeftSideNotWired | RightSideNotWired | BadLeftSideTerminals | BadRightSideTerminals |
              InputCountMismatch | OutputCountMismatch | NotConnected | BadLiveWire | TypeMismatch |
              BadMatch | BadSequent
