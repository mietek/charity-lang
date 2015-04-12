// Written by Gillian Posey, 1996.

// This is the main module of the program.  It contains one function (Start) with interface
// code in its where clause.  Mouse events for both circuit windows are handled by
// HandleMouse.

// Note that all drawing is done in XOR mode, so that a component, etc. can be erased by
// drawing it a second time.  Any list of drawing functions should have (SetPenMode XorMode)
// as its first function.

//*^* Drawing generics could be made more efficient by making the changes described in circuitDefs
//*^* (type definition) and utilities (definition of DrawGeneric).

//*^* Should CheckEquality check for equality of types?

//*^* Some menu functions should be disabled when the current window is closed and enabled when a
//*^* window is clicked in (e.g. Erase, FILLSequent, etc.).

//*^* Should keep track of current file name and whether it has been saved, and have both "Save" and "Save As" options.

//*^* Probably should be able to move links in rewiring mode (including inside and outside of boxes?).  Ought to ask if
//*^* it should save before exiting (if not already saved).

//*^* If wire instead of just wireID were kept, DeleteWire could use position data to do deletion more efficiently.

//*^* Would be nice to be able to change positions of box connection points.
//*^* Change moves so that wires move as the component moves?
//*^* Add undo?

module circuits

import StdEnv, deltaWindow, deltaDialog, deltaEventIO, deltaPicture, deltaSystem, deltaMenu, deltaFileSelect
import unify, circuitDefs, typeCircuit, utilities, insert, sequent, typeDefs, circuitSave, rewire, rewriteDefs,
       rewrite, stdRewrites, rewriteSave, circuitWireMatch, parseType, generics

import debug  //*^* Can be removed when CheckConnections is removed.  (Also contains functions that can be used to
              //*^* compare DEBUG circuits when debugging).

:: *ProgState = {windowInfo        :: (WindowState, WindowState),  // Contains info about the state of the circuit windows.
                 currentWindow     :: Window,                      // Indicates the last (open) window that was clicked in.
                 editMode          :: EditMode,                    // EditingCircuit or ProofMode.
                 editType          :: EditType,                    // Indicates the current meaning of a mouse click.
                 drawMode          :: DrawMode,                    // Indicates what should happen as the mouse moves.
                 sizeDir           :: SizeDirection,               // When sizing a box, indicates the selected corner.
                 itemSelected      :: Bool,                        // Indicates whether an item is selected or not.
                 currentComp       :: Component,                   // The selected component, if any.
                 currentWire       :: WireID,                      // Identifies the selected wire, if any (-1 otherwise).
                 oldComp           :: Component,                   // The selected component before moving or sizing.
                 connectFrom       :: ConnectionInfo,              // Info about the initial connection point of a wire.
                 startPos          :: Point,                       // The start position for drawing, moving, etc.
                 lastPos           :: Point,                       // The last position for drawing, moving, etc.
                 terminalBounds    :: (Int, Int),                  // The horizontal boundaries for movement of a start
                                                                   // or end terminal in ProofMode.
                 nextID            :: Int,                         // The next free ID for menu items, etc.
//*^*                 generics          :: [(CompSpecifics, Int, Int)], // A list of info about user-defined components,
                                                                        // giving the specifics, width and height.
                 generics          :: [(CompSpecifics Component, Int, Int)],
                 newGenericDisplay :: Circuit,                     // A temporary circuit used to display a new user-defined
                                                                   // component while its types are being set.
                 files             :: *Files                       // The current file system.
                }

:: Window = Window1 | Window2 | NoWindow

:: WindowState = {windowID     :: WindowId,               // The ID used in creating the window.
                  windowDef    :: WindowDef ProgState IO, // The window definition used in creating the window.
                  windowName   :: String,                 // The window's name (displayed in its caption).
                  fileName     :: String,                 // The file name for the circuit in the window.
                  windowOpen   :: Bool,                   // Indicates whether the window is open.
                  circuit      :: Circuit,                // The window's circuit.
                  wires        :: [Wire],                 // The wires for the window's circuit.
                  compID       :: ComponentID,            // The next free component ID for the window's circuit.
                  nextWireID   :: WireID,                 // The next free wire ID for the window's circuit.
                  nextVar      :: Int,                    // The next free variable number for the window's circuit.
                  inputPairs   :: [(Int, Connection)],    // Sorted pairs giving the horizontal positions and connections
                                                          // of the circuit's start terminals in ProofMode.
                  outputPairs  :: [(Int, Connection)],    // Sorted pairs giving the horizontal positions and connections
                                                          // of the circuit's end terminals in ProofMode.
                  windowDomain :: PictureDomain           // The window's rectangular drawing region.
                 }

// Indicates what should happen as the mouse is moved with the button down.
:: DrawMode = NotDrawing | DrawingWire | DrawingBox | MoveSelection | SizeBox

// Indicates which corner of a box was selected to size it.
:: SizeDirection = TopLeft | TopRight | BottomLeft | BottomRight

:: *IO :== IOState ProgState

Start :: !*World -> *World
Start world1 = CloseEvents leftovers (closefiles finalState.files world3)
where
  (events, world2) = OpenEvents world1
  (initialFiles, world3) = openfiles world2
  (finalState, leftovers) = StartIO [menu, WindowSystem [window1], DialogSystem [toolbox1]] initialState initialIO events

  initialState = {windowInfo = (window1State, window2State),
                  currentWindow = Window1,
                  editMode = EditingCircuit,
                  editType = StartTerminalDraw,  // A mouse click will create a StartTerminal initially.
                  drawMode = NotDrawing,
                  sizeDir = TopLeft,
                  itemSelected = False,
                  currentComp = dummyComponent,
                  oldComp = dummyComponent,
                  currentWire = -1,
                  connectFrom = (NONE, 0, 0, Free Unit),
                  startPos = (-1,-1),
                  lastPos = (-1,-1),
                  terminalBounds = (0,0),
                  nextID = firstFreeID,
                  generics = [],
                  newGenericDisplay = [],
                  files = initialFiles
                 }

  window1State = {windowID = window1ID,
                  windowDef = window1,
                  windowName = "Window 1",
                  fileName = "",
                  windowOpen = True,
                  circuit = [],
                  wires = [],
                  compID = 0,
                  nextWireID = 0,
                  nextVar = 1,
                  inputPairs = [],
                  outputPairs = [],
                  windowDomain = initialDomain
                 }
  window2State = {window1State & windowID = window2ID, windowDef=window2, windowName = "Window 2", windowOpen = False}

  initialIO = []

  menu = MenuSystem [PullDownMenu fileID "File" Able
                      [MenuItem saveID "Save Circuit" (Key 's') Able Save,
                       MenuItem readID "Read Circuit" NoKey Able Read,
                       MenuSeparator,
                       MenuItem openID "Open Window" NoKey Able OpenProgWindow,
                       MenuItem closeID "Close Window" NoKey Able CloseProgWindow,
                       MenuSeparator,
                       MenuItem saveRewID "Save New Rewrite" NoKey Unable SaveNewRewrite,
                       MenuItem readRewID "Read Rewrite File" NoKey Able ReadRewritesFile,
                       MenuSeparator,
                       MenuItem quitID "Quit" NoKey Able Quit
                      ],
                     PullDownMenu editID "Edit" Able
                      [MenuItem deleteID "Delete" (Key 'd') Unable DeleteSelection,
                       MenuItem setTypeID "Set Type" NoKey Unable SetNewWireType,
                       MenuItem removeTypeID "Remove User Type" NoKey Unable RemoveWireType,
                       MenuItem redrawID "Redraw" NoKey Able Redraw,
                       MenuSeparator,
                       MenuItem eraseID "Erase circuit" NoKey Able EraseCirc
                      ],
                     PullDownMenu modeID "Mode" Able
                      [CheckMenuItem editModeID "Edit Mode" NoKey Able Mark (SetEditMode EditingCircuit),
                       CheckMenuItem proofModeID "Proof Mode" NoKey Able NoMark (SetEditMode ProofMode)
                      ],
                     PullDownMenu newCompsID "New Components" Able
                      [MenuItem addCompID "Create Component" NoKey Able AddGeneric,
                       MenuItem readCompID "Read Saved Components" NoKey Able ReadGenericsFile
                      ],
                     PullDownMenu flipLinkMenuID "Flip Link" Unable
                      [MenuItem flipLinkID "Flip Thinning Link" (Key 'f') Able FlipLink],
                     PullDownMenu doRewritesID "Do Rewrite" Unable
                      [MenuItem doRewrite1ID tensorReduct.rewriteName NoKey Able (DoProofRewrite tensorReduct),
                       MenuItem doRewrite2ID sumReduct.rewriteName NoKey Able (DoProofRewrite sumReduct),
                       MenuItem doRewrite3ID unitReductLeft.rewriteName NoKey Able (DoProofRewrite unitReductLeft),
                       MenuItem doRewrite4ID unitReductRight.rewriteName NoKey Able (DoProofRewrite unitReductRight),
                       MenuItem doRewrite5ID counitReductLeft.rewriteName NoKey Able (DoProofRewrite counitReductLeft),
                       MenuItem doRewrite6ID counitReductRight.rewriteName NoKey Able (DoProofRewrite counitReductRight),
                       MenuItem doRewrite7ID "Box reduction" NoKey Able DoProofBoxRewrite,
                       MenuSeparator,
                       MenuItem doRewrite8ID tensorExp.rewriteName NoKey Able (DoProofRewrite tensorExp),
                       MenuItem doRewrite9ID sumExp.rewriteName NoKey Able (DoProofRewrite sumExp),
                       MenuItem doRewrite10ID unitExpLeft.rewriteName NoKey Able (DoProofRewrite unitExpLeft),
                       MenuItem doRewrite11ID unitExpRight.rewriteName NoKey Able (DoProofRewrite unitExpRight),
                       MenuItem doRewrite12ID counitExpLeft.rewriteName NoKey Able (DoProofRewrite counitExpLeft),
                       MenuItem doRewrite13ID counitExpRight.rewriteName NoKey Able (DoProofRewrite counitExpRight),
                       MenuItem doRewrite14ID boxExp.rewriteName NoKey Able (DoProofRewrite boxExp),
                       MenuSeparator,
                       MenuItemGroup doRewritesGroupID []  // Menu items for new rewrites will be added here.
                      ],
                     PullDownMenu viewRewritesID "View Rewrite" Able
                      [MenuItem closeRewritesID "Close Rewrite Windows" NoKey Unable CloseRewrites,
                       MenuSeparator,
                       MenuItem viewRewrite1ID tensorReduct.rewriteName NoKey Able (DisplayRewrite tensorReduct),
                       MenuItem viewRewrite2ID sumReduct.rewriteName NoKey Able (DisplayRewrite sumReduct),
                       MenuItem viewRewrite3ID unitReductLeft.rewriteName NoKey Able (DisplayRewrite unitReductLeft),
                       MenuItem viewRewrite4ID unitReductRight.rewriteName NoKey Able (DisplayRewrite unitReductRight),
                       MenuItem viewRewrite5ID counitReductLeft.rewriteName NoKey Able (DisplayRewrite counitReductLeft),
                       MenuItem viewRewrite6ID counitReductRight.rewriteName NoKey Able (DisplayRewrite counitReductRight),
                       MenuItem viewRewrite7ID "Box reduction" NoKey Able DisplayBoxRewrite,
                       MenuSeparator,
                       MenuItem viewRewrite8ID tensorExp.rewriteName NoKey Able (DisplayRewrite tensorExp),
                       MenuItem viewRewrite9ID sumExp.rewriteName NoKey Able (DisplayRewrite sumExp),
                       MenuItem viewRewrite10ID unitExpLeft.rewriteName NoKey Able (DisplayRewrite unitExpLeft),
                       MenuItem viewRewrite11ID unitExpRight.rewriteName NoKey Able (DisplayRewrite unitExpRight),
                       MenuItem viewRewrite12ID counitExpLeft.rewriteName NoKey Able (DisplayRewrite counitExpLeft),
                       MenuItem viewRewrite13ID counitExpRight.rewriteName NoKey Able (DisplayRewrite counitExpRight),
                       MenuItem viewRewrite14ID boxExp.rewriteName NoKey Able (DisplayRewrite boxExp),
                       MenuSeparator,
                       MenuItemGroup viewRewritesGroupID []  // Menu items for new rewrites will be added here.
                      ],
                     PullDownMenu editRewritesID "Edit Rewrite" Unable
                      [MenuItem editRewrite1ID tensorReduct.rewriteName NoKey Able (EditRewrite tensorReduct),
                       MenuItem editRewrite2ID sumReduct.rewriteName NoKey Able (EditRewrite sumReduct),
                       MenuItem editRewrite3ID unitReductLeft.rewriteName NoKey Able (EditRewrite unitReductLeft),
                       MenuItem editRewrite4ID unitReductRight.rewriteName NoKey Able (EditRewrite unitReductRight),
                       MenuItem editRewrite5ID counitReductLeft.rewriteName NoKey Able (EditRewrite counitReductLeft),
                       MenuItem editRewrite6ID counitReductRight.rewriteName NoKey Able (EditRewrite counitReductRight),
                       MenuItem editRewrite7ID "Box reduction" NoKey Able EditBoxRewrite,
                       MenuSeparator,
                       MenuItem editRewrite8ID tensorExp.rewriteName NoKey Able (EditRewrite tensorExp),
                       MenuItem editRewrite9ID sumExp.rewriteName NoKey Able (EditRewrite sumExp),
                       MenuItem editRewrite10ID unitExpLeft.rewriteName NoKey Able (EditRewrite unitExpLeft),
                       MenuItem editRewrite11ID unitExpRight.rewriteName NoKey Able (EditRewrite unitExpRight),
                       MenuItem editRewrite12ID counitExpLeft.rewriteName NoKey Able (EditRewrite counitExpLeft),
                       MenuItem editRewrite13ID counitExpRight.rewriteName NoKey Able (EditRewrite counitExpRight),
                       MenuItem editRewrite14ID boxExp.rewriteName NoKey Able (EditRewrite boxExp),
                       MenuSeparator,
                       MenuItemGroup editRewritesGroupID []  // Menu items for new rewrites will be added here.
                      ],
                                                                     // VVV Menu items for new rewrites will be added here.
                     PullDownMenu saveRewritesID "Save Rewrite" Unable [MenuItemGroup saveRewritesGroupID []],
                     PullDownMenu verifyID "Verify" Able
                      [MenuItem showTypeID "Show Wire Type" NoKey Able ShowType,
                       MenuItem fillSequentID "FILL Sequentialize" NoKey Able FILLSequent,
                       MenuItem equalityID "Equality Check" NoKey Unable CheckEquality,
                       MenuItem checkConnectsID "Check Connections" NoKey Able CheckConnects //*^* For debugging checks
                      ]
                    ]

  window1 = ScrollWindow window1ID windowPos "Window 1: Not saved"
              (ScrollBar (Thumb thumbValue) (Scroll scrollValue))
              (ScrollBar (Thumb thumbValue) (Scroll scrollValue))
              initialDomain minWinSize initWinSize (RedrawCircuit Window1)
              [Mouse Able (HandleMouse Window1)]

  window2 = ScrollWindow window2ID windowPos "Window 2: Not saved"
              (ScrollBar (Thumb thumbValue) (Scroll scrollValue))
              (ScrollBar (Thumb thumbValue) (Scroll scrollValue))
              initialDomain minWinSize initWinSize (RedrawCircuit Window2)
              [Mouse Able (HandleMouse Window2)]

  // The toolbox when in EditingCircuit mode.  The Unable button is the one currently selected.
  toolbox1 = CommandDialog toolbox1ID "Tool Box" [/* No attributes specified */] dummyID
              [DialogIconButton startTermID Left buttonDomain (DrawButton StartTerminalDraw) Unable (SetEditType StartTerminalDraw),
               DialogIconButton endTermID (RightTo startTermID) buttonDomain (DrawButton EndTerminalDraw) Able (SetEditType EndTerminalDraw),
               DialogIconButton tensorEID (RightTo endTermID) buttonDomain (DrawButton TensorEDraw) Able (SetEditType TensorEDraw),
               DialogIconButton tensorIID (RightTo tensorEID) buttonDomain (DrawButton TensorIDraw) Able (SetEditType TensorIDraw),
               DialogIconButton sumEID (RightTo tensorIID) buttonDomain (DrawButton SumEDraw) Able (SetEditType SumEDraw),
               DialogIconButton sumIID (RightTo sumEID) buttonDomain (DrawButton SumIDraw) Able (SetEditType SumIDraw),
               DialogIconButton lollyID (RightTo sumIID) buttonDomain (DrawButton LollyDraw) Able (SetEditType LollyDraw),
               DialogIconButton boxID (RightTo lollyID) buttonDomain (DrawButton BoxDraw) Able (SetEditType BoxDraw),
               DialogIconButton genericID (RightTo boxID) buttonDomain (DrawButton GenericDraw) Able (SetEditType GenericDraw),
               DialogIconButton unitIID Left buttonDomain (DrawButton UnitIDraw) Able (SetEditType UnitIDraw),
               DialogIconButton unitELID (RightTo unitIID) buttonDomain (DrawButton UnitELDraw) Able (SetEditType UnitELDraw),
               DialogIconButton unitERID (RightTo unitELID) buttonDomain (DrawButton UnitERDraw) Able (SetEditType UnitERDraw),
               DialogIconButton counitEID (RightTo unitERID) buttonDomain (DrawButton CounitEDraw) Able (SetEditType CounitEDraw),
               DialogIconButton counitILID (RightTo counitEID) buttonDomain (DrawButton CounitILDraw) Able (SetEditType CounitILDraw),
               DialogIconButton counitIRID (RightTo counitILID) buttonDomain (DrawButton CounitIRDraw) Able (SetEditType CounitIRDraw),
               DialogIconButton drawWireID (RightTo counitIRID) buttonDomain (DrawButton WireDraw) Able (SetEditType WireDraw),
               DialogIconButton select1ID (RightTo drawWireID) buttonDomain (DrawButton Selection) Able (SetEditType Selection)
              ]

  // The toolbox when in ProofMode.  The Unable button is the one currently selected.
  toolbox2 = CommandDialog toolbox2ID "Tool Box" [/* No attributes specified */] dummyID
              [DialogIconButton rewireID Left buttonDomain (DrawButton Rewiring) Able (SetProofEditType Rewiring),
               DialogIconButton select2ID (RightTo rewireID) buttonDomain (DrawButton Selection) Unable
                                (SetProofEditType Selection)
              ]

  // All IDs used in the creation of windows, menus, etc.
  [dummyID, fileID, saveID, readID, openID, closeID, saveRewID, readRewID, quitID, editID, deleteID, setTypeID,
   removeTypeID, redrawID, eraseID, verifyID, modeID, editModeID, proofModeID, newCompsID, addCompID, readCompID,
   flipLinkMenuID, flipLinkID, doRewritesID, doRewritesGroupID, doRewrite1ID, doRewrite2ID, doRewrite3ID, doRewrite4ID,
   doRewrite5ID, doRewrite6ID, doRewrite7ID, doRewrite8ID, doRewrite9ID,  doRewrite10ID, doRewrite11ID, doRewrite12ID,
   doRewrite13ID, doRewrite14ID, viewRewritesID, closeRewritesID, viewRewritesGroupID,  viewRewrite1ID, viewRewrite2ID,
   viewRewrite3ID, viewRewrite4ID, viewRewrite5ID, viewRewrite6ID, viewRewrite7ID, viewRewrite8ID, viewRewrite9ID,
   viewRewrite10ID, viewRewrite11ID, viewRewrite12ID, viewRewrite13ID, viewRewrite14ID, editRewritesID,
   editRewritesGroupID,  editRewrite1ID, editRewrite2ID, editRewrite3ID, editRewrite4ID, editRewrite5ID, editRewrite6ID,
   editRewrite7ID, editRewrite8ID, editRewrite9ID, editRewrite10ID, editRewrite11ID, editRewrite12ID, editRewrite13ID,
   editRewrite14ID, saveRewritesID, saveRewritesGroupID, showTypeID, fillSequentID, equalityID, checkConnectsID, window1ID, window2ID,
   toolbox1ID, startTermID, endTermID, tensorEID, tensorIID, sumEID, sumIID, lollyID, unitIID, unitELID, unitERID,
   counitEID, counitILID, counitIRID, boxID, genericID, drawWireID, select1ID, toolbox2ID, rewireID, select2ID, noticeID,
   noID, yesID, rewriteInfoID, ok1ID, ok2ID, cancel1ID, cancel2ID, text1ID, text2ID, rewriteNameID, wireTypeDlgID,
   wireTypeID, leftSideWindowID, rightSideWindowID, addGenericID, compNameID, compInputsID, compOutputsID,
   genericDisplayWinID, genericListID, getGenericID, firstFreeID : _] = [0..]

  ////////////////////// *** Toolbox functions

  ////////// DrawButton
  // Takes an edit type indicating which button is to be drawn and its select state, indicating
  // whether it is enabled or not (the current button will not be enabled, and will be drawn
  // with a thick border, other buttons are enabled and will be drawn with a normal (thin) border.
  // Returns the drawing functions to draw the button.
  DrawButton :: EditType !SelectState -> [DrawFunction]
  DrawButton item Able
    = [DrawRectangle buttonDomain :
       DrawButtonIcon item
      ]
  DrawButton item Unable
    = [SetPenSize (2,2),
       DrawRectangle buttonDomain,
       SetPenSize (1,1) :
       DrawButtonIcon item
      ]

  ////////// SetEditType
  // Sets the new editType in the program state, enables the button for the old editType and
  // and disables the button for the new editType.  (Called in EditingCircuit mode.)
  SetEditType :: EditType DialogInfo !ProgState IO -> (ProgState, IO)

  // In this case, the old edit type was Selection (since an item is selected).  The item must be deselected
  // when the edit type changes.
  SetEditType newEditType _ state=:{itemSelected=True, currentComp, currentWire, currentWindow, windowInfo} io
    = ({state & itemSelected = False, editType = newEditType}, newIO)
  where
    {windowID, wires} = GetWindowState currentWindow windowInfo
    newIO = ChangeDialog toolbox1ID
             [DisableDialogItems [GetItemID newEditType],
              EnableDialogItems [select1ID]
             ] (Deselect windowID currentComp currentWire wires io)

  SetEditType newEditType _ state io
    = ({state & editType = newEditType}, newIO)
  where
    newIO = ChangeDialog toolbox1ID
             [DisableDialogItems [GetItemID newEditType],
              EnableDialogItems [GetItemID state.editType]
             ] io

  ////////// GetItemID
  // Returns the ID for the dialog button corresponding to the edit type.
  GetItemID :: !EditType -> DialogItemId
  GetItemID StartTerminalDraw = startTermID
  GetItemID EndTerminalDraw   = endTermID
  GetItemID TensorEDraw       = tensorEID
  GetItemID TensorIDraw       = tensorIID
  GetItemID SumEDraw          = sumEID
  GetItemID SumIDraw          = sumIID
  GetItemID LollyDraw         = lollyID
  GetItemID UnitIDraw         = unitIID
  GetItemID UnitELDraw        = unitELID
  GetItemID UnitERDraw        = unitERID
  GetItemID CounitEDraw       = counitEID
  GetItemID CounitILDraw      = counitILID
  GetItemID CounitIRDraw      = counitIRID
  GetItemID BoxDraw           = boxID
  GetItemID GenericDraw       = genericID
  GetItemID WireDraw          = drawWireID
  GetItemID Selection         = select1ID

  ////////// SetProofEditType
  // Similar to SetEditType, except that it is called in ProofMode and must also make sure
  // that if the new edit type is Rewiring and something is selected, it stays selected
  // if and only if it is a thinning link.
  SetProofEditType :: !EditType DialogInfo ProgState IO -> (ProgState, IO)
  SetProofEditType Rewiring _ state=:{itemSelected=True, currentComp, currentWire, currentWindow, windowInfo, nextID}
                   io
    | (currentWire==(-1)) && (IsThinningLink currentComp)
        = ({state & editType = Rewiring}, newIO1)
    | otherwise
        = ({state & itemSelected = False, editType = Rewiring}, newIO2)
  where
    {windowID, wires} = GetWindowState currentWindow windowInfo

    newIO1 = seq [ChangeDialog toolbox2ID [DisableDialogItems [rewireID], EnableDialogItems [select2ID]],
                  DisableMenus [doRewritesID]
                 ] io
    newIO2 = seq [ChangeDialog toolbox2ID [DisableDialogItems [rewireID], EnableDialogItems [select2ID]],
                  DisableMenus [doRewritesID],
                  DrawInWindow windowID [SetPenMode XorMode : DrawSelection currentComp currentWire wires]
                 ] io

  SetProofEditType Rewiring _ state=:{nextID} io
    = ({state & editType = Rewiring}, newIO)
  where
    newIO = seq [ChangeDialog toolbox2ID [DisableDialogItems [rewireID], EnableDialogItems [select2ID]]] io

  SetProofEditType Selection _ state io
    = ({state & editType = Selection}, newIO)
  where
    newIO = ChangeDialog toolbox2ID [DisableDialogItems [select2ID], EnableDialogItems [rewireID]] io

  /////////////////////////////// *** Window Functions *** ///////////////////////////

  ////////// HandleMouse
  // The mouse event handler for the circuit windows.
  HandleMouse :: Window MouseState ProgState IO -> (ProgState, IO)

//***  // Display the type on a wire or connection.
//***  HandleMouse currentWindow (mousePos, ButtonDoubleDown, _) state=:{editType=Selection, windowInfo} io
//***    | selectionMade = (state, DisplayNotice (WireTypeString wireType) io)
//***    | otherwise     = (state, io)
//***  where
//***    ({circuit, wires}) = GetWindowState currentWindow windowInfo
//***    (selectionMade, wireType) = GetSelectedType mousePos circuit wires

  // First connection point for wire
  HandleMouse currentWindow (mousePos, ButtonDown, _) state=:{editType=WireDraw, windowInfo} io
      // VVV An unconnected connection point was not selected
    | IsNotConnected connectInfo  = ({state & currentWindow=currentWindow}, io)
    | otherwise                   = (newState, newIO)
  where
    {windowID, circuit} = GetWindowState currentWindow windowInfo
    (connectionPoint, connectInfo) = GetConnectionPoint mousePos circuit
    newState = {state & currentWindow = currentWindow,
                        drawMode = DrawingWire,
                        connectFrom = connectInfo,
                        startPos = connectionPoint,
                        lastPos = connectionPoint}
    newIO = DrawInWindow windowID [SetPenMode XorMode, DrawLine (connectionPoint, connectionPoint)] io

  // Stretching a new wire with its initial connection at startPos.
  HandleMouse currentWindow (mousePos, ButtonStillDown, _) state=:{drawMode=DrawingWire, windowInfo, startPos, lastPos} io
    = ({state & lastPos = mousePos}, newIO)
  where
    {windowID} = GetWindowState currentWindow windowInfo
    newIO = DrawInWindow windowID [SetPenMode XorMode, DrawLine (startPos, lastPos), DrawLine (startPos, mousePos)] io

  // Second connection point for wire
  HandleMouse currentWindow (mousePos, ButtonUp, _)
              state=:{drawMode=DrawingWire, windowInfo, connectFrom, startPos, lastPos}
              io1
    | (IsNotConnected connection)  // An unconnected connection point was not selected
        = ({state & drawMode=NotDrawing}, oldIO1)
    | (not goodConnection)
        = ({state & drawMode=NotDrawing}, oldIO2)
    | otherwise
        = ({state & windowInfo=newWindowInfo, drawMode=NotDrawing}, newIO)
  where
    (windowState=:{windowID, circuit, wires, nextWireID=wireID}) = GetWindowState currentWindow windowInfo
    newWindowInfo
      = PutWindowState currentWindow {windowState & circuit=newCircuit, wires=newWires, nextWireID=wireID+1} windowInfo

    (connectionPoint, connection) = GetConnectionPoint mousePos circuit
    oldIO1 = DrawInWindow windowID [SetPenMode XorMode, DrawLine (startPos, lastPos)] io1
    (goodConnection, newCircuit, newWires, io2)
      = AddWire connectFrom connection (startPos, connectionPoint) circuit wires wireID io1
    oldIO2 = DrawInWindow windowID [SetPenMode XorMode, DrawLine (startPos, lastPos)] io2
    newIO = DrawInWindow windowID
                         [SetPenMode XorMode, DrawLine (startPos, lastPos), DrawLine (startPos, connectionPoint)]
                         io2

  // Setting first corner of a new box
  HandleMouse currentWindow (mousePos, ButtonDown, _) state=:{editType=BoxDraw, windowInfo} io
    | badPosition = ({state & currentWindow=currentWindow}, io)
    | otherwise
        = ({state & currentWindow=currentWindow, drawMode=DrawingBox, startPos=mousePos, lastPos=mousePos}, newIO)
  where
    (windowState=:{windowID, circuit}) = GetWindowState currentWindow windowInfo

    // The box's corner can't be on top of any part of a component.
    badPosition = any ((any (IsInRectangle mousePos)) o ComponentRectangles) circuit
    newIO       = DrawInWindow windowID [SetPenMode XorMode, DrawRectangle (mousePos, mousePos)] io

  // Setting the border size of a new box
  HandleMouse currentWindow (mousePos, ButtonStillDown, _) state=:{drawMode=DrawingBox, startPos, lastPos, windowInfo} io
    = ({state & lastPos=mousePos}, newIO)
  where
    {windowID} = GetWindowState currentWindow windowInfo
    newIO = DrawInWindow windowID [SetPenMode XorMode, DrawRectangle (startPos, lastPos),
                                   DrawRectangle (startPos, mousePos)
                                  ] io

  // Setting the opposite corner of a new box
  HandleMouse currentWindow (mousePos, ButtonUp, _)
              state=:{drawMode=DrawingBox, startPos, lastPos, windowInfo} io
    | (TooSmallBox boxRectangle) || badPosition || (not boxInserted)
          // Fails if the box is too small, overlaps part of another component or introduces a cycle into the circuit.
        = ({state & drawMode=NotDrawing}, oldIO)
    | otherwise
        = ({state & drawMode=NotDrawing, windowInfo=newWindowInfo}, newIO)
  where
    (windowState=:{windowID, circuit, compID, nextVar}) = GetWindowState currentWindow windowInfo
    // boxRectangle specified by top-left and bottom-right corners
    boxRectangle           = NormalizeRectangle (startPos, mousePos)
    (inComps, newCircuit1) = GetInternalComponents boxRectangle circuit
    newComp                = {spec=Box inComps, id=compID, inputs=[NewConnection (Var (nextVar+1))],
                              outputs=[NewConnection (Var nextVar), NewConnection (Then (Var nextVar, Var (nextVar+1)))],
                              pos=(RCT boxRectangle)}
    badPosition            = OverlapCheck newComp newCircuit1
    oldIO                  = DrawInWindow windowID [SetPenMode XorMode, DrawRectangle (startPos, lastPos)] io
    (boxInserted, newCircuit2) = InsertComponent newComp newCircuit1
    newCompID              = compID + 1
    newWindowInfo
      = PutWindowState currentWindow {windowState & circuit=newCircuit2, compID=newCompID, nextVar = nextVar+2} windowInfo
    newIO                  = DrawInWindow windowID [SetPenMode XorMode, DrawRectangle (startPos, lastPos)
                                                    : DrawComponent {newComp & spec=Box []}] io

  // Selection when a box is already selected (in Edit Mode) (possibly to resize the box)
  HandleMouse currentWindow (mousePos, ButtonDown, _)
              state=:{editMode=EditingCircuit, editType=Selection, itemSelected=True,
                      currentComp=:{spec=Box boxCircuit, id, inputs, outputs, pos}, currentWire=(-1),
                      currentWindow=lastWindow, windowInfo}
              io
    | not (SameWindow currentWindow lastWindow)  // Cannot be reselection of the box in this case
        = if selectionMade
             (if (wireID==(-1))
                 ({compSelectionState & currentWindow=currentWindow}, reselectionIO1)
                 ({wireSelectionState & currentWindow=currentWindow},
                  EnableMenuItems [setTypeID, removeTypeID] reselectionIO1
                 )
             )
             ({state & currentWindow=currentWindow, itemSelected=False}, Deselect lastWindowID currentComp (-1) [] io)
    | IsInRectangle mousePos (TopLeftRectangle compRect)
        = ({state & drawMode=SizeBox, sizeDir=TopLeft, startPos=mousePos, lastPos=mousePos, oldComp=currentComp}, io)
    | IsInRectangle mousePos (TopRightRectangle compRect)
        = ({state & drawMode=SizeBox, sizeDir=TopRight, startPos=mousePos, lastPos=mousePos, oldComp=currentComp}, io)
    | IsInRectangle mousePos (BottomLeftRectangle compRect)
        = ({state & drawMode=SizeBox, sizeDir=BottomLeft, startPos=mousePos, lastPos=mousePos, oldComp=currentComp}, io)
    | IsInRectangle mousePos (BottomRightRectangle compRect)
        = ({state & drawMode=SizeBox, sizeDir=BottomRight, startPos=mousePos, lastPos=mousePos, oldComp=currentComp}, io)
    | selectionMade
        = if (wireID==(-1))
             (compSelectionState, reselectionIO2)
             (wireSelectionState, EnableMenuItems [setTypeID, removeTypeID] reselectionIO2)
    | otherwise
        = ({state & itemSelected=False}, Deselect currentWindowID currentComp (-1) [] io)
  where
    ({windowID=currentWindowID, circuit, wires}) = GetWindowState currentWindow windowInfo
    ({windowID=lastWindowID})                    = GetWindowState lastWindow windowInfo

    currentComp = {spec=Box boxCircuit, id=id, inputs=inputs, outputs=outputs, pos=pos}
    compRect = ComponentRectangle currentComp
    (selectionMade, wireID, selectedComp) = GetSelection mousePos circuit wires

    compSelectionState = {state & drawMode    = MoveSelection,
                                  currentComp = selectedComp,
                                  currentWire = -1,
                                  lastPos     = mousePos,
                                  oldComp     = selectedComp
                         }
    wireSelectionState = {state & drawMode    = NotDrawing,
                                  currentComp = selectedComp,
                                  currentWire = wireID
                         }
    reselectionIO1 = seq [DrawInWindow lastWindowID [SetPenMode XorMode : DrawSelection currentComp (-1) []],
                          DrawInWindow currentWindowID [SetPenMode XorMode : DrawSelection selectedComp wireID wires]
                         ] io
    reselectionIO2 = DrawInWindow currentWindowID
                                  [SetPenMode XorMode :
                                   ((DrawSelection currentComp (-1) []) ++
                                    (DrawSelection selectedComp wireID wires)
                                   )
                                  ]
                                  io

  // Selection when a box is already selected (in Proof Mode) (possibly to resize the box)
  HandleMouse currentWindow (mousePos, ButtonDown, _)
              state=:{editMode=ProofMode, editType=Selection, itemSelected=True,
                      currentComp=:{spec=Box boxCircuit, id, inputs, outputs, pos}, currentWire=(-1),
                      currentWindow=lastWindow, windowInfo, nextID}
              io
    | not (SameWindow currentWindow lastWindow)
        = if selectionMade
             (if (wireID==(-1))
                 (if (IsThinningLink selectedComp)
                     ({compSelectionState & currentWindow=currentWindow}, EnableMenus [flipLinkMenuID] newIO1)
                     (if (IsStartTerminal selectedComp)
                         ({compSelectionState & currentWindow=currentWindow, terminalBounds=inputBounds}, newIO1)
                         (if (IsEndTerminal selectedComp)
                             ({compSelectionState & currentWindow=currentWindow, terminalBounds=outputBounds}, newIO1)
                             ({compSelectionState & currentWindow=currentWindow}, newIO1)
                         )
                     )
                 )
                 ({wireSelectionState & currentWindow=currentWindow},
                  EnableMenus [doRewritesID] newIO1
                 )
             )
             ({state & currentWindow=currentWindow, itemSelected=False},
              DrawInWindow lastWindowID [SetPenMode XorMode : DrawSelection currentComp (-1) []] io
             )
    | IsInRectangle mousePos (TopLeftRectangle compRect)
        = ({state & drawMode=SizeBox, sizeDir=TopLeft, startPos=mousePos, lastPos=mousePos, oldComp=currentComp}, io)
    | IsInRectangle mousePos (TopRightRectangle compRect)
        = ({state & drawMode=SizeBox, sizeDir=TopRight, startPos=mousePos, lastPos=mousePos, oldComp=currentComp}, io)
    | IsInRectangle mousePos (BottomLeftRectangle compRect)
        = ({state & drawMode=SizeBox, sizeDir=BottomLeft, startPos=mousePos, lastPos=mousePos, oldComp=currentComp}, io)
    | IsInRectangle mousePos (BottomRightRectangle compRect)
        = ({state & drawMode=SizeBox, sizeDir=BottomRight, startPos=mousePos, lastPos=mousePos, oldComp=currentComp}, io)
    | selectionMade
        = if (wireID==(-1))
             (if (IsThinningLink selectedComp)
                 (compSelectionState, EnableMenus [flipLinkMenuID] newIO2)
                 (if (IsStartTerminal selectedComp)
                     ({compSelectionState & terminalBounds=inputBounds}, newIO2)
                     (if (IsEndTerminal selectedComp)
                         ({compSelectionState & terminalBounds=outputBounds}, newIO2)
                         (compSelectionState, newIO2)
                     )
                 )
             )
             (wireSelectionState, EnableMenus [doRewritesID] newIO2)
    | otherwise
        = ({state & itemSelected=False},
           DrawInWindow currentWindowID [SetPenMode XorMode : DrawSelection currentComp (-1) []] io
          )
  where
    ({windowID=currentWindowID, circuit, wires, inputPairs, outputPairs, windowDomain=(_,(maxX,_))})
      = GetWindowState currentWindow windowInfo
    ({windowID=lastWindowID}) = GetWindowState lastWindow windowInfo

    currentComp = {spec=Box boxCircuit, id=id, inputs=inputs, outputs=outputs, pos=pos}
    compRect = ComponentRectangle currentComp

    (selectionMade, wireID, selectedComp) = GetSelection mousePos circuit wires
    inputBounds  = GetBounds (hd selectedComp.outputs).cWireID inputPairs maxX
    outputBounds = GetBounds (hd selectedComp.inputs).cWireID outputPairs maxX

    compSelectionState = {state & drawMode    = MoveSelection,
                                  currentComp = selectedComp,
                                  currentWire = -1,
                                  lastPos     = mousePos,
                                  oldComp     = selectedComp
                         }
    wireSelectionState = {state & drawMode    = NotDrawing,
                                  currentComp = selectedComp,
                                  currentWire = wireID,
                                  startPos    = mousePos
                         }
    newIO1 = seq [DrawInWindow lastWindowID [SetPenMode XorMode : DrawSelection currentComp (-1) []],
                  DrawInWindow currentWindowID [SetPenMode XorMode : DrawSelection selectedComp wireID wires]
                 ] io
    newIO2 = DrawInWindow currentWindowID
                          [SetPenMode XorMode :
                           ((DrawSelection currentComp (-1) []) ++
                            (DrawSelection selectedComp wireID wires)
                           )
                          ]
                          io

  // Sizing a box
  HandleMouse currentWindow (mousePos, ButtonStillDown, _)
              state=:{drawMode=SizeBox, sizeDir, lastPos, currentComp, windowInfo} io
    | TooSmallBox boxRect = (state, io)  // Doesn't allow the box to become too small in sizing.
    | otherwise           = ({state & lastPos=mousePos, currentComp=newComp}, sizedIO)
  where
    {windowID} = GetWindowState currentWindow windowInfo

    (newComp=:{pos=RCT boxRect}) = {currentComp & pos=(ResizeBox sizeDir currentComp.pos lastPos mousePos)}
    sizedIO = DrawInWindow windowID
                           [SetPenMode XorMode :
                            ((DrawSelection currentComp (-1) []) ++ (DrawComponent {currentComp & spec=Box []}) ++
                             (DrawSelection newComp (-1) []) ++ (DrawComponent {newComp & spec=Box []}))
                           ] io

  // Finished sizing box (while in Edit Mode)
  HandleMouse currentWindow (mousePos, ButtonUp, _)
              state=:{editMode=EditingCircuit, drawMode=SizeBox, sizeDir, lastPos, currentComp, oldComp, windowInfo} io
    | TooSmallBox newCompRect  // Uses the last size of the box that was big enough in this case.
        = if newBox1OK  // Sizing a box might result in a cycle in the circuit.
             ({state & drawMode=NotDrawing, windowInfo=newWindowInfo1, currentComp=newBox1}, sizedIO1)
             ({state & drawMode=NotDrawing, currentComp=oldComp}, undoneIO)
    | newBox2OK  // Sizing a box might result in a cycle in the circuit.
        = ({state & drawMode=NotDrawing, windowInfo=newWindowInfo2, currentComp=newBox2}, sizedIO2)
    | otherwise
        = ({state & drawMode=NotDrawing, currentComp=oldComp}, undoneIO)
  where
    (windowState=:{windowID, circuit, wires}) = GetWindowState currentWindow windowInfo
    newWindowInfo1 = PutWindowState currentWindow {windowState & circuit=newCircuit1, wires=newWires1} windowInfo
    newWindowInfo2 = PutWindowState currentWindow {windowState & circuit=newCircuit2, wires=newWires2} windowInfo

    (newComp=:{pos=RCT newCompRect}) = {currentComp & pos=(ResizeBox sizeDir currentComp.pos lastPos mousePos)}
    (newBox1OK, newBox1, newCircuit1, newWires1, redraws1) = ChangeSizedBox oldComp currentComp circuit wires
    (newBox2OK, newBox2, newCircuit2, newWires2, redraws2) = ChangeSizedBox oldComp newComp circuit wires
    sizedIO1 = DrawInWindow windowID [SetPenMode XorMode : redraws1] io
    sizedIO2 = DrawInWindow windowID
                            [SetPenMode XorMode :
                             ((DrawSelection currentComp (-1) []) ++ (DrawComponent {currentComp & spec=Box []}) ++
                              (DrawSelection newComp (-1) []) ++ (DrawComponent {newComp & spec=Box []}) ++
                              redraws2
                             )
                            ] io
    undoneIO = DrawInWindow windowID
                            [SetPenMode XorMode :
                             ((DrawSelection currentComp (-1) []) ++ (DrawComponent {currentComp & spec=Box []}) ++
                              (DrawSelection oldComp (-1) []) ++ (DrawComponent {oldComp & spec=Box []}))
                            ] io

  // Finished sizing box (while in Proof Mode)
  // Components can be added to or removed from a box circuit's contents by sizing, so the new circuit must
  // be checked for sequentialization as well as cycles.
  HandleMouse currentWindow (mousePos, ButtonUp, _)
              state=:{editMode=ProofMode, drawMode=SizeBox, sizeDir, lastPos, currentComp, oldComp, windowInfo} io
    | TooSmallBox newCompRect  // Uses the last size of the box that was big enough.
        = if newBox1OK
             ({state & drawMode=NotDrawing, windowInfo=newWindowInfo1, currentComp=newBox1}, sizedIO1)
             ({state & drawMode=NotDrawing, currentComp=oldComp}, undoneIO)
    | newBox2OK
        = ({state & drawMode=NotDrawing, windowInfo=newWindowInfo2, currentComp=newBox2}, sizedIO2)
    | otherwise
        = ({state & drawMode=NotDrawing, currentComp=oldComp}, undoneIO)
  where
    (windowState=:{windowID, circuit, wires}) = GetWindowState currentWindow windowInfo
    newWindowInfo1 = PutWindowState currentWindow {windowState & circuit=newCircuit1, wires=newWires1} windowInfo
    newWindowInfo2 = PutWindowState currentWindow {windowState & circuit=newCircuit2, wires=newWires2} windowInfo

    (newComp=:{pos=RCT newCompRect}) = {currentComp & pos=(ResizeBox sizeDir currentComp.pos lastPos mousePos)}

    (newBox1OK, newBox1, newCircuit1, newWires1, redraws1) = ChangeSizedBoxInProof oldComp currentComp circuit wires
    (newBox2OK, newBox2, newCircuit2, newWires2, redraws2) = ChangeSizedBoxInProof oldComp newComp circuit wires

    sizedIO1 = DrawInWindow windowID [SetPenMode XorMode : redraws1] io
    sizedIO2 = DrawInWindow windowID
                            [SetPenMode XorMode :
                             ((DrawSelection currentComp (-1) []) ++ (DrawComponent {currentComp & spec=Box []}) ++
                              (DrawSelection newComp (-1) []) ++ (DrawComponent {newComp & spec=Box []}) ++
                              redraws2
                             )
                            ] io
    undoneIO = DrawInWindow windowID
                            [SetPenMode XorMode :
                             ((DrawSelection currentComp (-1) []) ++ (DrawComponent {currentComp & spec=Box []}) ++
                              (DrawSelection oldComp (-1) []) ++ (DrawComponent {oldComp & spec=Box []}))
                            ] io

  // Selection of some circuit component (while in Edit Mode)
  HandleMouse currentWindow (mousePos, ButtonDown, _)
              state=:{editMode=EditingCircuit, editType=Selection, currentWindow=lastWindow, windowInfo, currentComp,
                      currentWire, itemSelected}
              io
    | SameWindow currentWindow lastWindow
        = if selectionMade
             (if (wireID==(-1))    // Selection is not a wire
                 (if itemSelected
                     (compSelectionState, reselectionIO1)
                     ({compSelectionState & itemSelected=True}, selectionIO)
                 )
                 (if itemSelected
                     (wireSelectionState, EnableMenuItems [setTypeID, removeTypeID] reselectionIO1)
                     ({wireSelectionState & itemSelected=True}, EnableMenuItems [setTypeID, removeTypeID] selectionIO)
                 )
             )
             (if itemSelected
                 ({state & itemSelected=False}, Deselect currentWindowID currentComp currentWire currentWires io)
                 (state, io)
             )
    | selectionMade
        = if (wireID==(-1))    // Selection is not a wire
             (if itemSelected
                 ({compSelectionState & currentWindow=currentWindow}, reselectionIO2)
                 ({compSelectionState & currentWindow=currentWindow, itemSelected=True}, selectionIO)
             )
             (if itemSelected
                 ({wireSelectionState & currentWindow=currentWindow},
                  EnableMenuItems [setTypeID, removeTypeID] reselectionIO2
                 )
                 ({wireSelectionState & currentWindow=currentWindow, itemSelected=True},
                  EnableMenuItems [setTypeID, removeTypeID] selectionIO
                 )
             )
    | otherwise
        = if itemSelected
             ({state & currentWindow=currentWindow, itemSelected=False},
              Deselect lastWindowID currentComp currentWire lastWires io
             )
             ({state & currentWindow=currentWindow}, io)
  where
    ({windowID=currentWindowID, circuit, wires=currentWires}) = GetWindowState currentWindow windowInfo
    ({windowID=lastWindowID, wires=lastWires})                = GetWindowState lastWindow windowInfo

    (selectionMade, wireID, selectedComp) = GetSelection mousePos circuit currentWires
     // VVV The component will move with the mouse until the button is up.
    compSelectionState = {state & drawMode    = MoveSelection,
                                  currentComp = selectedComp,
                                  currentWire = -1,
                                  lastPos     = mousePos,
                                  oldComp     = selectedComp
                         }
    wireSelectionState = {state & drawMode    = NotDrawing,
                                  currentComp = selectedComp,
                                  currentWire = wireID
                         }
    reselectionIO1 = DrawInWindow currentWindowID
                                  [SetPenMode XorMode :
                                   ((DrawSelection currentComp currentWire currentWires) ++
                                    (DrawSelection selectedComp wireID currentWires)
                                   )
                                  ]
                                  io
    reselectionIO2 = seq [DrawInWindow lastWindowID [SetPenMode XorMode : DrawSelection currentComp currentWire lastWires],
                          DrawInWindow currentWindowID [SetPenMode XorMode : DrawSelection selectedComp wireID currentWires]
                         ] io
    selectionIO = seq [EnableMenuItems [deleteID],
                       DrawInWindow currentWindowID [SetPenMode XorMode : DrawSelection selectedComp wireID currentWires]
                      ] io

  // Selection of some circuit component (while in Proof Mode)
  HandleMouse currentWindow (mousePos, ButtonDown, _)
              state=:{editMode=ProofMode, editType=Selection, currentWindow=lastWindow, windowInfo, currentComp,
                      currentWire, itemSelected, nextID}
              io
    | SameWindow currentWindow lastWindow
        = if selectionMade
             (if (wireID==(-1))    // Selection is not a wire
                 (if itemSelected
                     (if (IsThinningLink selectedComp)
                         (compSelectionState,
                          seq [DisableMenus [doRewritesID], EnableMenus [flipLinkMenuID]] reselectionIO1
                         )
                         (if (IsStartTerminal selectedComp)
                             ({compSelectionState & terminalBounds=inputBounds},
                              DisableMenus [flipLinkMenuID, doRewritesID] reselectionIO1
                             )
                             (if (IsEndTerminal selectedComp)
                                 ({compSelectionState & terminalBounds=outputBounds},
                                  DisableMenus [flipLinkMenuID, doRewritesID] reselectionIO1
                                 )
                                 (compSelectionState,
                                  DisableMenus [flipLinkMenuID, doRewritesID] reselectionIO1
                                 )
                             )
                         )
                     )
                     (if (IsThinningLink selectedComp)
                         ({compSelectionState & itemSelected=True}, EnableMenus [flipLinkMenuID] selectionIO)
                         (if (IsStartTerminal selectedComp)
                             ({compSelectionState & itemSelected=True, terminalBounds=inputBounds}, selectionIO)
                             (if (IsEndTerminal selectedComp)
                                 ({compSelectionState & itemSelected=True, terminalBounds=outputBounds}, selectionIO)
                                 ({compSelectionState & itemSelected=True}, selectionIO)
                             )
                         )
                     )
                 )
                 (if itemSelected
                     (wireSelectionState,
                      seq [DisableMenus [flipLinkMenuID], EnableMenus [doRewritesID]] reselectionIO1
                     )
                     ({wireSelectionState & itemSelected=True}, EnableMenus [doRewritesID] selectionIO)
                 )
             )
             (if itemSelected
                 ({state & itemSelected=False}, unselectedIO1)
                 (state, io)
             )
    | selectionMade
        = if (wireID==(-1))    // Selection is not a wire
             (if itemSelected
                 (if (IsThinningLink selectedComp)
                     ({compSelectionState & currentWindow=currentWindow},
                      seq [DisableMenus [doRewritesID], EnableMenus [flipLinkMenuID]]
                          reselectionIO2
                     )
                     (if (IsStartTerminal selectedComp)
                         ({compSelectionState & currentWindow=currentWindow, terminalBounds=inputBounds},
                          DisableMenus [flipLinkMenuID, doRewritesID] reselectionIO2
                         )
                         (if (IsEndTerminal selectedComp)
                             ({compSelectionState & currentWindow=currentWindow, terminalBounds=outputBounds},
                              DisableMenus [flipLinkMenuID, doRewritesID] reselectionIO2
                             )
                             ({compSelectionState & currentWindow=currentWindow},
                              DisableMenus [flipLinkMenuID, doRewritesID] reselectionIO2
                             )
                         )
                     )
                 )
                 (if (IsThinningLink selectedComp)
                     ({compSelectionState & currentWindow=currentWindow, itemSelected=True},
                      EnableMenus [flipLinkMenuID] selectionIO
                     )
                     (if (IsStartTerminal selectedComp)
                         ({compSelectionState & currentWindow=currentWindow, itemSelected=True, terminalBounds=inputBounds},
                          selectionIO
                         )
                         (if (IsEndTerminal selectedComp)
                             ({compSelectionState & currentWindow=currentWindow, itemSelected=True,
                               terminalBounds=outputBounds
                              },
                              selectionIO
                             )
                             ({compSelectionState & currentWindow=currentWindow, itemSelected=True}, selectionIO)
                         )
                     )
                 )
             )
             (if itemSelected
                 ({wireSelectionState & currentWindow=currentWindow},
                  seq [DisableMenus [flipLinkMenuID], EnableMenus [doRewritesID]]
                  reselectionIO2
                 )
                 ({wireSelectionState & currentWindow=currentWindow, itemSelected=True},
                  EnableMenus [doRewritesID] selectionIO
                 )
             )
    | otherwise
        = if itemSelected
             ({state & currentWindow=currentWindow, itemSelected=False}, unselectedIO2)
             ({state & currentWindow=currentWindow}, io)
  where
    ({windowID=currentWindowID, circuit, wires=currentWires, inputPairs, outputPairs, windowDomain=(_,(maxX,_))})
      = GetWindowState currentWindow windowInfo
    ({windowID=lastWindowID, wires=lastWires}) = GetWindowState lastWindow windowInfo

    (selectionMade, wireID, selectedComp) = GetSelection mousePos circuit currentWires
    inputBounds  = GetBounds (hd selectedComp.outputs).cWireID inputPairs maxX
    outputBounds = GetBounds (hd selectedComp.inputs).cWireID outputPairs maxX

     // VVV The component will move with the mouse until the button is up.
    compSelectionState = {state & drawMode    = MoveSelection,
                                  currentComp = selectedComp,
                                  currentWire = -1,
                                  lastPos     = mousePos,
                                  oldComp     = selectedComp
                         }
    wireSelectionState = {state & drawMode    = NotDrawing,
                                  currentComp = selectedComp,
                                  currentWire = wireID,
                                  startPos    = mousePos
                         }
    reselectionIO1 = DrawInWindow currentWindowID
                                  [SetPenMode XorMode :
                                   ((DrawSelection currentComp currentWire currentWires) ++
                                    (DrawSelection selectedComp wireID currentWires)
                                   )
                                  ]
                                  io
    reselectionIO2 = seq [DrawInWindow lastWindowID [SetPenMode XorMode : DrawSelection currentComp currentWire lastWires],
                          DrawInWindow currentWindowID [SetPenMode XorMode : DrawSelection selectedComp wireID currentWires]
                         ] io
    selectionIO  = DrawInWindow currentWindowID [SetPenMode XorMode : DrawSelection selectedComp wireID currentWires] io
    unselectedIO1
      = seq [DisableMenus [flipLinkMenuID, doRewritesID],
             DrawInWindow currentWindowID [SetPenMode XorMode : DrawSelection currentComp currentWire currentWires]
            ] io
    unselectedIO2
      = seq [DisableMenus [flipLinkMenuID, doRewritesID],
             DrawInWindow lastWindowID [SetPenMode XorMode : DrawSelection currentComp currentWire lastWires]
            ] io

  // Dragging a circuit component (in Edit Mode)
  HandleMouse currentWindow ((mouseX, mouseY), ButtonStillDown, _)
              state=:{editMode=EditingCircuit, drawMode=MoveSelection, currentComp, lastPos=(lastX,lastY), windowInfo} io
    = ({state & currentComp=newComp, lastPos=(mouseX, mouseY)}, newIO)
  where
    {windowID} = GetWindowState currentWindow windowInfo

    newComp = MoveComponent offset currentComp
    offset = (mouseX - lastX, mouseY - lastY)
    newIO = DrawInWindow windowID
                         [SetPenMode XorMode :
                          ((DrawSelection currentComp (-1) []) ++
                           (DrawComponent currentComp)         ++
                           (DrawSelection newComp (-1) [])     ++
                           (DrawComponent newComp)
                          )
                         ]
                         io

  // Dragging a circuit component (in Proof Mode)
  // Terminals cannot be moved horizontally past the terminals on either side of them, since this would
  // change the order of inputs or outputs.
  HandleMouse currentWindow ((mouseX, mouseY), ButtonStillDown, _)
              state=:{drawMode=MoveSelection, currentComp, lastPos=(lastX,lastY), terminalBounds=(minX,maxX), windowInfo} io
    | (IsStartTerminal currentComp) || (IsEndTerminal currentComp)
        = if ((terminalXpos < minX) || (terminalXpos > maxX))
             (state, io)
             ({state & currentComp=newComp, lastPos=(mouseX, mouseY)}, newIO)
    | otherwise
        = ({state & currentComp=newComp, lastPos=(mouseX, mouseY)}, newIO)
  where
    {windowID} = GetWindowState currentWindow windowInfo

    terminalXpos = TerminalXPosition newComp
    newComp = MoveComponent offset currentComp
    offset = (mouseX - lastX, mouseY - lastY)
    newIO = DrawInWindow windowID
                         [SetPenMode XorMode :
                          ((DrawSelection currentComp (-1) []) ++
                           (DrawComponent currentComp)         ++
                           (DrawSelection newComp (-1) [])     ++
                           (DrawComponent newComp)
                          )
                         ]
                         io

  // Placing a moved circuit component (while in Edit Mode)
  // Has to check that no cycles are introduced if the component is moved into or out of a box.
  HandleMouse currentWindow ((mouseX, mouseY), ButtonUp, _)
              state=:{editMode=EditingCircuit, drawMode=MoveSelection, currentComp, windowInfo, lastPos=(lastX,lastY),
                      oldComp}
              io
    | badPosition || (not compChanged)
        = ({state & drawMode=NotDrawing, currentComp=oldComp}, undoneIO)
    | otherwise
        = ({state & drawMode=NotDrawing, windowInfo=newWindowInfo}, newIO)
  where
    (windowState=:{windowID, circuit, wires}) = GetWindowState currentWindow windowInfo
    newWindowInfo = PutWindowState currentWindow {windowState & circuit=newCircuit, wires=newWires} windowInfo

    // The component shouldn't overlap part of any other component.
    badPosition = OverlapCheck newComp (RemoveWholeComp oldComp circuit)
    newComp = MoveComponent offset currentComp
    offset = (mouseX - lastX, mouseY - lastY)
    undoneIO = DrawInWindow windowID
                            [SetPenMode XorMode :
                             ((DrawSelection currentComp (-1) []) ++
                              (DrawComponent currentComp)   ++
                              (DrawSelection oldComp (-1) [])     ++
                              (DrawComponent oldComp)
                             )
                            ]
                            io
    (compChanged, newCircuit) = ChangeComponent (ComponentRectangle oldComp) newComp circuit
    (newWires, redraws)   = MoveWires newComp wires  // Changes the endpoints of the component's wires.
    newIO = DrawInWindow windowID
                         [SetPenMode XorMode :
                          ((DrawSelection currentComp (-1) []) ++
                           (DrawComponent currentComp)   ++
                           (DrawSelection newComp (-1) [])     ++
                           (DrawComponent newComp)             ++
                           redraws
                          )
                         ]
                         io

  // Placing a moved circuit component (while in Proof Mode)
  // If a component is moved in or out of a box (or both), the new circuit must be checked for sequentialization
  // as well as cycles.  Also, terminals cannot be moved horizontally past the terminals on either side of them,
  // since this would change the order of inputs or outputs.
  HandleMouse currentWindow ((mouseX, mouseY), ButtonUp, _)
              state=:{editMode=ProofMode, drawMode=MoveSelection, currentComp, windowInfo, lastPos=(lastX,lastY),
                      oldComp, terminalBounds=(minX,maxX)}
              io
    | (IsStartTerminal currentComp) || (IsEndTerminal currentComp)
        = if ((terminalXpos < minX) || (terminalXpos > maxX))
             (if (badPosition1 || (not compChanged1))
                 ({state & drawMode=NotDrawing, currentComp=oldComp}, undoneIO)
                 ({state & drawMode=NotDrawing, windowInfo=newWindowInfo1}, newIO1)
             )
             (if (badPosition2 || (not compChanged2))
                 ({state & drawMode=NotDrawing, currentComp=oldComp}, undoneIO)
                 ({state & drawMode=NotDrawing, windowInfo=newWindowInfo2}, newIO2)
             )
    | badPosition2 || (not compChanged2)
        = ({state & drawMode=NotDrawing, currentComp=oldComp}, undoneIO)
    | otherwise
        = ({state & drawMode=NotDrawing, windowInfo=newWindowInfo3}, newIO2)
  where
    (windowState=:{windowID, circuit, wires, inputPairs, outputPairs})
      = GetWindowState currentWindow windowInfo
    newWindowInfo1
      = PutWindowState currentWindow
                       {windowState & circuit=newCircuit1, wires=newWires1, inputPairs=newInputPairs1,
                        outputPairs=newOutputPairs1
                       } windowInfo
    newWindowInfo2
      = PutWindowState currentWindow
                       {windowState & circuit=newCircuit2, wires=newWires2, inputPairs=newInputPairs2,
                        outputPairs=newOutputPairs2
                       } windowInfo
    newWindowInfo3 = PutWindowState currentWindow {windowState & circuit=newCircuit2, wires=newWires2} windowInfo

    offset = (mouseX - lastX, mouseY - lastY)
    newComp = MoveComponent offset currentComp
    terminalXpos = TerminalXPosition newComp

    undoneIO = DrawInWindow windowID
                            [SetPenMode XorMode :
                             ((DrawSelection currentComp (-1) []) ++
                              (DrawComponent currentComp)   ++
                              (DrawSelection oldComp (-1) [])     ++
                              (DrawComponent oldComp)
                             )
                            ]
                            io
    // The component shouldn't overlap part of any other component.
    badPosition1 = OverlapCheck currentComp (RemoveWholeComp oldComp circuit)
    (compChanged1, newCircuit1) = ChangeComponentInProof (ComponentRectangle oldComp) currentComp circuit
    (newWires1, redraws1)       = MoveWires currentComp wires  // Repositions the ends of the component's wires.
    (newInputPairs1, newOutputPairs1) = ChangePositionPair currentComp inputPairs outputPairs

    // The component shouldn't overlap part of any other component.
    badPosition2 = OverlapCheck newComp (RemoveWholeComp oldComp circuit)
    (compChanged2, newCircuit2) = ChangeComponentInProof (ComponentRectangle oldComp) newComp circuit
    (newWires2, redraws2)       = MoveWires newComp wires  // Repositions the ends of the component's wires.
    (newInputPairs2, newOutputPairs2) = ChangePositionPair newComp inputPairs outputPairs

    newIO1 = DrawInWindow windowID [SetPenMode XorMode : redraws1] io

    newIO2 = DrawInWindow windowID
                          [SetPenMode XorMode :
                           ((DrawSelection currentComp (-1) []) ++
                            (DrawComponent currentComp)   ++
                            (DrawSelection newComp (-1) [])     ++
                            (DrawComponent newComp)             ++
                            redraws2
                           )
                          ]
                          io

  // Selection of a new wire for the currently selected thinning link (editType=Rewiring possible only in ProofMode)
  HandleMouse currentWindow (mousePos, ButtonDown, _)
              state=:{editType=Rewiring, itemSelected=True, currentComp, currentWindow=lastWindow, windowInfo} io
    | not (SameWindow currentWindow lastWindow)
        = if (compSelected && (IsThinningLink selectedComp))
             ({state & currentWindow=currentWindow, currentComp=selectedComp}, reselectionIO)
             ({state & currentWindow=currentWindow, itemSelected=False}, deselectionIO)
    | IsNotError returnCode
        = ({state & windowInfo=newWindowInfo, currentComp=newComp}, newIO)
    | otherwise
        = ReturnVal returnCode
  where
    (windowState=:{windowID=currentWindowID, circuit, wires, inputPairs, outputPairs})
      = GetWindowState currentWindow windowInfo
    ({windowID=lastWindowID}) = GetWindowState lastWindow windowInfo

    newWindowInfo = PutWindowState
                      currentWindow
                      {windowState & circuit=newCircuit, wires=newWires, inputPairs=inputPairs2, outputPairs=outputPairs2}
                      windowInfo

    (compSelected, selectedComp) = GetSelectedComp mousePos circuit

    reselectionIO = seq [DrawInWindow lastWindowID [SetPenMode XorMode : DrawSelection currentComp (-1) []],
                         DrawInWindow currentWindowID [SetPenMode XorMode : DrawSelection selectedComp (-1) []]
                        ] io

    deselectionIO = seq [DisableMenus [flipLinkMenuID],
                         DrawInWindow lastWindowID [SetPenMode XorMode : DrawSelection currentComp (-1) []]
                        ] io

    (returnCode, newCircuit, newWires, newComp, redraws, inputPairs2, outputPairs2)
      = Rewire currentComp mousePos circuit wires inputPairs outputPairs

    newIO = DrawInWindow currentWindowID
                         [SetPenMode XorMode :
                          (redraws ++ (DrawSelection currentComp (-1) []) ++ (DrawSelection newComp (-1) []))
                         ] io

    ReturnVal BadWire
      = (state, DisplayNotice "The selected wire is connected to this thinning link." io)
    ReturnVal NewCompOverlaps
      = (state, DisplayNotice "The selected position overlaps another component." io)
    ReturnVal BadRewiring
      = (state, DisplayNotice "This is not an empire move." io)
    ReturnVal NoSelection
      = if (compSelected && (IsThinningLink selectedComp))
           ({state & currentComp=selectedComp}, newIO)
           (state, io)
    where
      (compSelected, selectedComp) = GetSelectedComp mousePos circuit
      newIO = DrawInWindow currentWindowID
                           [SetPenMode XorMode :
                            ((DrawSelection currentComp (-1) []) ++ (DrawSelection selectedComp (-1) []))
                           ] io

  // Selection of a thinning link (editType=Rewiring possible only in ProofMode)
  HandleMouse currentWindow (mousePos, ButtonDown, _) state=:{editType=Rewiring, windowInfo} io
    | (compSelected && (IsThinningLink selectedComp))
        = ({state & currentWindow=currentWindow, itemSelected=True, currentWire=(-1), currentComp=selectedComp}, newIO)
    | otherwise
        = ({state & currentWindow=currentWindow}, io)
  where
    {windowID, circuit} = GetWindowState currentWindow windowInfo

    (compSelected, selectedComp) = GetSelectedComp mousePos circuit
    newIO = seq [EnableMenus [flipLinkMenuID],
                 DrawInWindow windowID [SetPenMode XorMode : DrawSelection selectedComp (-1) []]
                ] io

  // Adding a generic component
  HandleMouse currentWindow (mousePos, ButtonDown, _) state=:{editType=GenericDraw, windowInfo, nextID, generics} io
    | isEmpty generics = ({state & currentWindow=currentWindow}, DisplayNotice "There are no other components defined." io)
    | otherwise        = OpenModalDialog getGenericDlg {state & currentWindow=currentWindow}
                                         (seq [DisableAllWindows windowInfo, DisableMenuSystem] io)
  where
    getGenericDlg = CommandDialog getGenericID "Select Component" [] ok1ID
                     [StaticText text1ID Left "Select the component:  ",
                      DialogPopUp genericListID (RightTo text1ID) Able nextID (MakeGenericMenu generics nextID),
                      DialogButton ok1ID Left "Continue" Able PlaceGenericComp,
                      DialogButton cancel1ID (RightTo ok1ID) "Cancel" Able CancelGetGeneric
                     ]

    // Makes a menu of all the current user-defined components.
    MakeGenericMenu [(Generic name _ _, _, _) : generics] id
      = [RadioItem id name Able DoNothing : MakeGenericMenu generics (id+1)]
    MakeGenericMenu [] _ = []

    DoNothing :: DialogInfo (DialogState ProgState IO) -> (DialogState ProgState IO)
    DoNothing _ dlgState = dlgState

    PlaceGenericComp :: DialogInfo ProgState IO -> (ProgState, IO)
    PlaceGenericComp dlgInfo state=:{generics, currentWindow, windowInfo} io
      | badPosition
          = (state,
             seq [CloseDialog getGenericID, EnableAllWindows windowInfo, EnableMenuSystem,
//*^*                  DisplayNotice ("The id is " +++ (toString genericMenuID) +++ "."),  //*^* debug check
                  DisplayNotice "This position overlaps another component"
                 ] io
            )
      | otherwise
          = ({state & windowInfo=newWindowInfo},
             seq [CloseDialog getGenericID, EnableAllWindows windowInfo, EnableMenuSystem,
//*^*                  DisplayNotice ("The id is " +++ (toString genericMenuID) +++ "."),  //*^* debug check
                  DrawInWindow windowID [SetPenMode XorMode : DrawComponent newComp]
                 ] io
            )
    where
      genericMenuID = GetSelectedRadioItemId genericListID dlgInfo
      // Gets info about the selected component, or the first component if 0 is returned.
      newGeneric    = generics ! (max 0 (genericMenuID - nextID))

      (windowState=:{circuit, compID, nextVar, windowID}) = GetWindowState currentWindow windowInfo

      // Makes a component from the component information, including positioning it and setting its input and output types.
      (newComp, newNextVar) = MakeGenericComp newGeneric compID nextVar mousePos
      badPosition           = OverlapCheck newComp circuit
      (_, newCircuit)       = InsertComponent newComp circuit

      newWindowInfo
        = PutWindowState currentWindow {windowState & circuit=newCircuit, compID=compID+1, nextVar=newNextVar} windowInfo

    CancelGetGeneric :: DialogInfo ProgState IO -> (ProgState, IO)
    CancelGetGeneric _ state=:{windowInfo} io
      = (state, seq [CloseDialog getGenericID, EnableAllWindows windowInfo, EnableMenuSystem] io)

  // Adding a circuit component
  HandleMouse currentWindow (mousePos, ButtonDown, _) state=:{editMode=EditingCircuit, editType, windowInfo} io
    | badPosition   = ({state & currentWindow=currentWindow}, io)
    | not noProblem = ({state & currentWindow=currentWindow}, DisplayNotice "Hey!" io)
    | otherwise
        = ({state & currentWindow=currentWindow, windowInfo=newWindowInfo}, newIO)
  where
    (windowState=:{windowID, circuit, compID, nextVar}) = GetWindowState currentWindow windowInfo
    newWindowInfo
      = PutWindowState currentWindow {windowState & circuit=newCircuit, compID=newCompID, nextVar=newNextVar} windowInfo

    (newComponent, newNextVar) = MakeComponent editType compID mousePos nextVar
    badPosition                = OverlapCheck newComponent circuit
    (noProblem, newCircuit)    = InsertComponent newComponent circuit
    newCompID = compID + 1
    newIO = DrawInWindow windowID [SetPenMode XorMode : DrawComponent newComponent] io

  HandleMouse _ _ state io = (state, io)

  ////////// RedrawCircuit
  // The function called for the circuit windows when they need to be redrawn.
  RedrawCircuit :: Window UpdateArea ProgState -> (ProgState, [DrawFunction])
  RedrawCircuit currentWindow _ state=:{currentWindow=lastWindow, windowInfo, itemSelected, currentComp, currentWire}
    | itemSelected && (SameWindow currentWindow lastWindow) = (state, redraws1)
    | otherwise                                             = (state, redraws2)
  where
    {circuit, wires, windowDomain} = GetWindowState currentWindow windowInfo
    redraws1 = [(EraseRectangle windowDomain),
                 SetPenMode XorMode :
                 ((DrawSelection currentComp currentWire wires) ++
                  (flatten (map DrawWire wires)) ++
                  (flatten (map DrawComponent circuit))
                 )
               ]
    redraws2 = [(EraseRectangle windowDomain),
                 SetPenMode XorMode :
                 ((flatten (map DrawWire wires)) ++
                  (flatten (map DrawComponent circuit))
                 )
               ]

  //////////////////// *** Menu Functions *** /////////////////////////

  ////////// Save
  // Saves the circuit (and associated information) in the current window in a file.
  Save :: ProgState IO -> (ProgState, IO)
  Save state=:{files, currentWindow, windowInfo} io1
    | NotWindow currentWindow
        = (state,
           DisplayNotice "No window is currently selected.  Click in a window to select it." io1
          )
    | not confirmed
        = (state, DisplayNotice "Click in the window you want to save before selecting this option." io2)
    | not fileSelected        = ({state & files=files2}, io3)
    | not fileOpened          = ({state & files=files3}, DisplayNotice "Unable to open this file for writing." io3)
    | not fileClosed          = ({state & files=files4}, DisplayNotice "An error occurred in closing the file." io3)
    | otherwise               = ({state & windowInfo=newWindowInfo, files=files4},
                                 ChangeWindowTitle windowID (windowState.windowName +++ ": " +++ fileName) io3
                                )
  where
    (windowState=:{windowID, windowName, circuit, wires, compID, nextWireID, nextVar})
      = GetWindowState currentWindow windowInfo
    (confirmed, io2) = ConfirmCheck
    ConfirmCheck
      | AllOpen windowInfo = Confirm ("Save the circuit in " +++ windowName +++ "?") io1
      | otherwise          = (True, io1)
    (fileSelected, fileName, files2, io3) = SelectOutputFile "" windowState.fileName files io2
    (fileOpened, file1, files3)           = fopen fileName FWriteText files2
    file2                                 = SaveCircuit circuit wires compID nextWireID nextVar file1
    (fileClosed, files4)                  = fclose file2 files3
    newWindowInfo                         = PutWindowState currentWindow {windowState & fileName=fileName} windowInfo

  ////////// Read
  // Reads a circuit and associated information into the current window from a file.
  Read :: ProgState IO -> (ProgState, IO)
  Read state=:{editMode=EditingCircuit, files, currentWindow, windowInfo} io1
    | NotWindow currentWindow = (state,
                                 DisplayNotice "No window is currently selected.  Click in a window to select it." io1
                                )
    | not confirmed           = (state, DisplayNotice "Click in the window you want before selecting this option." io2)
    | not fileSelected        = ({state & files=files2}, io3)
    | not fileOpened          = ({state & files=files3}, DisplayNotice "Unable to open this file for reading." io3)
    | not circuitRead         = ({state & files=files4}, DisplayNotice "Unable to read a circuit from this file." io3)
    | not fileClosed          = ({state & files=files4}, DisplayNotice "An error occurred in closing the file." io3)
    | otherwise               = (newState2, io4)
  where
    (windowState=:{windowID, windowName, circuit}) = GetWindowState currentWindow windowInfo
    (confirmed, io2) = ConfirmCheck
    ConfirmCheck
      | isEmpty circuit = (True, io1)
      | otherwise       = Confirm ("Read into " +++ windowName +++ "?  Its current contents will be erased.") io1
    (fileSelected, fileName, files2, io3) = SelectInputFile files io2
    (fileOpened, file1, files3)           = fopen fileName FReadText files2
    (circuitRead, newCircuit, newWires, newCompID, newNextWireID, newNextVar, file2) = ReadCircuitFile file1
    (fileClosed, files4)                  = fclose file2 files3
    newWindowState = {windowState & fileName=fileName, circuit=newCircuit, wires=newWires, compID=newCompID,
                                    nextWireID=newNextWireID, nextVar=newNextVar
                     }
    newState1 = {state & itemSelected=False, windowInfo=(PutWindowState currentWindow newWindowState windowInfo),
                 files=files4
                }
    (newState2, io4) = DrawInWindowFrame windowID (RedrawCircuit currentWindow) newState1
                         (ChangeWindowTitle windowID (windowName +++ ": " +++ fileName) io3)

  // If a circuit is read in in ProofMode, it must be checked to see if it sequentializes.  If it doesn't,
  // the program goes into EditingCircuit mode.  Otherwise, new input and output position pairs are needed.
  Read state=:{editMode=ProofMode, files, currentWindow, windowInfo} io1
    | NotWindow currentWindow = (state,
                                 DisplayNotice "No window is currently selected.  Click in a window to select it." io1
                                )
    | not confirmed           = (state, DisplayNotice "Click in the window you want before selecting this option." io2)
    | not fileSelected        = ({state & files=files2}, io3)
    | not fileOpened          = ({state & files=files3}, DisplayNotice "Unable to open this file for reading." io3)
    | not circuitRead         = ({state & files=files4}, DisplayNotice "Unable to read a circuit from this file." io3)
    | not fileClosed          = ({state & files=files4}, DisplayNotice "An error occurred in closing the file." io3)
    | not sequentializes      = SetEditMode EditingCircuit newState2 io4
    | otherwise               = (newState2, io4)
  where
    (windowState=:{windowID, windowName, circuit}) = GetWindowState currentWindow windowInfo
    (confirmed, io2) = ConfirmCheck
    ConfirmCheck
      | isEmpty circuit = (True, io1)
      | otherwise       = Confirm ("Read into " +++ windowName +++ "?  Its current contents will be erased.") io1
    (fileSelected, fileName, files2, io3) = SelectInputFile files io2
    (fileOpened, file1, files3)           = fopen fileName FReadText files2
    (circuitRead, newCircuit, newWires, newCompID, newNextWireID, newNextVar, file2) = ReadCircuitFile file1
    (fileClosed, files4)                  = fclose file2 files3
    (sequentializes, sequentCirc) = FILLSequentialize newCircuit
    newInputPairs  = GetInputPositionPairs newWires (hd sequentCirc).inputs
    newOutputPairs = GetOutputPositionPairs newWires (hd sequentCirc).outputs
    newWindowState = {windowState & fileName=fileName, circuit=newCircuit, wires=newWires, compID=newCompID,
                      nextWireID=newNextWireID, nextVar=newNextVar, inputPairs=newInputPairs, outputPairs=newOutputPairs
                     }
    newState1 = {state & itemSelected=False, windowInfo=(PutWindowState currentWindow newWindowState windowInfo),
                 files=files4
                }
    (newState2, io4) = DrawInWindowFrame windowID (RedrawCircuit currentWindow) newState1
                         (ChangeWindowTitle windowID (windowName +++ ": " +++ fileName) io3)

  ////////// OpenProgWindow
  // Opens a new circuit window (shouldn't be called if both windows are open already).
  OpenProgWindow :: ProgState IO -> (ProgState, IO)
  OpenProgWindow state=:{windowInfo} io
    | AllOpen newWindowInfo
        = (newState,
           seq [EnableMenus [editRewritesID, saveRewritesID], EnableMenuItems [saveRewID], DisableMenuItems [openID]] newIO
          )
    | otherwise
        = (newState, newIO)
  where
    newWindow                  = GetClosedWindow windowInfo
    (windowState=:{windowDef}) = GetWindowState newWindow windowInfo
    newWindowInfo = PutWindowState
                      newWindow
                      {windowState & fileName="", windowOpen=True, wires=[], nextWireID=0, compID=0, nextVar=1}
                      windowInfo
    newState = {state & windowInfo=newWindowInfo}
    newIO    = seq [OpenWindows [windowDef], EnableMenuItems [closeID]] io

  ////////// GetClosedWindow
  // Finds a window that isn't open.
  GetClosedWindow :: (WindowState, WindowState) -> Window
  GetClosedWindow ({windowOpen=False},_)  = Window1
  GetClosedWindow (_, {windowOpen=False}) = Window2

  ////////// CloseProgWindow
  // Closes the current window.  Shouldn't be called if no window is open, but might be called when no window is current.
  CloseProgWindow :: ProgState IO -> (ProgState, IO)
  CloseProgWindow state=:{currentWindow, windowInfo} io
    | NotWindow currentWindow = (state, DisplayNotice "No window is selected.  Click in a window to select it." io)
    | not confirmed           = (state, io2)
    | AllClosed newWindowInfo = (newState, DisableMenuItems [closeID] newIO)
    | otherwise               = (newState, newIO)
  where
    (windowState=:{windowName, windowID, circuit}) = GetWindowState currentWindow windowInfo
    newWindowInfo = PutWindowState
                      currentWindow
                      {windowState & fileName="", windowOpen=False, circuit=[], wires=[], nextWireID=0, compID=0, nextVar=1}
                      windowInfo
    (confirmed, io2) = ConfirmCheck
    ConfirmCheck
      | isEmpty circuit = (True, io)
      | otherwise       = Confirm ("Close " +++ windowName +++ "?  Its contents will be lost.") io
    newState = {state & currentWindow=NoWindow, itemSelected=False, windowInfo=newWindowInfo}
    newIO    = seq [CloseWindows [windowID],
                    DisableMenus [editRewritesID, saveRewritesID],
                    DisableMenuItems [saveRewID, deleteID, setTypeID, removeTypeID],
                    CloseDialog rewriteInfoID,
                    EnableMenuItems [openID]
                   ] io2

  ////////// AllClosed
  // Checks whether both circuit windows are closed.
  AllClosed :: (WindowState, WindowState) -> Bool
  AllClosed ({windowOpen=False}, {windowOpen=False}) = True
  AllClosed _ = False

  ////////// SaveNewRewrite
  // Saves a new rewrite (given by the circuits in the two circuit windows - the live wire in the left side
  // should be selected).  Shouldn't be called unless two windows are open.
  SaveNewRewrite :: ProgState IO -> (ProgState, IO)
  SaveNewRewrite state io
    = OpenModalDialog rewriteDialog state io
  where
    rewriteDialog =
      (CommandDialog rewriteInfoID "Save Rewrite" [] ok1ID
       [StaticText text1ID Left "Before creating a rewrite, you should make sure that the right side is",
        StaticText text1ID Left "as compact as possible (excluding terminals).",
        StaticText text1ID Left "",
        StaticText text1ID Left "You must also select the live wire on the left side of the rule.",
        StaticText text1ID Left "(The live wire is the wire that will be selected to indicate where a",
        StaticText text1ID Left "rewrite is to be performed in a circuit.)",
        StaticText text1ID Left "",
        StaticText text1ID Left "Enter the name that should appear in the menu for this rewrite rule:",
        EditText rewriteNameID Left (Inch 1.5) 1 "",
        DialogButton ok1ID Left "Continue" Able ContinueRewrite,
        DialogButton cancel1ID (RightTo ok1ID) "Cancel" Able CancelRewrite
       ]
      )

    ContinueRewrite :: DialogInfo ProgState IO -> (ProgState, IO)
    ContinueRewrite _ state=:{itemSelected=False} io
      = (state, DisplayNotice "Please select the live wire on the left side of the rewrite." io)
    ContinueRewrite _ state=:{currentWire=(-1)} io
      = (state, DisplayNotice "Please select the live wire on the left side of the rewrite." io)
    ContinueRewrite dlgInfo state=:{currentWindow, windowInfo, files, nextID, currentWire, editMode, itemSelected} io
      | rewriteName==""             = (state, DisplayNotice "You must give a name for the rewrite." io)
      | not (IsNotError returnCode) = (state, seq [CloseDialog rewriteInfoID, DisplayNotice (ErrorMessage returnCode)] io)
      | not fileSelected            = ({state & files=files2}, io2)
      | not fileOpened              = ({state & files=files3}, DisplayNotice "Unable to open this file for writing." io2)
      | not fileClosed              = ({state & files=files4}, DisplayNotice "An error occurred closing the file." io2)
      | otherwise
          = ({state & files=files4, nextID=nextID+4},
             seq [AppendMenuItems doRewritesGroupID nextID
                    [MenuItem nextID rewriteName NoKey Able (DoProofRewrite rewrite)],
                  AppendMenuItems viewRewritesGroupID (nextID+1)
                    [MenuItem (nextID+1) rewriteName NoKey Able (DisplayRewrite rewrite)],
                  AppendMenuItems editRewritesGroupID (nextID+2)
                    [MenuItem (nextID+2) rewriteName NoKey Able (EditRewrite rewrite)],
                  AppendMenuItems saveRewritesGroupID (nextID+3)
                    [MenuItem (nextID+3) rewriteName NoKey Able (ResaveRewrite rewriteName nextID)],
                  DisplayNotice ("The rewrite rule has been added to the end of the file using the circuit in " +++
                                 windowName +++ " as the left side of the rule."
                                )
                 ] io2
            )
    where
      rewriteName                           = GetEditText rewriteNameID dlgInfo
      (leftSide, rightSide, nextWireID)     = GetRewriteInfo currentWindow windowInfo
      ({windowName})                        = GetWindowState currentWindow windowInfo
      (returnCode, rewrite)                 = CreateRewrite rewriteName leftSide rightSide currentWire nextWireID
      (fileSelected, fileName, files2, io2) = SelectOutputFile "Save Rewrite In:" "" files (CloseDialog rewriteInfoID io)
      (fileOpened, file1, files3)           = fopen fileName FAppendText files2
      file2                                 = SaveRewrite rewrite file1
      (fileClosed, files4)                  = fclose file2 files3

      ErrorMessage LeftSideNotWired      = "The left side of this rule is not fully connected."
      ErrorMessage RightSideNotWired     = "The right side of this rule is not fully connected."
      ErrorMessage BadLeftSideTerminals  = "There are terminals in the boxes on the left side of this rule."
      ErrorMessage BadRightSideTerminals = "There are terminals in the boxes on the right side of this rule."
      ErrorMessage InputCountMismatch    = "The two sides of the rule do not have the same number of inputs."
      ErrorMessage OutputCountMismatch   = "The two sides of the rule do not have the same number of outputs."
      ErrorMessage NotConnected          = "The left side of the rule must be completely connected."
      ErrorMessage BadLiveWire           = "The live wire must not enter, leave or be inside a box."
      ErrorMessage TypeMismatch          = "The types of the inputs and outputs of the two sides of the rule don't match."

    CancelRewrite :: DialogInfo ProgState IO -> (ProgState, IO)
    CancelRewrite _ state io = (state, CloseDialog rewriteInfoID io)

  ////////// GetRewriteInfo
  // Gets the left side, right side and the next free wire ID on the left side when creating a rewrite
  // (the live wire is the selected wire, and the left side is the current window).
  GetRewriteInfo :: Window (WindowState, WindowState) -> (Circuit, Circuit, WireID)
  GetRewriteInfo Window1 ({circuit=circuit1, nextWireID}, {circuit=circuit2})
    = (circuit1, circuit2, nextWireID)
  GetRewriteInfo Window2 ({circuit=circuit1}, {circuit=circuit2, nextWireID})
    = (circuit2, circuit1, nextWireID)

  ////////// ReadRewritesFile
  // Reads rewrite rules from a file.
  ReadRewritesFile :: ProgState IO -> (ProgState, IO)
  ReadRewritesFile state=:{files} io
    | not fileSelected = ({state & files=files2}, io2)
    | not fileOpened   = ({state & files=files3}, DisplayNotice "Unable to open this file for reading." io2)
    | not noProblem    = ({newState & files=files4}, DisplayNotice "Unable to read all rewrites from this file." newIO)
    | not fileClosed   = ({newState & files=files4}, DisplayNotice "An error occurred in closing the file." newIO)
    | otherwise        = ({newState & files=files4}, DisplayNotice "Finished reading the rewrite file." newIO)
  where
    (fileSelected, fileName, files2, io2) = SelectInputFile files io
    (fileOpened, file1, files3)           = fopen fileName FReadText files2
    (noProblem, file2, newState, newIO)   = ReadRewrites file1 {state & files=files3} io2
    (fileClosed, files4)                  = fclose file2 newState.files

    ReadRewrites file1 state=:{nextID} io
      | done            = (True, file2, state, io)
      | not rewriteRead = (False, file2, state, io)
      | otherwise       = ReadRewrites file2 {state & nextID=nextID+4} newIO
    where
      (done, rewriteRead, rewrite, file2) = ReadRewrite file1
      newIO = seq [AppendMenuItems doRewritesGroupID nextID
                     [MenuItem nextID rewrite.rewriteName NoKey Able (DoProofRewrite rewrite)],
                   AppendMenuItems viewRewritesGroupID (nextID+1)
                     [MenuItem (nextID+1) rewrite.rewriteName NoKey Able (DisplayRewrite rewrite)],
                   AppendMenuItems editRewritesGroupID (nextID+2)
                     [MenuItem (nextID+2) rewrite.rewriteName NoKey Able (EditRewrite rewrite)],
                   AppendMenuItems saveRewritesGroupID (nextID+3)
                     [MenuItem (nextID+3) rewrite.rewriteName NoKey Able (ResaveRewrite rewrite.rewriteName nextID)]
                  ] io

  ////////// Quit
  // Terminates the program
  Quit :: ProgState IO -> (ProgState, IO)
  Quit state io = (state, QuitIO io)

  ///////// DeleteSelection
  // Deletes the selected item.
  DeleteSelection :: ProgState IO -> (ProgState, IO)
  DeleteSelection state=:{editMode=ProofMode} io = (state, io)
  DeleteSelection state=:{currentWindow, windowInfo, currentComp, currentWire, itemSelected} io
    | not itemSelected = (state, io)   // If no window is selected, then no item should be selected
    | currentWire==(-1)
        = if (IsBox currentComp)
             (compDeletedState, newIO1)  // The box's internal circuit is *not* deleted.
             (compDeletedState, newIO2)
    | otherwise
        = (wireDeletedState, newIO3)
  where
    (windowState=:{windowID, circuit, wires}) = GetWindowState currentWindow windowInfo

    compDeletedState = {state & windowInfo = compDeletedWindowInfo, itemSelected = False}
    compDeletedWindowInfo = PutWindowState
                              currentWindow
                              {windowState & circuit=compDeletedCircuit, wires=compDeletedWires, nextVar=newNextVar1}
                              windowInfo

    // If the component is a box, its internal circuit will *not* be deleted.
    (compDeletedCircuit, compDeletedWires, newNextVar1) = DeleteComponent currentComp circuit
    newIO1 = DrawInWindow windowID
                          [SetPenMode XorMode :
                           ((DrawComponentWires currentComp wires) ++
                            (DrawComponent {currentComp & spec=Box []}) ++
                            (DrawSelection currentComp currentWire wires)
                           )
                          ]
                          (DisableMenuItems [deleteID] io)
    newIO2 = DrawInWindow windowID
                          [SetPenMode XorMode :
                           ((DrawComponentWires currentComp wires) ++
                            (DrawComponent currentComp) ++
                            (DrawSelection currentComp currentWire wires)
                           )
                          ]
                          (DisableMenuItems [deleteID] io)

    wireDeletedState = {state & windowInfo=wireDeletedWindowInfo, itemSelected = False}
    wireDeletedWindowInfo = PutWindowState
                              currentWindow
                              {windowState & circuit = wireDeletedCircuit, wires = wireDeletedWires, nextVar = newNextVar2}
                              windowInfo
    (wireDeletedCircuit, wireDeletedWires, newNextVar2)
      = DeleteWire currentWire circuit
    newIO3 = DrawInWindow windowID
                          [SetPenMode XorMode :
                           ((DrawWire (GetFirst (((==) currentWire) o GetWireID) wires)) ++
                            (DrawSelection currentComp currentWire wires)
                           )
                          ]
                          (DisableMenuItems [deleteID, setTypeID, removeTypeID] io)

  ///////// SetNewWireType
  // Allows the user to set a user-specified type on the selected wire.
  // This function should never be called if a wire is not selected.
  SetNewWireType :: ProgState IO -> (ProgState, IO)
  SetNewWireType state=:{windowInfo} io
    = OpenModalDialog wireTypeDialog state (seq [DisableAllWindows windowInfo, DisableMenuSystem] io)
  where
    wireTypeDialog
      = CommandDialog wireTypeDlgID "Set Wire Type" [] ok1ID
         [StaticText text1ID Left "Enter the wire's new type (use infix `(x)' for products, infix `(+)' for sums,",
          StaticText text1ID Left "infix `(-o)' for implications, `<function name>(type1, ...)' for other functions,",
          StaticText text1ID Left "`Unit' for units, and `Counit' for counits, and enclose constants in quotation marks.",
          StaticText text1ID Left "Function names, variables and constants should contain only letters and underline",
          StaticText text1ID Left "characters.  Ex:  MyFunc(\"MyConst\"(x)Unit, My_Var)",
          StaticText text1ID Left "",
          EditText wireTypeID Left (Inch 5.0) 1 "",
          DialogButton ok1ID Left "Continue" Able SetType,
          DialogButton cancel1ID (RightTo ok1ID) "Cancel" Able CancelType
         ]

    SetType :: DialogInfo ProgState IO -> (ProgState, IO)
    SetType dlgInfo state=:{currentWire, currentWindow, windowInfo} io
      | not parsed
          = (state, DisplayNotice "This type doesn't parse." io)
      | IsUserType wire.wireType
          = if successA
               ({state & windowInfo=newWindowInfoA},
                seq [CloseDialog wireTypeDlgID, EnableMenuSystem, EnableAllWindows windowInfo] io
               )
               (state, DisplayNotice "This type doesn't unify." io)
      | successB
          = ({state & windowInfo=newWindowInfoB},
             seq [CloseDialog wireTypeDlgID, EnableMenuSystem, EnableAllWindows windowInfo,
                  // VVV The wire's appearance changes when the type becomes user-specified.
                  DrawInWindow windowID [SetPenMode XorMode : ((DrawWire wire) ++ (DrawWire newWire))]
                 ] io
            )
      | otherwise
          = (state, DisplayNotice "This type doesn't unify." io)
    where
      typeString     = GetEditText wireTypeID dlgInfo
      (parsed, type) = ParseType typeString

      (windowState=:{circuit, wires, windowID}) = GetWindowState currentWindow windowInfo
      (wire, wires2) = RemoveAndReturn (((==) currentWire) o GetWireID) wires

      // Called to retype the whole circuit when the wire already had a user-specified type.
      (successA, newCircuitA, newWiresA, newNextVarA)
        = Retype (ChangeWireType currentWire (wire.wireLine) (User type) circuit)
      newWindowInfoA = PutWindowState currentWindow
                                      {windowState & circuit=newCircuitA, wires=newWiresA, nextVar=newNextVarA}
                                      windowInfo

      // Called when the previous type was free - it's not necessary to retype the whole circuit in this case.
      (successB, subs) = MatchTypes type (GetType wire.wireType) []
      newCircuitB      = SubsIntoCircuit subs (ChangeWireType currentWire (wire.wireLine) (User type) circuit)
      newWire          = {wire & wireType=(User type)}
      newWiresB        = [newWire : SubsIntoWires subs wires2]
      newWindowInfoB = PutWindowState currentWindow
                                      {windowState & circuit=newCircuitB, wires=newWiresB}
                                      windowInfo

    CancelType :: DialogInfo ProgState IO -> (ProgState, IO)
    CancelType _ state=:{windowInfo} io
      = (state, seq [CloseDialog wireTypeDlgID, EnableMenuSystem, EnableAllWindows windowInfo] io)

  ///////// RemoveWireType
  // Removes the user-specified type from the selected wire and retypes the circuit.
  RemoveWireType :: ProgState IO -> (ProgState, IO)
  RemoveWireType state=:{currentWire, currentWindow, windowInfo} io
    | IsUserType wire.wireType
        = ({state & windowInfo=newWindowInfo},
           DrawInWindow windowID [SetPenMode XorMode : ((DrawWire wire) ++ (DrawWire {wire & wireType=newWireType}))] io
          )
    | otherwise                = (state, DisplayNotice "This wire does not have a user-specified type." io)
  where
    (windowState=:{circuit, wires, windowID}) = GetWindowState currentWindow windowInfo
    wire = GetFirst (((==) currentWire) o GetWireID) wires
    newWireType = MakeFree wire.wireType
    (_, newCircuit, newWires, newNextVar) = Retype (ChangeWireType currentWire wire.wireLine newWireType circuit)
    newWindowInfo = PutWindowState currentWindow
                                   {windowState & circuit=newCircuit, wires=newWires, nextVar=newNextVar}
                                   windowInfo

  ////////// Redraw
  // Redraws the circuit in the current window.
  Redraw :: ProgState IO -> (ProgState, IO)
  Redraw state=:{currentWindow, windowInfo} io
    | NotWindow currentWindow = (state, DisplayNotice "No window is selected.  Click in a window to select it." io)
    | otherwise               = DrawInWindowFrame windowID (RedrawCircuit currentWindow) state io
  where
    {windowID} = GetWindowState currentWindow windowInfo

  ////////// EraseCirc
  // Removes the circuit from the current window.  It should not be possible to call it in ProofMode.
  EraseCirc :: ProgState IO -> (ProgState, IO)

  EraseCirc state=:{currentWindow, windowInfo, files} io1
    | NotWindow currentWindow
        = (state, DisplayNotice "No window is selected.  Click in a window to select it." io1)
    | not confirmed
        = (state, DisplayNotice "Click in the window you want to erase before selecting this option." io2)
    | otherwise
        = SetEditMode EditingCircuit newState
                      (seq [DrawInWindow windowID [EraseRectangle windowDomain],
                            ChangeWindowTitle windowID (windowName +++ ": Not saved"),
                            DisableMenuItems [deleteID, setTypeID, removeTypeID]
                           ] io2
                      )
  where
    (windowState=:{windowID, windowName, windowDomain, circuit}) = GetWindowState currentWindow windowInfo
    (confirmed, io2) = ConfirmCheck
    ConfirmCheck
      | isEmpty circuit = (True, io1)
      | otherwise       = Confirm ("Erase the contents of " +++ windowName +++ "?") io1
    newState = {state & windowInfo = PutWindowState currentWindow newWindowState windowInfo,
                        drawMode = NotDrawing,
                        sizeDir = TopLeft,
                        itemSelected = False,
                        files = files
               }
    newWindowState = {windowState & fileName="",
                                    circuit = [],
                                    wires = [],
                                    compID=0,
                                    nextWireID = 0,
                                    nextVar = 1
                     }

  ////////// SetEditMode
  // Sets the edit mode.  If the edit mode was EditingCircuit and is being changed to
  // ProofMode, the circuits in all open windows must sequentialize for it to succeed.
  SetEditMode :: EditMode ProgState IO -> (ProgState, IO)

  SetEditMode EditingCircuit state=:{editMode=EditingCircuit} io = (state, io)

  SetEditMode EditingCircuit state=:{currentWindow, windowInfo, itemSelected, currentComp, currentWire} io
    | itemSelected = ({state & editMode=EditingCircuit, editType=StartTerminalDraw, itemSelected=False}, newIO1)
    | otherwise    = ({state & editMode=EditingCircuit, editType=StartTerminalDraw}, newIO2)
  where
    (windowState=:{windowID, wires}) = GetWindowState currentWindow windowInfo
    newIO1 = seq [CloseDialog toolbox2ID, OpenDialog toolbox1,
                  DrawInWindow windowID [SetPenMode XorMode : DrawSelection currentComp currentWire wires],
                  DisableMenus [flipLinkMenuID, doRewritesID], DisableMenuItems [equalityID],
                  EnableMenus [editRewritesID], EnableMenuItems [eraseID],
                  UnmarkMenuItems [proofModeID], MarkMenuItems [editModeID]
                 ] io
    newIO2 = seq [CloseDialog toolbox2ID, OpenDialog toolbox1, DisableMenus [flipLinkMenuID, doRewritesID],
                  DisableMenuItems [equalityID], EnableMenus [editRewritesID], EnableMenuItems [eraseID],
                  UnmarkMenuItems [proofModeID], MarkMenuItems [editModeID]
                 ] io

  SetEditMode ProofMode state=:{editMode=ProofMode} io = (state, io)

  SetEditMode ProofMode
              state=:{currentWindow, windowInfo=(window1State, window2State), itemSelected, currentComp, currentWire,
                      nextID}
              io
    | not ((isEmpty window1State.circuit) || success1)
        = (state,
           DisplayNotice ("The circuit in " +++ window1State.windowName +++ " does not sequentialize (in FILL)")
                         io
          )
    | not ((isEmpty window2State.circuit) || success2)
        = (state,
           DisplayNotice ("The circuit in " +++ window2State.windowName +++ " does not sequentialize (in FILL)")
                         io
          )
    | itemSelected = ({state & editMode=ProofMode, editType=Selection, windowInfo=newWindowInfo, itemSelected=False},
                      newIO1
                     )
    | otherwise    = ({state & editMode=ProofMode, editType=Selection, windowInfo=newWindowInfo}, newIO2)
  where
    (success1, sequentCirc1)
      = if window1State.windowOpen
           (FILLSequentialize window1State.circuit)
           (True, [])

    (success2, sequentCirc2)
      = if window2State.windowOpen
           (FILLSequentialize window2State.circuit)
           (True, [])

    newWindowInfo
      = (if window1State.windowOpen {window1State & inputPairs=inputPairs1, outputPairs=outputPairs1} window1State,
         if window2State.windowOpen {window2State & inputPairs=inputPairs2, outputPairs=outputPairs2} window2State
        )
    // These keep track of the order of start and end terminals and determine how much they can be moved horizontally.
    inputPairs1  = GetInputPositionPairs window1State.wires (hd sequentCirc1).inputs
    outputPairs1 = GetOutputPositionPairs window1State.wires (hd sequentCirc1).outputs
    inputPairs2  = GetInputPositionPairs window2State.wires (hd sequentCirc2).inputs
    outputPairs2 = GetOutputPositionPairs window2State.wires (hd sequentCirc2).outputs

    ({windowID, wires}) = GetWindowState currentWindow (window1State, window2State)
    newIO1 = seq [CloseDialog toolbox1ID, OpenDialog toolbox2,
                  DrawInWindow windowID [SetPenMode XorMode : DrawSelection currentComp currentWire wires],
                  EnableMenuItems [equalityID], DisableMenus [editRewritesID],
                  DisableMenuItems [deleteID, setTypeID, removeTypeID, eraseID],
                  UnmarkMenuItems [editModeID],
                  MarkMenuItems [proofModeID]
                 ] io
    newIO2 = seq [CloseDialog toolbox1ID, OpenDialog toolbox2,
                  EnableMenuItems [equalityID], DisableMenus [editRewritesID],
                  DisableMenuItems [eraseID], UnmarkMenuItems [editModeID], MarkMenuItems [proofModeID]
                 ] io

  ////////// AddGeneric
  // Allows the user to create a new component.  First opens a dialog box in which the user gives the name
  // and number of inputs and outputs for the component, then opens a window displaying the component to
  // allow the user to specify the input and output types by clicking on each one and then entering
  // the type.
  AddGeneric :: ProgState IO -> (ProgState, IO)
  AddGeneric state=:{windowInfo} io
    = (state, OpenDialog addGenericDlg (seq [DisableAllWindows windowInfo, DisableMenuSystem] io))
  where
    addGenericDlg =
      (CommandDialog addGenericID "Add New Component" [] ok1ID
       [StaticText text1ID Left "Enter the component's name:    ",
        EditText compNameID (RightTo text1ID) (Inch 1.5) 1 "",
        StaticText text2ID Left "Enter the number of inputs:  ",
        EditText compInputsID (Below compNameID) (Inch 1.0) 1 "",
        StaticText text2ID Left "Enter the number of outputs:  ",
        EditText compOutputsID (Below compInputsID) (Inch 1.0) 1 "",
        DialogButton ok1ID Left "Continue" Able ContinueGeneric,
        DialogButton cancel1ID (RightTo ok1ID) "Cancel" Able CancelGeneric
       ]
      )

    ContinueGeneric :: DialogInfo ProgState IO -> (ProgState, IO)
    ContinueGeneric dlgInfo state=:{generics} io
      | compName==""
          = (state,
             seq [CloseDialog addGenericID, EnableAllWindows windowInfo, EnableMenuSystem,
                  DisplayNotice "The component must have a name."
                 ] io
            )
      | (inputCount < 0) || (outputCount < 0)
          = (state,
             seq [CloseDialog addGenericID, EnableAllWindows windowInfo, EnableMenuSystem,
                  DisplayNotice "You cannot have a negative number of inputs or outputs"
                 ] io
            )
      | (inputCount == 0) && (outputCount == 0)
          = (state,
             seq [CloseDialog addGenericID, EnableAllWindows windowInfo, EnableMenuSystem,
                  DisplayNotice "The component must have some inputs or outputs."
                 ] io
            )
      | any (((==) compName) o GetGenericName o fst3) generics
          = if replaceConfirmed
               (OpenModalDialog finalGenericDlg {state & newGenericDisplay=displayCirc}
                                (seq [CloseDialog addGenericID, OpenWindows [genericDisplayWin]] io2)
               )
               (state, seq [CloseDialog addGenericID, EnableAllWindows windowInfo, EnableMenuSystem] io2)
      | otherwise
          = OpenModalDialog finalGenericDlg {state & newGenericDisplay=displayCirc}
                            (seq [CloseDialog addGenericID, OpenWindows [genericDisplayWin]] io)
    where
      compName    = GetEditText compNameID dlgInfo
      inputCount  = toInt (GetEditText compInputsID dlgInfo)
      outputCount = toInt (GetEditText compOutputsID dlgInfo)

      (replaceConfirmed, io2) = Confirm "A component with this name already exists.  Do you want to replace it?" io

      // newGeneric1 is a tuple giving the CompSpecifics, width and height for the new component.
      newGeneric1 = MakeNewGeneric compName inputCount outputCount

      // Creates a small circuit used to display the circuit while the user sets input and output types.
      (displayCirc, displayWires) = MakeGenericDisplay newGeneric1

      finalGenericDlg =
        (CommandDialog addGenericID "Add New Component" [] ok1ID
         [StaticText text1ID Left "Click on the component's wires to set their types.",
          DialogButton ok1ID Left "Continue" Able DoneGeneric,
          DialogButton cancel1ID (RightTo ok1ID) "Cancel" Able CancelGeneric
         ]
        )

      genericDisplayWin = FixedWindow genericDisplayWinID (100,100) ("New component:  " +++ compName)
                                      displayWinArea (DrawGenericDisplay displayCirc displayWires)
                                      [Mouse Able (SetGenericTypesHandler displayCirc displayWires (-1))]

      displayWinArea = ((0,0),(width + 2*displayMargin, height + 2*displayTerminalDistance + 2*displayMargin))
      (_, width, height) = newGeneric1

      DoneGeneric :: DialogInfo ProgState IO -> (ProgState, IO)
      DoneGeneric _ state=:{newGenericDisplay, generics, files} io1
        | not fileSelected = ({state & files=files2}, io2)
        | not fileOpened   = ({state & files=files3}, DisplayNotice "Unable to open this file for writing." io2)
        | not fileClosed   = ({state & files=files4}, DisplayNotice "An error occurred when closing the file." io2)
        | otherwise        = ({state & newGenericDisplay=[], generics=[newGeneric2 : newGenerics], files=files4},
                              seq [CloseDialog addGenericID, CloseWindows [genericDisplayWinID],
                                   EnableAllWindows windowInfo, EnableMenuSystem,
                                   DisplayNotice "The component has been added at the end of the file."
                                  ] io2
                             )
      where
        newGeneric2    = (newGenericSpec, width, height)
        // SetGenericTypes replaces user variables with variables, with 0 as the first variable number.
        newGenericSpec = SetGenericTypes (GetDisplayGeneric newGenericDisplay)
        // Removes a component with the same name if there is one.
        newGenerics    = filter (((<>) (GetGenericName newGenericSpec)) o GetGenericName o fst3) generics

        (fileSelected, fileName, files2, io2) = SelectOutputFile "Save Component As:" "" files io1
        (fileOpened, file1, files3)           = fopen fileName FAppendText files2
        file2                                 = SaveGeneric newGeneric2 file1
        (fileClosed, files4)                  = fclose file2 files3

    // CancelGeneric cancels the creation of a new component from either the first dialog box (for entering the name
    // and number of inputs and outputs) or the second dialog box (to indicate when the user is done).
    CancelGeneric :: DialogInfo ProgState IO -> (ProgState, IO)
    CancelGeneric _ state=:{windowInfo} io
      = (state,
         seq [CloseDialog addGenericID,
              CloseWindows [genericDisplayWinID],
              EnableAllWindows windowInfo,
              EnableMenuSystem
             ] io
        )

  ////////// MakeGenericDisplay
  // Creates a circuit consisting of the new component and start and end terminals for each
  // of its connections, with arbitrary variable types on the wires.
//*^*  MakeGenericDisplay :: (CompSpecifics, Int, Int) -> (Circuit, [Wire])
  MakeGenericDisplay :: (CompSpecifics Component, Int, Int) -> (Circuit, [Wire])
  MakeGenericDisplay (Generic compName inputTypes outputTypes, width, height)
    = (startTerminals ++ [newComp : endTerminals], inWires ++ outWires)
  where
    newComp        = {spec=Generic compName inputTypes outputTypes, id=0, inputs=inputs, outputs=outputs, pos=RCT compRect}
    inputs         = [{cWireID=x, cWireType=Free type} \\ x <- [0..] & type <- inputTypes]
    outputs        = [{cWireID=x, cWireType=Free type} \\ x <- [inputCount..] & type <- outputTypes]
    compRect       = ((displayMargin, displayMargin + displayTerminalDistance),
                      (displayMargin + width, displayMargin + displayTerminalDistance + height)
                     )
    startTerminals = [{spec=StartTerminal, id=(x+1), inputs=[], outputs=[input],
                       pos=PT (fst (InConnectionPoint newComp.spec newComp.pos x), displayMargin)
                      } \\ x <- [0..] & input <- inputs
                     ]
    inWires        = [{wireID=cWireID,
                       wireLine=(OutConnectionPoint StartTerminal pos 0, InConnectionPoint newComp.spec newComp.pos x),
                       wireType=cWireType
                      } \\ x <- [0..] & {pos, outputs=[{cWireID, cWireType}]} <- startTerminals
                     ]
    endTerminals   = [{spec=EndTerminal, id=(x+inputCount+1), inputs=[output], outputs=[],
                       pos=PT (fst (OutConnectionPoint newComp.spec newComp.pos x), endTermY)
                      } \\ x <- [0..] & output <- outputs
                     ]
    outWires       = [{wireID=cWireID,
                       wireLine=(OutConnectionPoint newComp.spec newComp.pos x, InConnectionPoint EndTerminal pos 0),
                       wireType=cWireType
                      } \\ x <- [0..] & {pos, inputs=[{cWireID, cWireType}]} <- endTerminals
                     ]
    inputCount     = length inputTypes
    endTermY       = displayMargin + height + 2*displayTerminalDistance

  ////////// GetDisplayGeneric
  // Takes a circuit used to display a new component while its input and output types are set and returns
  // the CompSpecifics of the new component in the circuit.
//*^*  GetDisplayGeneric :: Circuit -> CompSpecifics
  GetDisplayGeneric :: Circuit -> CompSpecifics Component
  GetDisplayGeneric [{spec=StartTerminal} : circuit]
    = GetDisplayGeneric circuit
  GetDisplayGeneric [{spec=Generic name _ _, inputs, outputs} : _]
    = Generic name (map (GetType o GetConnectWireType) inputs) (map (GetType o GetConnectWireType) outputs)

  ////////// DrawGenericDisplay
  // Used to draw the display circuit for the new component.
  DrawGenericDisplay :: Circuit [Wire] UpdateArea ProgState -> (ProgState, [DrawFunction])
  DrawGenericDisplay circuit wires _ state = (state, redraws)
  where
    redraws    = ((flatten (map DrawWire wires)) ++
                  (flatten (map DrawComponent circuit))
                 )

  ////////// SetGenericTypesHandler
  // Allows the user to set types for input and output connections of a new component by
  // clicking on the wire for the desired connection, and then entering the new type.
  SetGenericTypesHandler :: Circuit [Wire] WireID MouseState ProgState IO -> (ProgState, IO)
//***  SetGenericTypesHandler circuit wires lastWireID (mousePos, ButtonDoubleDown, _) state=:{windowInfo} io
  SetGenericTypesHandler circuit wires lastWireID (mousePos, ButtonDown, _) state=:{windowInfo} io
    | selectionMade
        = if (lastWireID<>(-1))
             (OpenModalDialog wireTypeDialog state
               (DrawInWindow genericDisplayWinID
                 [SetPenMode XorMode :
                  ((DrawSelection dummyComponent lastWireID wires) ++ (DrawSelection dummyComponent wire.wireID wires))
                 ] io
               )
             )
             (OpenModalDialog wireTypeDialog state
               (DrawInWindow genericDisplayWinID
                 [SetPenMode XorMode : DrawSelection dummyComponent wire.wireID wires] io
               )
             )
    | otherwise     = (state, io)
  where
    (selectionMade, wire) = GetSelectedWire mousePos wires

    wireTypeDialog
      = CommandDialog wireTypeDlgID "Set Wire Type" [] ok2ID
         [StaticText text1ID Left "Enter the wire's new type (use infix `(x)' for products, infix `(+)' for sums,",
          StaticText text1ID Left "infix `(-o)' for implications, `<function name>(type1, ...)' for other functions,",
          StaticText text1ID Left "`Unit' for units, and `Counit' for counits, and enclose constants in quotation marks.",
          StaticText text1ID Left "Function names, variables and constants should contain only letters and underline",
          StaticText text1ID Left "characters.  Ex:  MyFunc(\"MyConst\"(x)Unit, My_Var)",
          StaticText text1ID Left "",
            // VVV If the user has already set a type for the connection, it is displayed in the text box.
          EditText wireTypeID Left (Inch 5.0) 1 (if (IsUserType wire.wireType) (TypeString (GetType wire.wireType)) ""),
          DialogButton ok2ID Left "Continue" Able (SetGenericType circuit wires),
          DialogButton cancel2ID (RightTo ok2ID) "Cancel" Able CancelType
         ]

    SetGenericType :: Circuit [Wire] DialogInfo ProgState IO -> (ProgState, IO)
    SetGenericType circuit wires dlgInfo state io
      | not parsed
          = (state,
             seq [ChangeMouseFunction genericDisplayWinID (SetGenericTypesHandler circuit wires wire.wireID),
                  DisplayNotice "This type doesn't parse."
                 ] io
            )
      | otherwise
          = ({state & newGenericDisplay = newCircuit},
             seq [CloseDialog wireTypeDlgID,
                  ChangeUpdateFunction genericDisplayWinID (DrawGenericDisplay newCircuit newWires),
                  ChangeMouseFunction genericDisplayWinID (SetGenericTypesHandler newCircuit newWires wire.wireID),
                  DrawInWindow genericDisplayWinID [SetPenMode XorMode : ((DrawWire wire) ++ (DrawWire newWire))]
                 ] io
            )
    where
      typeString     = GetEditText wireTypeID dlgInfo
      (parsed, type) = ParseType typeString

      (_, wires2) = RemoveAndReturn (((==) wire.wireID) o GetWireID) wires

      newCircuit = ChangeWireType wire.wireID (wire.wireLine) (User type) circuit
      newWire    = {wire & wireType=(User type)}
      newWires   = [newWire : wires2]

    CancelType :: DialogInfo ProgState IO -> (ProgState, IO)
    CancelType _ state io
      = (state,
         seq [ChangeMouseFunction genericDisplayWinID (SetGenericTypesHandler circuit wires wire.wireID),
              CloseDialog wireTypeDlgID
             ] io
        )

  SetGenericTypesHandler _ _ _ _ state io = (state, io)  // Ignores all other mouse events.

  ////////// ReadGenericsFile
  // Reads generic (user-defined) components in from a file.
  ReadGenericsFile :: ProgState IO -> (ProgState, IO)
  ReadGenericsFile state=:{files} io
    | not fileSelected = ({state & files=files2}, io2)
    | not fileOpened   = ({state & files=files3}, DisplayNotice "Unable to open this file for reading." io2)
    | not noProblem    = ({newState & files=files4}, DisplayNotice "Unable to read all components from this file." newIO)
    | not fileClosed   = ({newState & files=files4}, DisplayNotice "An error occurred when closing the file." newIO)
    | otherwise        = ({newState & files=files4}, DisplayNotice "Finished reading components from the file." newIO)
  where
    (fileSelected, fileName, files2, io2) = SelectInputFile files io
    (fileOpened, file1, files3)           = fopen fileName FReadText files2
    (noProblem, file2, newState, newIO)   = ReadGenerics file1 {state & files=files3} io2
    (fileClosed, files4)                  = fclose file2 newState.files

    ReadGenerics file1 state=:{generics} io1
      | done             = (True, file2, state, io1)
      | not genericRead  = (False, file2, state, io1)
      | any (((==) genericName) o GetGenericName o fst3) generics
          = if replaceConfirmed
               (ReadGenerics file2 {state & generics=[newGeneric : newGenerics]} io2)
               (ReadGenerics file2 state io2)
      | otherwise
          = ReadGenerics file2 {state & generics=[newGeneric : generics]} io1
    where
      (done, genericRead, newGeneric, file2) = ReadGeneric file1

      genericName = GetGenericName (fst3 newGeneric)
      (replaceConfirmed, io2)
        = Confirm ("There is already a component with the name \"" +++ genericName +++ "\".  Do you want to replace it?")
                  io1
      (_, newGenerics) = RemoveAndReturn (((==) genericName) o GetGenericName o fst3) generics

  ////////// FlipLink
  // Flips the selected thinning link:  UnitEL to UnitER and vice versa, and CounitIL to CounitIR and vice versa.
  // It keeps the link in the same rectangular area, which may bend the wire but guarantees no overlaps with
  // other components.
  FlipLink :: ProgState IO -> (ProgState, IO)
  FlipLink state=:{editMode=EditingCircuit} io = (state, io)
  FlipLink state=:{itemSelected=False} io = (state, io)
  FlipLink state=:{currentWindow, windowInfo, currentComp} io
    | IsThinningLink currentComp
        = ({state & windowInfo=newWindowInfo, currentComp=flippedComp}, newIO)
    | otherwise
        = (state, io)
  where
    (windowState=:{windowID, circuit, wires}) = GetWindowState currentWindow windowInfo
    newWindowInfo = PutWindowState currentWindow {windowState & circuit=newCircuit, wires=newWires} windowInfo

    newCircuit = ReplaceComponent flippedComp circuit
    (inWireID, outWireID, lassoWireID) = GetLassoWireIDs currentComp
    (inWire, wires2)    = RemoveAndReturn (((==) inWireID) o GetWireID) wires
    (outWire, wires3)   = RemoveAndReturn (((==) outWireID) o GetWireID) wires2
    (lassoWire, wires4) = RemoveAndReturn (((==) lassoWireID) o GetWireID) wires3
    newWires = [newInWire, newOutWire, newLassoWire : wires4]

    (flippedComp, newInLine, newOutLine, newLassoLine)
      = Flip currentComp inWire.wireLine outWire.wireLine lassoWire.wireLine
    ((leftX, _), (rightX, _)) = ComponentRectangle currentComp

    newInWire    = {inWire & wireLine=newInLine}
    newOutWire   = {outWire & wireLine=newOutLine}
    newLassoWire = {lassoWire & wireLine=newLassoLine}

    newIO = DrawInWindow windowID
             [SetPenMode XorMode :
              ((DrawWire inWire) ++ (DrawWire outWire) ++ (DrawWire lassoWire) ++
               (DrawWire newInWire) ++ (DrawWire newOutWire) ++ (DrawWire newLassoWire) ++
               (DrawComponent currentComp) ++ (DrawComponent flippedComp)
              )
             ] io

    Flip comp=:{spec=UnitEL, inputs=[inC1, inC2], pos=PT (x,y)} (inStart, _) (_, outEnd) (lassoStart, _)
      = ({comp & spec=UnitER, inputs=[inC2, inC1], pos=PT newPoint},
         (inStart, LassoInConnection newPoint),
         (LassoOutConnection newPoint, outEnd),
         (lassoStart, LassoRightInConnection newPoint)
        )
    where
      newPoint = (rightX + leftX - x, y)

    Flip comp=:{spec=UnitER, inputs=[inC1, inC2], pos=PT (x,y)} (inStart, _) (_, outEnd) (lassoStart, _)
      = ({comp & spec=UnitEL, inputs=[inC2, inC1], pos=PT newPoint},
         (inStart, LassoInConnection newPoint),
         (LassoOutConnection newPoint, outEnd),
         (lassoStart, LassoLeftInConnection newPoint)
        )
    where
      newPoint = (rightX + leftX - x, y)

    Flip comp=:{spec=CounitIL, outputs=[outC1, outC2], pos=PT (x,y)} (inStart, _) (_, outEnd) (_, lassoEnd)
      = ({comp & spec=CounitIR, outputs=[outC2, outC1], pos=PT newPoint},
         (inStart, LassoInConnection newPoint),
         (LassoOutConnection newPoint, outEnd),
         (LassoRightOutConnection newPoint, lassoEnd)
        )
    where
      newPoint = (rightX + leftX - x, y)

    Flip comp=:{spec=CounitIR, outputs=[outC1, outC2], pos=PT (x,y)} (inStart, _) (_, outEnd) (_, lassoEnd)
      = ({comp & spec=CounitIL, outputs=[outC2, outC1], pos=PT newPoint},
         (inStart, LassoInConnection newPoint),
         (LassoOutConnection newPoint, outEnd),
         (LassoLeftOutConnection newPoint, lassoEnd)
        )
    where
      newPoint = (rightX + leftX - x, y)

  ////////// ReplaceComponent
  // Takes a new component and a circuit, finds the component in the circuit with the same ID as the
  // new component, and replaces it with the new component.
  ReplaceComponent :: Component Circuit ->Circuit
  ReplaceComponent comp1=:{id=id1} [comp2=:{spec=Box boxCircuit, id=id2, pos=RCT rectangle} : circuit]
    | id1==id2
        = [comp1 : circuit]
    | Encloses rectangle (ComponentRectangle comp1)
        = [{comp2 & spec=Box (ReplaceComponent comp1 boxCircuit)} : circuit]
    | otherwise
        = [comp2 : ReplaceComponent comp1 circuit]

  ReplaceComponent comp1=:{id=id1} [comp2=:{id=id2} : circuit]
    | id1==id2  = [comp1 : circuit]
    | otherwise = [comp2 : ReplaceComponent comp1 circuit]

  ////////// DoProofRewrite
  // Performs a rewrite in the current window's circuit using the selected wire as the live wire.
  // Should never be called unless a wire is selected.
  DoProofRewrite :: Rewrite ProgState IO -> (ProgState, IO)
  DoProofRewrite rewrite state=:{currentWindow, windowInfo, currentWire, currentComp, startPos} io
    | IsNotError returnCode
        = ({state & windowInfo=newWindowInfo, itemSelected=False},
           DrawInWindow windowID [SetPenMode XorMode : (DrawSelection currentComp currentWire wires ++ redraws)] io
          )
    | otherwise
        = (state, DisplayNotice (ErrorMessage returnCode) io)
  where
    (windowState=:{windowID, circuit, wires, nextWireID, compID, nextVar, outputPairs})
      = GetWindowState currentWindow windowInfo
    (returnCode, newCircuit, newWires, newNextWireID, newCompID, newNextVar, redraws, newOutputPairs)
      = DoRewrite rewrite currentWire startPos circuit wires nextWireID compID nextVar outputPairs
    newWindowInfo
      = PutWindowState currentWindow
                       {windowState & circuit=newCircuit, wires=newWires, nextWireID=newNextWireID, compID=newCompID,
                                      nextVar=newNextVar, outputPairs=newOutputPairs
                       }
                       windowInfo

    ErrorMessage BadMatch        = "The selected part of the circuit does not match this rewrite.  Make sure that you selected the right wire."
    ErrorMessage TypeMismatch    = "The types in the rewrite rule do not match the types in the circuit."
    ErrorMessage NewCompOverlaps = "The right side of this rule overlaps part of the circuit."
    ErrorMessage BadSequent      = "The circuit will not sequentialize after this rewrite."

  ////////// DoProofBoxRewrite
  // Does the box reduction rewrite in the current window's circuit, using the selected wire as the live wire.
  // Should never be called unless a wire is selected.
  DoProofBoxRewrite :: ProgState IO -> (ProgState, IO)
  DoProofBoxRewrite state=:{currentWindow, windowInfo, currentWire, currentComp, startPos} io
    | IsNotError returnCode
        = ({state & windowInfo=newWindowInfo, itemSelected=False},
           DrawInWindow windowID [SetPenMode XorMode : (DrawSelection currentComp currentWire wires ++ redraws)] io
          )
    | otherwise
        = (state, DisplayNotice "The selected part of the circuit does not match this rewrite.  Make sure that you selected the right wire (between the box and the lolly)." io)
  where
    (windowState=:{windowID, circuit, wires, outputPairs}) = GetWindowState currentWindow windowInfo
    (returnCode, newCircuit, newWires, redraws, newOutputPairs, _, _, _)
      = DoBoxRewrite currentWire startPos circuit wires outputPairs
    newWindowInfo = PutWindowState currentWindow
                                   {windowState & circuit=newCircuit, wires=newWires, outputPairs=newOutputPairs}
                                   windowInfo

  ////////// CloseRewrites
  // Closes the rewrite display windows.  Shouldn't be called if they aren't open.
  CloseRewrites :: ProgState IO -> (ProgState, IO)
  CloseRewrites state io
    = (state, seq [CloseWindows [leftSideWindowID, rightSideWindowID], DisableMenuItems [closeRewritesID]] io)

  ////////// DisplayRewrite
  // Displays the two sides of a rewrite in two special display windows that can't be edited.
  DisplayRewrite :: Rewrite ProgState IO -> (ProgState, IO)
  DisplayRewrite {rewriteName, liveWireID, leftComp, leftTop, leftMid, leftBot, oldRightSide} state1 io1
    | windowsOpen
       = DrawInWindowFrame rightSideWindowID DrawRightCirc state2 io3
    | otherwise
       = (state1, seq [OpenWindows [leftSideWindow, rightSideWindow], EnableMenuItems [closeRewritesID]] io2)
  where
    (windowsOpen, io2) = RewriteWindowsOpen io1

    (state2, io3)
      = DrawInWindowFrame leftSideWindowID DrawLeftCirc state1
                          (seq [ChangeUpdateFunction leftSideWindowID DrawLeftCirc,
                                ChangeMouseFunction leftSideWindowID (DisplayTypesHandler leftSide leftWires),
                                ChangeWindowTitle leftSideWindowID (rewriteName +++ ": left side"),
                                ChangeUpdateFunction rightSideWindowID DrawRightCirc,
                                ChangeMouseFunction rightSideWindowID (DisplayTypesHandler oldRightSide rightWires),
                                ChangeWindowTitle rightSideWindowID (rewriteName +++ ": right side")
                               ] io2
                          )

    leftSideWindow  = ScrollWindow leftSideWindowID (100,100) (rewriteName +++ ": left side")
                                   (ScrollBar (Thumb thumbValue) (Scroll scrollValue))
                                   (ScrollBar (Thumb thumbValue) (Scroll scrollValue))
                                   initialDomain minWinSize rewriteWinSize DrawLeftCirc
                                   [Mouse Able (DisplayTypesHandler leftSide leftWires)]

    DrawLeftCirc :: UpdateArea ProgState -> (ProgState, [DrawFunction])
    DrawLeftCirc _ state = (state, redraws)
    where
      redraws   = [(EraseRectangle initialDomain) :
                   ((DrawSelection dummyComponent liveWireID leftWires) ++
                    (flatten (map DrawWire leftWires)) ++
                    (flatten (map DrawComponent leftSide))
                   )
                  ]

    leftWires = GetCircuitWires leftSide
    leftSide  = ReverseAppend leftTop [leftComp : (leftMid ++ leftBot)]

    rightSideWindow = ScrollWindow rightSideWindowID (115 + (MaxX/3), 100) (rewriteName +++ ": right side")
                                   (ScrollBar (Thumb thumbValue) (Scroll scrollValue))
                                   (ScrollBar (Thumb thumbValue) (Scroll scrollValue))
                                   initialDomain minWinSize rewriteWinSize DrawRightCirc
                                   [Mouse Able (DisplayTypesHandler oldRightSide rightWires)]

    DrawRightCirc :: UpdateArea ProgState -> (ProgState, [DrawFunction])
    DrawRightCirc _ state = (state, redraws)
    where
      redraws    = [(EraseRectangle initialDomain) :
                    ((flatten (map DrawWire rightWires)) ++
                     (flatten (map DrawComponent oldRightSide))
                    )
                   ]

    rightWires = GetCircuitWires oldRightSide

  ////////// DisplayBoxRewrite
  // Displays the box reduction rewrite in two special display windows that can't be edited.
  DisplayBoxRewrite :: ProgState IO -> (ProgState, IO)
  DisplayBoxRewrite state1 io1
    | windowsOpen
       = DrawInWindowFrame rightSideWindowID DrawRightCirc state2 io3
    | otherwise
       = (state1, seq [OpenWindows [leftSideWindow, rightSideWindow], EnableMenuItems [closeRewritesID]] io2)
  where
    (windowsOpen, io2) = RewriteWindowsOpen io1

    (state2, io3)
      = DrawInWindowFrame leftSideWindowID DrawLeftCirc state1
          (seq [ChangeUpdateFunction leftSideWindowID DrawLeftCirc,
                ChangeMouseFunction leftSideWindowID (DisplayTypesHandler boxReductLeftSide boxReductLeftWires),
                ChangeWindowTitle leftSideWindowID "Box reduction: left side",
                ChangeUpdateFunction rightSideWindowID DrawRightCirc,
                ChangeMouseFunction rightSideWindowID (DisplayTypesHandler boxReductRightSide boxReductRightWires),
                ChangeWindowTitle rightSideWindowID "Box reduction: right side"
               ] io2
          )

    leftSideWindow  = ScrollWindow leftSideWindowID (100,100) "Box reduction: left side"
                                   (ScrollBar (Thumb thumbValue) (Scroll scrollValue))
                                   (ScrollBar (Thumb thumbValue) (Scroll scrollValue))
                                   initialDomain minWinSize rewriteWinSize DrawLeftCirc
                                   [Mouse Able (DisplayTypesHandler boxReductLeftSide boxReductLeftWires)]

    DrawLeftCirc :: UpdateArea ProgState -> (ProgState, [DrawFunction])
    DrawLeftCirc _ state = (state, redraws)
    where
      redraws   = [(EraseRectangle initialDomain) :
                   ((DrawSelection dummyComponent boxReductLiveWireID boxReductLeftWires) ++
                    (flatten (map DrawWire boxReductLeftWires)) ++
                    (flatten (map DrawComponent boxReductLeftSide))
                   )
                  ]

    rightSideWindow = ScrollWindow rightSideWindowID (115 + (MaxX/3), 100) "Box reduction: right side"
                                   (ScrollBar (Thumb thumbValue) (Scroll scrollValue))
                                   (ScrollBar (Thumb thumbValue) (Scroll scrollValue))
                                   initialDomain minWinSize rewriteWinSize DrawRightCirc
                                   [Mouse Able (DisplayTypesHandler boxReductRightSide boxReductRightWires)]

    DrawRightCirc :: UpdateArea ProgState -> (ProgState, [DrawFunction])
    DrawRightCirc _ state = (state, redraws)
    where
      redraws    = [(EraseRectangle initialDomain) :
                    ((flatten (map DrawWire boxReductRightWires)) ++
                     (flatten (map DrawComponent boxReductRightSide))
                    )
                   ]

  ////////// DisplayTypesHandler
  // Allows the user to see the type of a wire or connection in a display circuit
  // by clicking on it.
  DisplayTypesHandler :: Circuit [Wire] MouseState ProgState IO -> (ProgState, IO)
//***  DisplayTypesHandler circuit wires (mousePos, ButtonDown, _) state io
//***    | selectionMade = (state, DisplayNotice (WireTypeString wireType) io)
//***    | otherwise     = (state, io)
//***  where
//***    (selectionMade, wireType) = GetSelectedType mousePos circuit wires
  DisplayTypesHandler _ _ _ state io = (state, io)  // Ignores other mouse events.

  ////////// RewriteWindowsOpen
  // Checks whether the rewrite display windows are already open (if the left-side window is open,
  // the right-side window should be also).
  RewriteWindowsOpen :: IO -> (Bool, IO)
  RewriteWindowsOpen io1 = (NotIsDummyFrame frame, io2)
  where
    (frame, io2) = WindowGetFrame leftSideWindowID io1

    NotIsDummyFrame ((0,0),(0,0)) = False
    NotIsDummyFrame _             = True

  ////////// EditRewrite
  // Places the two sides of a rewrite in the two circuit windows to be edited.  Shouldn't be called unless
  // two windows are open.
  EditRewrite :: Rewrite ProgState IO -> (ProgState, IO)
  EditRewrite rewrite=:{leftComp, leftTop, leftMid, leftBot, oldRightSide}
              state=:{windowInfo=(window1State, window2State)} io1
    | confirmed = DrawInWindowFrame window2ID (RedrawCircuit Window2) newState2 io3
    | otherwise = (state, io2)
  where
    (confirmed, io2) = WindowClearOK

    WindowClearOK
      | (isEmpty window1State.circuit) && (isEmpty window2State.circuit)
          = (True, io1)
      | otherwise
          = Confirm "Are you sure?  The current contents of both windows will be erased." io1

    (newState2, io3) = DrawInWindowFrame window1ID (RedrawCircuit Window1) newState1
                                         (seq [ChangeWindowTitle window1ID "Window 1: Not saved",
                                               ChangeWindowTitle window2ID "Window 2: Not saved"
                                              ] io2
                                         )
    newState1 = {state & windowInfo=(newWindow1State, newWindow2State), itemSelected=False}

    newWindow1State = {window1State & circuit=leftCirc, wires=leftWires, compID=leftCompID,
                                      nextWireID=leftNextWireID, nextVar=leftNextVar, fileName=""
                      }
    leftCirc  = ReverseAppend leftTop [leftComp : (leftMid ++ leftBot)]
    leftWires = GetCircuitWires leftCirc
    (leftCompID, leftNextWireID, leftNextVar) = GetCircuitNextVals leftCirc

    newWindow2State = {window2State & circuit=oldRightSide, wires=rightWires, compID=rightCompID,
                                      nextWireID=rightNextWireID, nextVar=rightNextVar, fileName=""
                      }
    rightWires = GetCircuitWires oldRightSide
    (rightCompID, rightNextWireID, rightNextVar) = GetCircuitNextVals oldRightSide

  ////////// EditBoxRewrite
  // Places the two sides of the box rewrite in the two circuit windows for editing (with an unnamed
  // generic component for the box's internal circuit).  Shouldn't be called unless two windows are open.
  EditBoxRewrite :: ProgState IO -> (ProgState, IO)
  EditBoxRewrite state1=:{windowInfo=(window1State, window2State)} io1
    | confirmed = DrawInWindowFrame window1State.windowID (RedrawCircuit Window1) state2 io3
    | otherwise = (state1, io2)
  where
    (confirmed, io2) = WindowClearOK

    WindowClearOK
      | (isEmpty window1State.circuit) && (isEmpty window2State.circuit)
          = (True, io1)
      | otherwise
          = Confirm "Are you sure?  The current contents of both windows will be erased." io1

    (state2, io3)    = DrawInWindowFrame window2State.windowID (RedrawCircuit Window2)
                                         {state1 & itemSelected=False, windowInfo=newWindowInfo}
                                         (seq [ChangeWindowTitle window1ID "Window 1: Not saved",
                                               ChangeWindowTitle window2ID "Window 2: Not saved"
                                              ] io2
                                         )
    newWindowInfo    = ({window1State & circuit=boxReductLeftSide, wires=boxReductLeftWires, compID=boxReductLeftCompID,
                         nextWireID=boxReductLeftNextWireID, nextVar=boxReductLeftNextVar, fileName=""
                        },
                        {window2State & circuit=boxReductRightSide, wires=boxReductRightWires, compID=boxReductRightCompID,
                         nextWireID=boxReductRightNextWireID, nextVar=boxReductRightNextVar, fileName=""
                        }
                       )

  ////////// ResaveRewrite
  // Saves a new version of a user-defined rewrite that is already on the menu.  Works like SaveNewRewrite,
  // except that it doesn't ask for the name of the rewrite.
  ResaveRewrite :: String MenuItemId ProgState IO -> (ProgState, IO)
  ResaveRewrite _ _ state=:{itemSelected=False} io
    = (state, DisplayNotice "Please select the live wire on the left side of the rewrite." io)
  ResaveRewrite _ _ state=:{currentWire=(-1)} io
    = (state, DisplayNotice "Please select the live wire on the left side of the rewrite." io)
  ResaveRewrite rewriteName firstID state=:{currentWindow, windowInfo, files, nextID, currentWire} io
    | not (IsNotError returnCode) = (state, DisplayNotice (ErrorMessage returnCode) io)
    | not fileSelected            = ({state & files=files2}, io2)
    | not fileOpened              = ({state & files=files3}, DisplayNotice "Unable to open this file for writing." io2)
    | not fileClosed              = ({state & files=files4}, DisplayNotice "An error occurred closing the file." io2)
    | otherwise
        = ({state & files=files4, nextID=nextID+4},
           seq [ChangeMenuItemFunctions
                 [(firstID, DoProofRewrite rewrite), (firstID+1, DisplayRewrite rewrite), (firstID+2, EditRewrite rewrite)],
                DisplayNotice ("The rewrite rule has been saved using the circuit in " +++ windowName +++
                               " as the left side of the rule."
                              )
               ] io2
          )
  where
    (leftSide, rightSide, nextWireID)     = GetRewriteInfo currentWindow windowInfo
    ({windowName})                        = GetWindowState currentWindow windowInfo
    (returnCode, rewrite)                 = CreateRewrite rewriteName leftSide rightSide currentWire nextWireID
    (fileSelected, fileName, files2, io2) = SelectOutputFile "Save Rewrite As:" "" files io
    (fileOpened, file1, files3)           = fopen fileName FWriteText files2
    file2                                 = SaveRewrite rewrite file1
    (fileClosed, files4)                  = fclose file2 files3

    ErrorMessage LeftSideNotWired      = "The left side of this rule is not fully connected."
    ErrorMessage RightSideNotWired     = "The right side of this rule is not fully connected."
    ErrorMessage BadLeftSideTerminals  = "There are terminals in the boxes on the left side of this rule."
    ErrorMessage BadRightSideTerminals = "There are terminals in the boxes on the right side of this rule."
    ErrorMessage InputCountMismatch    = "The two sides of the rule do not have the same number of inputs."
    ErrorMessage OutputCountMismatch   = "The two sides of the rule do not have the same number of outputs."
    ErrorMessage NotConnected          = "The left side of the rule must be completely connected."
    ErrorMessage BadLiveWire           = "The live wire must not enter, leave or be inside a box."
    ErrorMessage TypeMismatch          = "The types of the inputs and outputs of the two sides of the rule don't match."

  ////////// ShowType
  // Display the type on a wire or connection.
  ShowType state=:{itemSelected=True, currentWire, windowInfo, currentWindow} io
    | currentWire==(-1) = (state, io)
    | otherwise         = (state, DisplayNotice (WireTypeString wire.wireType) io)
  where
    ({wires}) = GetWindowState currentWindow windowInfo
    wire = GetFirst (((==) currentWire) o GetWireID) wires
  ShowType state io = (state, io)

  ////////// FILLSequent
  // Checks whether the circuit in the currently selected window sequentializes.
  FILLSequent :: ProgState IO -> (ProgState, IO)
  FILLSequent state=:{currentWindow, windowInfo} io
    | NotWindow currentWindow
        = (state, DisplayNotice "No window is selected.  Click in a window to select it." io)
    | success
        = (state,
           DisplayNotice ("The circuit in " +++ windowName +++ " sequentializes to " +++ (SequentString sequentCirc wires)
                          +++ " (in FILL)."
                         ) io
          )
    | otherwise
        = (state, DisplayNotice ("The circuit in " +++ windowName +++ " does not sequentialize (in FILL).") io)
  where
    {circuit, wires, windowName} = GetWindowState currentWindow windowInfo
    (success, sequentCirc) = FILLSequentialize circuit

  ////////// CheckEquality  //*^* Should check for equality of types?
  // Checks whether two circuits are equivalent in shape and order of inputs and outputs,
  // *but* does not check that they have equivalent input and output types (which might
  // not be the case if there are any user-specified types.  This function should
  // only be called in ProofMode (it won't give the right answer if either circuit is
  // not completely in one piece).
  CheckEquality :: ProgState IO -> (ProgState, IO)

  CheckEquality state=:{windowInfo=(windowState1, windowState2)} io
    | not (windowState1.windowOpen && windowState2.windowOpen)
        = (state, DisplayNotice "You must have both windows open to do an equality check." io)
    | (isEmpty topPairs) && (isEmpty bottomPairs)
        = (state, DisplayNotice "An equality check can't be done with a circuit that has no inputs or outputs." io)
    | success
        = (state, DisplayNotice "The two circuits are equal." io)
    | otherwise
        = (state, DisplayNotice "The two circuits are *not* equal." io)
  where
    topPairs    = zip ((map (GetConnectWireID o snd) windowState1.inputPairs),
                       (map (GetConnectWireID o snd) windowState2.inputPairs)
                      )
    bottomPairs = zip ((map (GetConnectWireID o snd) windowState1.outputPairs),
                       (map (GetConnectWireID o snd) windowState2.outputPairs)
                      )
    (wireID1, wireID2) = if (isEmpty bottomPairs)
                            (hd topPairs)
                            (hd bottomPairs)

    (success, _, _, _)
       = MatchCircuits wireID1 windowState1.circuit wireID2 windowState2.circuit [] [] (topPairs ++ bottomPairs)

  ////////// CheckConnects
  //*^* Used for debugging only - checks that every output in the current window's circuit is either
  //*^* not connected or is connected to the input of a component lower (later) in the circuit.
  CheckConnects :: ProgState IO -> (ProgState, IO)
  CheckConnects state=:{currentWindow, windowInfo} io
    | NotWindow currentWindow
        = (state, DisplayNotice "No window is selected.  Click in a window to select it." io)
    | success && (isEmpty leftovers)
        = (state, DisplayNotice ("The circuit in " +++ windowName +++ " is properly connected.") io)
    | otherwise
        = (state, DisplayNotice ("The circuit in " +++ windowName +++ " is *not* properly connected.") io)
  where
    {circuit, windowName} = GetWindowState currentWindow windowInfo
    (success, leftovers)  = CheckConnections circuit []

  ////////// AddWire
  // This function is called to add a new wire connecting two components.  It displays an error message if
  // an attempt is made to add a wire between two connections whose types don't unify.
  //
  // Takes as arguments:
  //   - a tuple giving the connection type (IN for input, OUT for output), component ID, an integer identifying
  //     which of the component's input or output connections was selected, and the connection's type for the
  //     first connection,
  //   - a tuple giving the same information for the second connection,
  //   - the line starting at the first connection point and ending at the second connection point,
  //   - the circuit in which to add the wire,
  //   - the circuit's wires,
  //   - the wire ID to give the new wire,
  //   - the io, for displaying a message.
  // Returns:
  //   - True if the connection could be made, False otherwise (if the connection didn't unify, caused a cycle
  //     or is between two input connections or two output connections).
  //   - The new circuit if the wire could be added,
  //   - The new circuit's wires,
  //   - The new io.
  AddWire :: ConnectionInfo ConnectionInfo Line Circuit [Wire] WireID IO -> (Bool, Circuit, [Wire], IO)

  AddWire (OUT, compOutID, connectOutID, outWireType) (IN, compInID, connectInID, inWireType) line circuit wires
          newWireID io
    | not typesUnify
        = (False, [], [], DisplayNotice "This connection doesn't unify." io)
    | noProblem
        = (True, SubsIntoCircuit subs newCircuit, SubsIntoWires subs newWires, io)
    | otherwise
        = (False, [], [], io)
  where
    (typesUnify, subs) = Unify (GetType outWireType) (GetType inWireType)
    (noProblem, newCircuit)
      = AddConnections (OUT, compOutID, connectOutID, outWireType) (IN, compInID, connectInID, inWireType) newWireID line
                       circuit
    newWires = [{wireID = newWireID, wireLine = line, wireType = outWireType} : wires]

  AddWire (IN, compInID, connectInID, inWireType) (OUT, compOutID, connectOutID, outWireType) (point1,point2) circuit wires
          newWireID io
    | not typesUnify
        = (False, [], [], DisplayNotice "This connection doesn't unify." io)
    | noProblem
        = (True, SubsIntoCircuit subs newCircuit, SubsIntoWires subs newWires, io)
    | otherwise
        = (False, [], [], io)
  where
    (typesUnify, subs) = Unify (GetType outWireType) (GetType inWireType)
    (noProblem, newCircuit)
      = AddConnections (OUT, compOutID, connectOutID, outWireType) (IN, compInID, connectInID, inWireType) newWireID
                       (point2, point1) circuit
    newWires = [{wireID = newWireID, wireLine = (point2, point1), wireType = outWireType} : wires]

  AddWire _ _ _ _ _ _ io  = (False, [], [], io)

  ////////// AddConnections
  // Takes information about two connections (the output connection first), the wire ID for the new wire, the
  // line from the first connection point to the second connection point, and a circuit, and searches for the
  // first or second connection, whichever it finds first in the circuit.  When it finds one connection, it
  // calls AddConnection to change the wire ID on the other connection.  If it finds the second connection
  // first, it must also insert this component into the bottom part of the circuit using InsertUnboxed,
  // which will check that no cycles have been created.  Returns True if it was successful (no cycle resulted
  // from adding the new wire) and False otherwise, and the new circuit if it was successful.
  AddConnections :: ConnectionInfo ConnectionInfo WireID Line Circuit -> (Bool, Circuit)

  AddConnections (OUT, compOutID, connectOutID, outWireType) (IN, compInID, connectInID, inWireType) wireID (start, end)
                 [box=:{spec=Box boxCircuit, id, pos=RCT boxRect} : circuit]
    | compOutID==id                               // First connection found
        = if (compOutID==compInID)
             (if (connectOutID==0)                // Indicates internal box out connection
                 (True, bothConnectsOnBox)
                 (False, [])
             )
             (if (IsInRectangle end boxRect)
                 (if (connectOutID==0)
                     (True, c1OnBoxC2InBox)
                     (False, [])
                 )
                 (True, c1OnBoxC2InCircuit)
             )
    | compInID==id                              // Second connection found
        = if (IsInRectangle start boxRect)
             (True, c1InBoxC2OnBox)
             (boxInserted1, c1InCircuitC2OnBox)
    | (IsInRectangle start boxRect)
        = if (IsInRectangle end boxRect)
             (success1, c1AndC2InBox)
             (True, c1InBoxC2InCircuit)
    | (IsInRectangle end boxRect)
         = (boxInserted2, c1InCircuitC2InBox)
    | otherwise
        = (success2, c1AndC2InCircuit)
  where
    bothConnectsOnBox
      = [SetConnection (SetConnection box (OUT, compOutID, connectOutID, outWireType) wireID)
                       (IN, compInID, connectInID, inWireType) wireID
         : circuit
        ]
    c1OnBoxC2InBox
      = [SetConnection {box & spec=Box boxCircC2} (OUT, compOutID, connectOutID, outWireType) wireID : circuit]
    c1OnBoxC2InCircuit = [SetConnection box (OUT, compOutID, connectOutID, outWireType) wireID : circC2]
    c1InBoxC2OnBox
      = [SetConnection {box & spec=Box boxCircC1} (IN, compInID, connectInID, inWireType) wireID : circuit]
    (boxInserted1, c1InCircuitC2OnBox)
      = InsertUnboxed (SetConnection box (IN, compInID, connectInID, inWireType) wireID) circC1
    c1AndC2InBox       = [{box & spec=Box boxCircBoth} : circuit]
    c1InBoxC2InCircuit = [{box & spec=Box boxCircC1} : circC2]
    (boxInserted2, c1InCircuitC2InBox)
      = InsertUnboxed {box & spec=Box boxCircC2} circC1
    c1AndC2InCircuit   = [box : circBoth]
    boxCircC1          = AddConnection (OUT, compOutID, connectOutID, outWireType) wireID start boxCircuit
    boxCircC2          = AddConnection (IN, compInID, connectInID, inWireType) wireID end boxCircuit
    circC1             = AddConnection (OUT, compOutID, connectOutID, outWireType) wireID start circuit
    circC2             = AddConnection (IN, compInID, connectInID, inWireType) wireID end circuit
    (success1, boxCircBoth)
      = AddConnections (OUT, compOutID, connectOutID, outWireType) (IN, compInID, connectInID, inWireType) wireID
                       (start, end) boxCircuit
    (success2, circBoth)
      = AddConnections (OUT, compOutID, connectOutID, outWireType) (IN, compInID, connectInID, inWireType) wireID
                       (start, end) circuit

  AddConnections (OUT, compOutID, connectOutID, outWireType) (IN, compInID, connectInID, inWireType) wireID (start, end)
                 [comp=:{id=compID} : circuit]
    | compOutID==compID
        = if (compOutID==compInID)
             (False, [])
             (True, c1OnCompC2InCirc)
    | compInID==compID
        = (compInserted, c1InCircC2OnComp)
    | otherwise
        = (success, [comp : circBoth])
  where
    c1OnCompC2InCirc
      = [SetConnection comp (OUT, compOutID, connectOutID, outWireType) wireID : circC2]
    (compInserted, c1InCircC2OnComp)
      = InsertUnboxed (SetConnection comp (IN, compInID, connectInID, inWireType) wireID) circC1
    circC1 = AddConnection (OUT, compOutID, connectOutID, outWireType) wireID start circuit
    circC2 = AddConnection (IN, compInID, connectInID, inWireType) wireID end circuit
    (success, circBoth)
      = AddConnections (OUT, compOutID, connectOutID, outWireType) (IN, compInID, connectInID, inWireType) wireID
                       (start, end) circuit

  ////////// AddConnection
  // Takes the connection information for one connection of a wire, the new wire ID, the connection point and
  // the circuit, and returns the circuit with the connection's wire ID changed.
  AddConnection :: ConnectionInfo WireID Point Circuit -> Circuit

  AddConnection connect=:(_, compID, _, _) wireID point [comp=:{spec=Box boxCircuit, id, pos=RCT boxRect} : circuit]
    | compID==id                  = connectOnBoxCirc
    | IsInRectangle point boxRect = connectInBoxCirc
    | otherwise                   = connectInCircuitCirc
  where
    connectOnBoxCirc     = [SetConnection comp connect wireID : circuit]
    newBoxCircuit        = AddConnection connect wireID point boxCircuit
    connectInBoxCirc     = [{comp & spec=Box newBoxCircuit} : circuit]
    connectInCircuitCirc = [comp : AddConnection connect wireID point circuit]

  AddConnection connect=:(_, compID, _, _) wireID point [comp=:{id} : circuit]
    | compID==id = [SetConnection comp connect wireID : circuit]
    | otherwise  = [comp : AddConnection connect wireID point circuit]

  ////////// SetConnection
  SetConnection :: Component ConnectionInfo WireID -> Component
  SetConnection comp=:{inputs} (IN, _, connectID, wireType) wireID
    = {comp & inputs = Replace connectID inputs {cWireID=wireID, cWireType=wireType}}
  SetConnection comp=:{outputs} (OUT, _, connectID, wireType) wireID
    = {comp & outputs = Replace connectID outputs {cWireID=wireID, cWireType=wireType}}

  ////////// NormalizeRectangle
  // Takes a rectangle represented by two opposite corner points and returns a
  // rectangle represented by the top-left corner point and the bottom-right
  // corner point, in that order.
  NormalizeRectangle :: Rectangle -> Rectangle
  NormalizeRectangle ((x1, y1), (x2, y2))
    = ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))

  ////////// GetSelection
  // Takes a mouse position, a circuit and the circuit's wires, and returns True if the mouse position was close
  // enough to a component or wire in the circuit to select it (False otherwise), the ID of the selected wire if a wire
  // was selected (-1 otherwise), and the selected component if a component was selected (a dummy component otherwise).
  GetSelection :: Point Circuit [Wire] -> (Bool, WireID, Component)
  GetSelection point circuit wires
    | compSelected = (True, -1, selectedComp)
    | otherwise    = (wireSelected, selectedWire.wireID, dummyComponent)
  where
    (compSelected, selectedComp) = GetSelectedComp point circuit
    (wireSelected, selectedWire) = GetSelectedWire point wires

  ////////// GetSelectedComp
  // Takes a mouse position and a circuit and returns True if the mouse position was on a component in the circuit
  // (False otherwise) and the selected component if any.
  GetSelectedComp :: Point Circuit -> (Bool, Component)

  GetSelectedComp point [comp=:{spec=Box boxCircuit, pos=(RCT rectangle)} : circuit]
    | any (IsInRectangle point) (BoxEdgeRectangles rectangle)
        = (True, comp)
    | (IsInRectangle point rectangle)
        = GetSelectedComp point boxCircuit
    | otherwise
        = GetSelectedComp point circuit

  GetSelectedComp point [comp : circuit]
    | IsInRectangle point (ComponentRectangle comp)
        = (True, comp)
    | otherwise
        = GetSelectedComp point circuit

  GetSelectedComp _ [] = (False, dummyComponent)

  ////////// GetSelectedWire
  // Takes a mouse position and a circuit's wires and returns True if the mouse position was close to one of
  // the wires (False otherwise) and the selected wire if any.
  GetSelectedWire :: Point [Wire] -> (Bool, Wire)
  GetSelectedWire point [wire=:{wireLine} : wires]
    | IsOnLine point wireLine = (True, wire)
    | otherwise               = GetSelectedWire point wires
  GetSelectedWire _ [] = (False, dummyWire)

  ////////// ChangeComponent
  // Takes as arguments:
  //   - the enclosing rectangle for the component before it was moved,
  //   - the newly moved component,
  //   - the circuit (including the component in its old position).
  // Returns:
  //   - True if the component could be moved to its new position without causing a cycle, False otherwise,
  //   - the new circuit.
  //
  // First calls ChangeComponentAux, which searches the circuit for the component or a box containing the
  // component.  If ChangeComponentAux finds the component, it removes it and sets removed=True so that
  // it can be reinserted into the circuit.  If the component's old and new positions are both in a box,
  // ChangeComponentAux calls ChangeComponent recursively to move the component in the box, and returns
  // removed=False to indicate that it is done.  If the component's old position is in a box but not its
  // new position, it removes the component from the box and sets removed=True so that it can be
  // reinsertion.  If a cycle is caused by the move, reinsertion of the component will fail.
  //*^* Note that it isn't necessary to have to old component's entire enclosing rectangle to determine
  //*^* whether it was in a box - a single point on the component would be sufficient.
  ChangeComponent :: Rectangle Component Circuit -> (Bool, Circuit)
  ChangeComponent oldCompRect newcomp circuit
    | removed
        = (inserted, newCircuit2)
    | noProblem
        = (True, newCircuit1)
    | otherwise
        = (False, [])
  where
    (noProblem, removed, newCircuit1) = ChangeComponentAux oldCompRect newcomp circuit
    (inserted, newCircuit2)           = InsertComponent newcomp newCircuit1

  ////////// ChangeComponentAux           // (noProblem, removed, newCircuit)
  ChangeComponentAux :: Rectangle Component Circuit -> (Bool, Bool, Circuit)
  ChangeComponentAux oldCompRect newcomp=:{id=newcompID} [box=:{spec=Box boxCircuit, id=boxID, pos=(RCT boxRect)} : circuit]
    | newcompID==boxID
        = (True, True, circuit)
    | Encloses boxRect oldCompRect
        = if (Encloses boxRect (ComponentRectangle newcomp))
             (if changedInBox
                 (True, False, [{box & spec=Box newBoxCircuit1} : circuit])
                 (False, False, [])
             )
             (True, True, [{box & spec=Box newBoxCircuit2} : circuit])
    | otherwise
        = (noProblem, removed, [box : newCircuit])
  where
    (changedInBox, newBoxCircuit1)   = ChangeComponent oldCompRect newcomp boxCircuit
    newBoxCircuit2                   = MoveComponentOut oldCompRect newcompID boxCircuit
    (noProblem, removed, newCircuit) = ChangeComponentAux oldCompRect newcomp circuit

  ChangeComponentAux oldCompRect newcomp=:{id=newcompID} [comp=:{id=compID} : circuit]
    | newcompID==compID
        = (True, True, circuit)
    | otherwise
        = (noProblem, removed, [comp : newCircuit])
  where
    (noProblem, removed, newCircuit) = ChangeComponentAux oldCompRect newcomp circuit

  ////////// ChangeComponentInProof
  // Similar to ChangeComponent, except that when removing a component from a box, the outermost
  // box that it is removed from is checked for sequentialization after the removal, and when
  // inserting a component into a box, the innermost box that it is inserted into is checked for
  // sequentialization after the insertion.  This should ensure that if the whole circuit
  // sequentialized before the component was moved, it will still sequentialize.
  ChangeComponentInProof :: Rectangle Component Circuit -> (Bool, Circuit)
  ChangeComponentInProof oldCompRect newcomp circuit
    | not noProblem = (False, [])
    | removed       = (inserted, newCircuit2)
    | otherwise     = (True, newCircuit1)
  where
    (noProblem, removed, newCircuit1)
      = ChangeComponentInProofAux oldCompRect newcomp circuit
    (inserted, _, newCircuit2)
      = InsertComponentInProof newcomp newCircuit1

  ////////// ChangeComponentInProofAux
  ChangeComponentInProofAux :: Rectangle Component Circuit -> (Bool, Bool, Circuit)

  // Checks that outer box from which component is removed sequentializes.
  ChangeComponentInProofAux oldCompRect newcomp=:{id=newcompID}
                            [box=:{spec=Box boxCircuit, id=boxID, pos=(RCT boxRect)} : circuit]
    | newcompID==boxID
        = (True, True, circuit)
    | Encloses boxRect oldCompRect
        = if (Encloses boxRect (ComponentRectangle newcomp))
             (if changedInBox
                 (True, False, [{box & spec=Box newBoxCircuit1} : circuit])
                 (False, False, [])
             )
             (if sequentializes
                 (True, True, [newBox2 : circuit])
                 (False, False, [])
             )
    | otherwise
        = (noProblem, removed, [box : newCircuit])
  where
    (changedInBox, newBoxCircuit1)   = ChangeComponentInProof oldCompRect newcomp boxCircuit
    newBoxCircuit2                   = MoveComponentOut oldCompRect newcompID boxCircuit
    newBox2                          = {box & spec=Box newBoxCircuit2}
    (sequentializes, _)              = FILLSequentialize [newBox2]
    (noProblem, removed, newCircuit) = ChangeComponentInProofAux oldCompRect newcomp circuit

  ChangeComponentInProofAux oldCompRect newcomp=:{id=newcompID} [comp=:{id=compID} : circuit]
    | newcompID==compID
        = (True, True, circuit)
    | otherwise
        = (noProblem, removed, [comp : newCircuit])
  where
    (noProblem, removed, newCircuit) = ChangeComponentInProofAux oldCompRect newcomp circuit

  ////////// MoveComponentOut
  // Takes the enclosing rectangle of a component, its ID, and a circuit, and removes the
  // component from the circuit (including the box circuit if the component is a box).
  MoveComponentOut :: Rectangle ComponentID Circuit -> Circuit

  MoveComponentOut compRect compID [box=:{spec=Box boxCircuit, id=boxID, pos=RCT boxRect} : circuit]
    | (compID==boxID)           = circuit
    | Encloses boxRect compRect = [{box & spec=Box (MoveComponentOut compRect compID boxCircuit)} : circuit]
    | otherwise                 = [box : MoveComponentOut compRect compID circuit]

  MoveComponentOut compRect compID [comp=:{id} : circuit]
    | compID==id = circuit
    | otherwise  = [comp : MoveComponentOut compRect compID circuit]

  ////////// MoveWires
  // Takes a component that has been moved and a list of circuit wires, and moves the endpoints of the wires
  // attached to the component to their new positions, returning the new list of wires and a list of
  // drawing functions used to erase the old wires and draw the new ones.
  MoveWires :: Component [Wire] -> ([Wire], [DrawFunction])
  MoveWires component wires
    = MoveWiresAux (ComponentConnections component) wires []
  where
    MoveWiresAux :: [(ConnectionType, WireID, Point)] [Wire] [DrawFunction] -> ([Wire], [DrawFunction])

    MoveWiresAux [connect : connects] wires redraws
      = MoveWiresAux connects newWires (newRedraws ++ redraws)
    where
      (newWires, newRedraws) = MoveWire connect wires

    MoveWiresAux [] wires redraws = (wires, redraws)

  ////////// MoveWire
  // Takes information about a wire connection and a list of wires, and changes the
  // wire so that the connection endpoint is in the new position given, returning
  // a list of drawing functions to erase the old wire and draw the new one.
  MoveWire :: (ConnectionType, WireID, Point) [Wire] -> ([Wire], [DrawFunction])

  MoveWire (OUT, movedWireID, newPoint) [wire=:{wireID, wireLine=(_, end)} : wires]
    | movedWireID==wireID = ([newWire : wires], (DrawWire wire) ++ (DrawWire newWire))
    | otherwise           = ([wire : newWires], redrawWires)
  where
    newWire                 = {wire & wireLine=(newPoint, end)}
    (newWires, redrawWires) = MoveWire (OUT, movedWireID, newPoint) wires

  MoveWire (IN, movedWireID, newPoint) [wire=:{wireID, wireLine=(start, _)} : wires]
    | movedWireID==wireID = ([newWire : wires], (DrawWire wire) ++ (DrawWire newWire))
    | otherwise           = ([wire : newWires], redrawWires)
  where
    newWire                 = {wire & wireLine=(start, newPoint)}
    (newWires, redrawWires) = MoveWire (IN, movedWireID, newPoint) wires

  ////////// GetInternalComponents
  // Takes a rectangle giving the border of a box and a circuit, and returns a list
  // all components in the circuit that are inside the rectangle and the remainder
  // of the circuit otherwise unchanged.
  GetInternalComponents :: Rectangle Circuit -> ([Component], Circuit)
  GetInternalComponents rectangle [] = ([], [])
  GetInternalComponents rectangle [comp=:{spec=Box boxCircuit, pos=(RCT boxRect)} : circuit]
    | Encloses rectangle boxRect
        = ([comp : inBox1], newCircuit1)
    | Encloses boxRect rectangle
        = (inBox2, [{comp & spec=Box newBoxCircuit} : circuit])
    | otherwise
        = (inBox1, [comp : newCircuit1])
  where
    (inBox1, newCircuit1) = GetInternalComponents rectangle circuit
    (inBox2, newBoxCircuit) = GetInternalComponents rectangle boxCircuit

  GetInternalComponents rectangle [comp : circuit]
    | Encloses rectangle (ComponentRectangle comp)
        = ([comp : inBox], newCircuit)
    | otherwise
        = (inBox, [comp : newCircuit])
  where
    (inBox, newCircuit) = GetInternalComponents rectangle circuit

  ////////// ResizeBox
  // Takes as arguments:
  //   - a value indicating which corner of the box was selected for sizing,
  //   - the box's placement (with border rectangle) before sizing,
  //   - the mouse position when the box was last sized,
  //   - the current mouse position.
  // Returns:
  //   - the box's new placement, with the size adjusted by the difference between
  //     the current mouse position and the last mouse position in the direction
  //     indicated.
  ResizeBox :: !SizeDirection !Placement !Point !Point -> Placement
  ResizeBox TopLeft (RCT ((leftX, topY), (rightX, bottomY))) (x1,y1) (x2,y2)
    = RCT ((leftX + (x2-x1), topY + (y2-y1)), (rightX, bottomY))
  ResizeBox TopRight (RCT ((leftX, topY), (rightX, bottomY))) (x1,y1) (x2,y2)
    = RCT ((leftX, topY + (y2-y1)), (rightX + (x2-x1), bottomY))
  ResizeBox BottomLeft (RCT ((leftX, topY), (rightX, bottomY))) (x1,y1) (x2,y2)
    = RCT ((leftX + (x2-x1), topY), (rightX, bottomY + (y2-y1)))
  ResizeBox BottomRight (RCT ((leftX, topY), (rightX, bottomY))) (x1,y1) (x2,y2)
    = RCT ((leftX, topY), (rightX + (x2-x1), bottomY + (y2-y1)))

  ////////// ChangeSizedBox
  // Changes the contents and wire positions of a box that has been sized and reinserts it into the circuit.
  //
  // Takes as arguments:
  //   - the box before sizing,
  //   - the newly sized box (with its old internal circuit),
  //   - the circuit (including the box before sizing),
  //   - the circuit's wires,
  // Returns:
  //   - True if sizing the box didn't result in a cycle and didn't cause it to overlap part of another component,
  //     False otherwise,
  //   - the newly sized box with its new internal circuit if there was no problem,
  //   - the new circuit (including the new box),
  //   - the new circuit's wires,
  //   - a list of drawing functions to erase the box's old wires and draw the new ones.
  ChangeSizedBox :: Component !Component Circuit [Wire] -> (Bool, Component, Circuit, [Wire], [DrawFunction])
  ChangeSizedBox oldbox newbox1=:{spec=Box boxCircuit, pos=RCT boxRect} circuit wires
    | boxOverlaps = (False, dummyComponent, [], [], [])
    | success     = (True, newbox3, newCircuit, newWires, redraws)
    | otherwise   = (False, dummyComponent, [], [], [])
  where
    (newBoxCircuit, removedComps) = filterInOut ((Encloses boxRect) o ComponentRectangle) boxCircuit
    newbox2 = {newbox1 & spec=Box newBoxCircuit}
    boxOverlaps
      = any ((flip any) (ComponentRectangles newbox2) o Overlaps)
            (flatten (map ComponentRectangles (removedComps ++ (RemoveWholeComp oldbox circuit))))
    (success, newbox3, newCircuit) = ChangeBoxInCircuit newbox2 removedComps circuit
    (newWires, redraws) = MoveWires {newbox3 & spec=Box []} wires

  ////////// ChangeBoxInCircuit
  // Calls CollectBoxContents, which collects the box's new contents, removing them and
  // the box from the circuit, and sets done=False if it finds the box at the current
  // level, and makes the changes to the box and the circuit at a lower level and sets
  // done=True if it finds that the box is inside another box.  If the box was at the
  // current level, it and the components removed from its internal circuit must all
  // be reinserted.  This will fail if there is a cycle.
  ChangeBoxInCircuit :: !Component [Component] !Circuit -> (Bool, Component, Circuit)
  ChangeBoxInCircuit box removedComps circuit
    | success
        = if done
             (True, newBox, newCircuit1)
             (insertionsOK, newBox, newCircuit2)
    | otherwise = (False, dummyComponent, [])
  where
    (success, done, newBox, newCircuit1) = CollectBoxContents box removedComps circuit
    (insertionsOK, newCircuit2)          = InsertAll [newBox : removedComps] newCircuit1

  ////////// CollectBoxContents
  // Proceeds down the circuit until it finds the box, finds a box that encloses the box,
  // or finds a component that is now contained in the box.  If it finds the box, it
  // calls GetBackSubs to remove components following the box that are now inside it and put
  // them in the box, then returns the box to be reinserted.  If it finds a box that
  // encloses the box, it recursively calls CollectBoxContents to do the job.  If it finds
  // something that is inside the box, it calls GetFrontComps to remove all components
  // now in the box from the circuit and put them in the box, then returns the new box
  // for reinsertion.
  CollectBoxContents :: !Component [Component] !Circuit -> (Bool, Bool, Component, Circuit)

  CollectBoxContents box1=:{id=id1, spec=Box boxCircuit1, pos=RCT boxRect1} removedComps
                     [box2=:{id=id2, spec=Box boxCircuit2, pos=RCT boxRect2} : circuit]
    | id1==id2
        = (True, False, newBox1, newCircuit1)
    | Encloses boxRect1 boxRect2
        = (True, False, newBox2, newCircuit2)
    | Encloses boxRect2 boxRect1
        = (success1, True, newBox3, [{box2 & spec=Box newBoxCircuit} : circuit])
    | otherwise
        = (success2, done, newBox4, [box2 : newCircuit3])
  where
    (newBox1, newCircuit1) = GetBackComps box1 circuit []
    (newBox2, newCircuit2) = GetFrontComps box1 circuit [box2]
    (success1, newBox3, newBoxCircuit) = ChangeBoxInCircuit box1 removedComps boxCircuit2
    (success2, done, newBox4, newCircuit3) = CollectBoxContents box1 removedComps circuit

  CollectBoxContents box=:{pos=RCT boxRect} removedComps [comp : circuit]
    | Encloses boxRect (ComponentRectangle comp)
        = (True, False, newBox1, newCircuit1)
    | otherwise
        = (success, done, newBox2, [comp : newCircuit2])
  where
    (newBox1, newCircuit1) = GetFrontComps box circuit [comp]
    (success, done, newBox2, newCircuit2) = CollectBoxContents box removedComps circuit

  ////////// ChangeSizedBoxInProof
  // Similar to ChangeSizedBox, but it verifies that a circuit that sequentialized before the
  // box was resized will still sequentialize by checking that the newly sized box with its
  // new contents still sequentializes.
  ChangeSizedBoxInProof :: Component !Component Circuit [Wire] -> (Bool, Component, Circuit, [Wire], [DrawFunction])
  ChangeSizedBoxInProof oldbox newbox1=:{spec=Box boxCircuit, pos=RCT boxRect} circuit wires
    | boxOverlaps = (False, dummyComponent, [], [], [])
    | success     = (True, newbox3, newCircuit, newWires, redraws)
    | otherwise   = (False, dummyComponent, [], [], [])
  where
    (newBoxCircuit, removedComps) = filterInOut ((Encloses boxRect) o ComponentRectangle) boxCircuit
    newbox2 = {newbox1 & spec=Box newBoxCircuit}
    boxOverlaps
      = any ((flip any) (ComponentRectangles newbox2) o Overlaps)
            (flatten (map ComponentRectangles (removedComps ++ (RemoveWholeComp oldbox circuit))))
    (success, newbox3, newCircuit) = ChangeBoxInCircuitInProof newbox2 removedComps circuit
    (newWires, redraws) = MoveWires {newbox3 & spec=Box []} wires

  ////////// ChangeBoxInCircuitInProof
  // Similar to ChangeBoxInCircuit, but it verifies that a circuit that sequentialized before
  // the box was resized will still sequentialize by checking that the newly sized box with its
  // new contents still sequentializes.
  ChangeBoxInCircuitInProof :: !Component [Component] !Circuit -> (Bool, Component, Circuit)
  ChangeBoxInCircuitInProof box removedComps circuit
    | success
        = if done
             (True, newBox, newCircuit1)
             (sequentializes && insertionsOK, newBox, newCircuit2)
    | otherwise = (False, dummyComponent, [])
  where
    (success, done, newBox, newCircuit1) = CollectBoxContentsInProof box removedComps circuit
    (sequentializes, _)                  = FILLSequentialize [newBox]
    (insertionsOK, newCircuit2)          = InsertAll [newBox : removedComps] newCircuit1

  ////////// CollectBoxContentsInProof
  // Similar to CollectBoxContents, but it verifies that a circuit that sequentialized before
  // the box was resized will still sequentialize by checking that the newly sized box with its
  // new contents still sequentializes.
  CollectBoxContentsInProof :: !Component [Component] !Circuit -> (Bool, Bool, Component, Circuit)

  CollectBoxContentsInProof box1=:{id=id1, spec=Box boxCircuit1, pos=RCT boxRect1} removedComps
                     [box2=:{id=id2, spec=Box boxCircuit2, pos=RCT boxRect2} : circuit]
    | id1==id2
        = (True, False, newBox1, newCircuit1)
    | Encloses boxRect1 boxRect2
        = (True, False, newBox2, newCircuit2)
    | Encloses boxRect2 boxRect1
        = (success1, True, newBox3, [{box2 & spec=Box newBoxCircuit} : circuit])
    | otherwise
        = (success2, done, newBox4, [box2 : newCircuit3])
  where
    (newBox1, newCircuit1) = GetBackComps box1 circuit []
    (newBox2, newCircuit2) = GetFrontComps box1 circuit [box2]
    (success1, newBox3, newBoxCircuit) = ChangeBoxInCircuitInProof box1 removedComps boxCircuit2
    (success2, done, newBox4, newCircuit3) = CollectBoxContentsInProof box1 removedComps circuit

  CollectBoxContentsInProof box=:{pos=RCT boxRect} removedComps [comp : circuit]
    | Encloses boxRect (ComponentRectangle comp)
        = (True, False, newBox1, newCircuit1)
    | otherwise
        = (success, done, newBox2, [comp : newCircuit2])
  where
    (newBox1, newCircuit1) = GetFrontComps box circuit [comp]
    (success, done, newBox2, newCircuit2) = CollectBoxContentsInProof box removedComps circuit

  ////////// GetFrontComps
  // Takes as arguments:
  //   - a box,
  //   - a circuit,
  //   - a list of components that have been removed from the circuit because they are now in the box,
  //     in reverse order,
  // Returns:
  //   - the box with its new components from the circuit,
  //   - the circuit with components now in the box removed,
  //
  // Searches down the circuit, collecting components to be put into the box's circuit, until it finds
  // the original box.  It then removes the box and calls GetBackComps to collect the remaining
  // components to go in the box, then appends the components from before the box to the front of its
  // box circuit and returns the box and the new circuit.
  GetFrontComps :: !Component !Circuit [Component] -> (Component, Circuit)
  GetFrontComps box=:{id=boxID, pos=RCT boxRect} [comp=:{id} : circuit] frontComps
    | boxID==id
        = ({newBox1 & spec=Box (ReverseAppend frontComps newBoxCircuit)}, newCircuit1)
    | Encloses boxRect (ComponentRectangle comp)
        = GetFrontComps box circuit [comp : frontComps]
    | otherwise
        = (newBox2, [comp : newCircuit2])
  where
    (newBox1=:{spec=Box newBoxCircuit}, newCircuit1) = GetBackComps box circuit []
    (newBox2, newCircuit2) = GetFrontComps box circuit frontComps

  ////////// GetBackComps
  // Searches down the circuit, collecting components to be put into the box's circuit, until it
  // reaches the end of the circuit.  It then appends the new components to the end of the box's
  // circuit and returns the box and the new circuit.
  GetBackComps :: !Component !Circuit [Component] -> (Component, Circuit)

  GetBackComps box=:{pos=RCT boxRect} [comp : circuit] backComps
    | Encloses boxRect (ComponentRectangle comp)
        = GetBackComps box circuit [comp : backComps]
    | otherwise
        = (newBox, [comp : newCircuit])
  where
    (newBox, newCircuit) = GetBackComps box circuit backComps

  GetBackComps box=:{spec=Box boxCircuit} [] backComps
    = ({box & spec=Box (boxCircuit ++ (reverse backComps))}, [])

  ////////// InsertAll
  // Takes a list of components and a circuit and inserts all the components into the
  // top level of the circuit, returning True if it was successful (there were no
  // cycles) and False otherwise, with the new circuit.
  InsertAll :: ![Component] Circuit -> (Bool, Circuit)

  InsertAll [comp : comps] circuit
    | success   = InsertAll comps newCircuit
    | otherwise = (False, [])
  where
    (success, newCircuit) = InsertUnboxed comp circuit

  InsertAll [] circuit = (True, circuit)

  ////////// MakeComponent
  // Takes an edit type indicating what component is to be made, the component's ID, the point
  // that will be used to position the component, and the next free variable number, and
  // returns the component with its types set to the most general types possible using new
  // variables and the new next free variable number.
  MakeComponent :: EditType ComponentID Point Int -> (Component, Int)
  MakeComponent StartTerminalDraw compID point nextVar
    = ({spec=StartTerminal, id=compID, inputs=[], outputs=[NewConnection (Var nextVar)], pos=(PT point)}, nextVar+1)
  MakeComponent EndTerminalDraw compID point nextVar
    = ({spec=EndTerminal, id=compID, inputs=[NewConnection (Var nextVar)], outputs=[], pos=(PT point)}, nextVar+1)
  MakeComponent TensorEDraw compID point nextVar
    = ({spec=TensorE, id=compID, inputs=[NewConnection (Product (Var nextVar, Var (nextVar+1)))],
        outputs=[NewConnection (Var nextVar), NewConnection (Var (nextVar+1))], pos=(PT point)}, nextVar+2)
  MakeComponent TensorIDraw compID point nextVar
    = ({spec=TensorI, id=compID, inputs=[NewConnection (Var nextVar), NewConnection (Var (nextVar+1))],
        outputs=[NewConnection (Product (Var nextVar, Var (nextVar+1)))], pos=(PT point)}, nextVar+2)
  MakeComponent SumEDraw compID point nextVar
    = ({spec=SumE, id=compID, inputs=[NewConnection (Sum (Var nextVar, Var (nextVar+1)))],
        outputs=[NewConnection (Var nextVar), NewConnection (Var (nextVar+1))], pos=(PT point)}, nextVar+2)
  MakeComponent SumIDraw compID point nextVar
    = ({spec=SumI, id=compID, inputs=[NewConnection (Var nextVar), NewConnection (Var (nextVar+1))],
        outputs=[NewConnection (Sum (Var nextVar, Var (nextVar+1)))], pos=(PT point)}, nextVar+2)
  MakeComponent LollyDraw compID point nextVar
    = ({spec=Lolly, id=compID, inputs=[NewConnection (Var nextVar), NewConnection (Then (Var nextVar, Var (nextVar+1)))],
        outputs=[NewConnection (Var (nextVar+1))], pos=(PT point)}, nextVar+2)
  MakeComponent UnitIDraw compID point nextVar
    = ({spec=UnitI, id=compID, inputs=[], outputs=[NewConnection Unit], pos=(PT point)}, nextVar)
  MakeComponent UnitELDraw compID point nextVar
    = ({spec=UnitEL, id=compID, inputs=[NewConnection Unit, NewConnection (Var nextVar)],
        outputs=[NewConnection (Var nextVar)], pos=(PT point)}, nextVar+1)
  MakeComponent UnitERDraw compID point nextVar
    = ({spec=UnitER, id=compID, inputs=[NewConnection (Var nextVar), NewConnection Unit],
        outputs=[NewConnection (Var nextVar)], pos=(PT point)}, nextVar+1)
  MakeComponent CounitEDraw compID point nextVar
    = ({spec=CounitE, id=compID, inputs=[NewConnection Counit], outputs=[], pos=(PT point)}, nextVar)
  MakeComponent CounitILDraw compID point nextVar
    = ({spec=CounitIL, id=compID, inputs=[NewConnection (Var nextVar)],
        outputs=[NewConnection Counit, NewConnection (Var nextVar)], pos=(PT point)}, nextVar+1)
  MakeComponent CounitIRDraw compID point nextVar
    = ({spec=CounitIR, id=compID, inputs=[NewConnection (Var nextVar)],
        outputs=[NewConnection (Var nextVar), NewConnection Counit], pos=(PT point)}, nextVar+1)

  ////////// DeleteComponent
  // Takes a component and a circuit and removes the component from the circuit (leaving the box circuit
  // in if the component is a box), removing any of its wire connections, then retypes it and returns the
  // new circuit, its wires and the new next free variable number.
  DeleteComponent :: Component Circuit -> (Circuit, [Wire], Int)
  DeleteComponent comp=:{inputs, outputs} circuit = (newCircuit, newWires, newNextVar)
  where
    (_, newCircuit, newWires, newNextVar) = Retype (RemoveWires (inputs ++ outputs) (RemoveComp comp circuit))

  ////////// RemoveComp
  // Takes a component and a circuit that includes the component, and removes the component from the circuit.
  // If the component is a box, the box circuit is left in the circuit.
  RemoveComp :: Component Circuit -> Circuit

  RemoveComp comp1=:{id=compID} [comp2=:{spec=Box boxCircuit, id=boxID, pos=RCT rectangle} : circuit]
    | (compID==boxID) = boxCircuit ++ circuit
    | Encloses rectangle (ComponentRectangle comp1)
        = [{comp2 & spec=Box (RemoveComp comp1 boxCircuit)} : circuit]
    | otherwise
        = [comp2 : RemoveComp comp1 circuit]

  RemoveComp comp1=:{id=compID} [comp2=:{id} : circuit]
    | (compID==id) = circuit
    | otherwise    = [comp2 : RemoveComp comp1 circuit]

  ////////// RemoveWires
  // Takes a list of connections and a circuit, and changes all the corresponding connections in the circuit
  // so that they are no longer connected.
  RemoveWires :: [Connection] Circuit -> Circuit

  RemoveWires [connect=:{cWireID} : connects] circuit
    = RemoveWires connects (RemoveWireFromCircuit cWireID circuit)

  RemoveWires [] circuit = circuit

  ////////// RemoveWireFromCircuit
  // Takes a wire ID and a circuit and changes the connections corresponding to the wire ID so that it is
  // no longer connected (has a wire ID of -1).
  //*^* Note:  This function doesn't try to keep track of how many connections it has changed, which would
  //*^*        allow it to terminate early.
  RemoveWireFromCircuit :: WireID Circuit -> Circuit

  RemoveWireFromCircuit wireID [comp=:{spec=Box boxCircuit, inputs, outputs} : circuit]
    = [{comp & spec=Box newBoxCircuit, inputs=newInputs, outputs=newOutputs} : RemoveWireFromCircuit wireID circuit]
  where
    newBoxCircuit = RemoveWireFromCircuit wireID boxCircuit
    newInputs  = Disconnect wireID inputs
    newOutputs = Disconnect wireID outputs

  RemoveWireFromCircuit wireID [comp=:{inputs, outputs} : circuit]
    = [{comp & inputs=newInputs, outputs=newOutputs} : RemoveWireFromCircuit wireID circuit]
  where
    newInputs  = Disconnect wireID inputs
    newOutputs = Disconnect wireID outputs

  RemoveWireFromCircuit _ [] = []

  ////////// Disconnect
  // Takes a wire ID and a list of connections, and changes any connection identified by the wire ID
  // so that it is no longer connected (has cWireID=-1)
  Disconnect :: WireID [Connection] -> [Connection]
  Disconnect id [connect=:{cWireType=User type, cWireID} : connects]
    | id==cWireID = [{connect & cWireID=(-1), cWireType=Free type} : connects]
    | otherwise  = [connect : Disconnect id connects]
  Disconnect id [connect=:{cWireID} : connects]
    | id==cWireID = [{connect & cWireID=(-1)} : connects]
    | otherwise  = [connect : Disconnect id connects]
  Disconnect _ [] = []

  ////////// DeleteWire
  // Takes a wire ID and a circuit and removes the wire from the circuit, retypes the circuit and
  // returns the new circuit, its wires, and the new next free variable number.
  DeleteWire :: WireID Circuit -> (Circuit, [Wire], Int)
  DeleteWire wireID circuit = (newCircuit, newWires, newNextVar)
  where
    (_, newCircuit, newWires, newNextVar) = Retype (RemoveWireFromCircuit wireID circuit)

  //////////////////////////// ***

  ////////// DisplayNotice
  // Takes a string, breaks it into multiple lines and displays it in a simple message box.
  DisplayNotice :: String IO -> IO
  DisplayNotice str io = newIO
  where
    (_, newIO) = OpenNotice (Notice strs (NoticeButton noticeID "OK") []) io
    strs = map toString (SplitString (fromString str) 0)

  ////////// Confirm
  // Takes a string, breaks it into multiple lines and displays it in a dialog box with Yes and No
  // buttons - returns True if Yes was selected, No otherwise.
  Confirm :: String IO -> (Bool, IO)
  Confirm str io1 = (confirmID==yesID, io2)
  where
    (confirmID, io2) = OpenNotice (Notice strs (NoticeButton noID "No") [(NoticeButton yesID "Yes")]) io1
    strs = map toString (SplitString (fromString str) 0)

  ////////// SplitString
  // Takes a list of characters and breaks it into multiple lists of characters that are not too
  // long - breaks only at spaces.  (The integer indicates the length of the current line.)
  SplitString :: [Char] Int -> [[Char]]

  SplitString [] _ = [[]]

  SplitString str n1
    | (n1+n2) >= 60
       = [[], (word ++ str1A) : strsA]
    | otherwise
       = [(word ++ str1B) : strsB]
  where
    (word, rem, n2) = ScanWord str 0
    [str1A : strsA] = SplitString rem (n2+1)
    [str1B : strsB] = SplitString rem (n1+n2+1)

  ////////// ScanWord
  // Scans a word (a list of characters terminated by a space or the end of the list) off a list of
  // characters and returns the word, the remaining characters and the length of the word without the space.
  ScanWord :: [Char] Int -> ([Char], [Char], Int)

  ScanWord [' ' : cs] n = ([' '], cs, n)

  ScanWord [c : cs] n1
    = ([c : word], rem, n2)
  where
    (word, rem, n2) = ScanWord cs (n1+1)

  ScanWord [] n = ([], [], n)

  ////////// DisableAllWindows
  // Disables the mouse handler and keyboard handler for all open circuit windows and hides the cursor
  // (while a modal dialog box is open).
  DisableAllWindows :: (WindowState, WindowState) IO -> IO
  DisableAllWindows ({windowOpen=True, windowID=window1ID}, {windowOpen=True, windowID=window2ID}) io
    = seq [DisableMouse window1ID, DisableMouse window2ID, DisableKeyboard window1ID, DisableKeyboard window2ID,
           ChangeWindowCursor window1ID HiddenCursor, ChangeWindowCursor window2ID HiddenCursor] io
  DisableAllWindows ({windowOpen=True, windowID}, {windowOpen=False}) io
    = seq [DisableMouse windowID, DisableKeyboard windowID, ChangeWindowCursor windowID HiddenCursor] io
  DisableAllWindows ({windowOpen=False}, {windowOpen=True, windowID}) io
    = seq [DisableMouse windowID, DisableKeyboard windowID, ChangeWindowCursor windowID HiddenCursor] io
  DisableAllWindows _ io = io

  ////////// EnableAllWindows
  // Enables the mouse handler and keyboard handler for all open circuit windows and reveals the cursor
  // (while a modal dialog box is open).
  EnableAllWindows :: (WindowState, WindowState) IO -> IO
  EnableAllWindows ({windowOpen=True, windowID=window1ID}, {windowOpen=True, windowID=window2ID}) io
    = seq [EnableMouse window1ID, EnableMouse window2ID, EnableKeyboard window1ID, EnableKeyboard window2ID,
           ChangeWindowCursor window1ID StandardCursor, ChangeWindowCursor window2ID StandardCursor] io
  EnableAllWindows ({windowOpen=True, windowID}, {windowOpen=False}) io
    = seq [EnableMouse windowID, EnableKeyboard windowID, ChangeWindowCursor windowID StandardCursor] io
  EnableAllWindows ({windowOpen=False}, {windowOpen=True, windowID}) io
    = seq [EnableMouse windowID, EnableKeyboard windowID, ChangeWindowCursor windowID StandardCursor] io
  EnableAllWindows _ io = io

  ////////// Deselect
  // Disables menu items that can only be enabled when something is selected and erases the
  // selection indicator.
  Deselect :: WindowId Component WireID [Wire] IO -> IO
  Deselect windowID comp wireID wires io
    = seq [DisableMenuItems [deleteID, setTypeID, removeTypeID],
           DrawInWindow windowID [SetPenMode XorMode : DrawSelection comp wireID wires]
          ] io

  ////////// GetWindowState
  GetWindowState :: Window (WindowState, WindowState) -> WindowState
  GetWindowState Window1 (windowState, _) = windowState
  GetWindowState Window2 (_, windowState) = windowState

  ////////// PutWindowState
  PutWindowState :: Window WindowState (WindowState, WindowState) -> (WindowState, WindowState)
  PutWindowState Window1 windowState (_, win2State) = (windowState, win2State)
  PutWindowState Window2 windowState (win1State, _) = (win1State, windowState)

  ////////// AllOpen
  // Checks whether both windows are open.
  AllOpen :: (WindowState, WindowState) -> Bool
  AllOpen ({windowOpen=True}, {windowOpen=True}) = True
  AllOpen _ = False

  ////////// SameWindow
  SameWindow :: Window Window -> Bool
  SameWindow Window1 Window1 = True
  SameWindow Window2 Window2 = True
  SameWindow _       _       = False

  ////////// NotWindow
  NotWindow :: Window -> Bool
  NotWindow NoWindow = True
  NotWindow _        = False

/////////////////////////////////////

dummyConnect   :== {cWireID=(-1), cWireType=(Free Unit)}
dummyWire      :== {wireID=(-1), wireLine=((-1,-1),(-1,-1)), wireType=Free Unit}

windowPos    :== (100, 100)  // Gives the initial top-left corner position of the circuit windows.
thumbValue   :== 5           // Used in defining the circuit window scrollbars.
scrollValue  :== 5           // Used in defining the circuit window scrollbars.
initialDomain :== ((0,0), (MaxX, MaxY))  // The picture domain for the circuit windows
minWinSize   :== (50,50)                 // The minimum size for the circuit windows
initWinSize  :== (MaxX/2, 2*MaxY/3)      // The initial size for the circuit windows
rewriteWinSize :== (MaxX/3, MaxY/2)      // The initial size for the rewrite display windows
MaxX :== fst MaxScrollWindowSize         // The maximum width for the window.
MaxY :== snd MaxScrollWindowSize         // The maximum height for the window.

displayMargin :== 10         // The margin around the display circuit used when creating a new generic component.
displayTerminalDistance = 30 // The distance of the terminals from the new component in the display circuit.
