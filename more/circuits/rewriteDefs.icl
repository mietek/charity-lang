implementation module rewriteDefs

import StdEnv
import circuitDefs, utilities

:: WireSub :== (WireID, WireID)
:: Rewrite
     = {rewriteName    :: String,       // The name that appears on the menu for the rewrite.
        liveWireID     :: WireID,       // Identifies the wire that is selected when doing a rewrite
        leftComp       :: Component,    // The component on the left side with the live wire as output (not included below)
        leftTop        :: Circuit,      // The part of the circuit that connects down to leftComp, in reverse order
        leftMid        :: Circuit,      // The part of the circuit that doesn't connect down or up to leftComp (not reversed)
        leftBot        :: Circuit,      // The part of the circuit that connects up to leftComp (not reversed)
        rightSide      :: Circuit,      // - The right side of the rule, with start and end terminals removed.
                                        //   Input and output wire IDs match the left side except where wires are broken
                                        //   or connected in the rewrite - for connected wires, the wire ID matches the
                                        //   left-side input wire ID, and for a broken wire, the input wire ID matches the
                                        //   left side and the output wire ID is new.  Input and output wire types also
                                        //   match - where a substitution was required on the right side to make it
                                        //   match the left side, the wire has a "User" type.
        oldRightSide   :: Circuit,      // The original right-hand circuit (with terminals, original types, etc.)
        ruleRect       :: Rectangle,    // The enclosing rectangle for the right side (excluding terminals).
        inConnects     :: [Connection], // Rule inputs, excluding those for wires that become connected.
        outConnects    :: [Connection], // Rule outputs, excluding those for connected wires and broken wires
        connectedPairs :: [(WireID, WireID, WireType)], // The input and output wire IDs for wires that become connected
                                                        //  and their type
        brokenWireConnect :: Connection // In an expansion, this gives the output connection to the right side.
                                        // (*** Note that the latest version of DoRewrite doesn't use the type, so this
                                        // could just store the wire ID. ***)
       }

dummyRewrite :== {rewriteName="", liveWireID=(-1), leftComp=dummyComponent, leftTop=[], leftMid=[], leftBot=[],
                  rightSide=[], oldRightSide=[], ruleRect=((0,0),(0,0)), inConnects=[], outConnects=[], connectedPairs=[],
                  brokenWireConnect={cWireID=(-1), cWireType=Free Unit}
                 }

