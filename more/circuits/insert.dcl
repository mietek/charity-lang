// This module contains code used to insert components into a circuit, in both modes
// (in proof mode, a sequentialization test must be done).  The circuit and all box
// circuits are kept in order, so any component that connects down from another
// component will be later in the circuit (list of components) than the second
// component.  All functions assumes that the original circuit is sorted.

definition module insert

import StdEnv, circuitDefs, utilities

// Inserts a component (which may need to be inserted into a box or boxes) into a circuit.
// Returns True if there was no problem (the insertion did not result in a cycle) and the
// new circuit.
InsertComponent :: Component !Circuit -> (Bool, Circuit)

// Like InsertComponent, but checks sequentialization of innermost box that the component is inserted into
// (in FILL, this should ensure that the whole circuit still sequentializes if it sequentialized before).
InsertComponentInProof :: Component !Circuit -> (Bool, Bool, Circuit)

// This inserts a component at the uppermost level of the circuit (it doesn't do
// insertion into boxes) and checks that the insertion doesn't result in a
// cycle.
InsertUnboxed :: Component !Circuit -> (Bool, Circuit)

// Checks if there are any direct connections from the first component down to the second.
ConnectsDown :: !Component Component -> Bool
