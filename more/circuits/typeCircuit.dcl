// Contains code used to completely retype a circuit after the deletion
// of a wire or component or the removal or change of a user-specified
// type.  It is assumed that component connections that aren't
// attached to a wire will have Free types and a User type will be kept
// on both component connections of the wire that carries it.

definition module typeCircuit

import circuitDefs

// Retypes the circuit, returning True if it was successful (all types
// unified), the retyped circuit, the new list of wires, and the
// new next free variable number.
Retype :: !Circuit -> (Bool, Circuit, [Wire], Int)
