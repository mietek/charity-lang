// Contains code that determines whether two circuits are the same, starting
// from a pair of corresponding wires in the two circuits.

definition module circuitWireMatch

import StdEnv
import circuitDefs, rewriteDefs

// Takes as arguments:
//   - the wire ID from the first circuit,
//   - the first circuit,
//   - the wire ID from the second circuit,
//   - the second circuit,
//   - a list of matched wire ID pairs identifying wires from below the circuit that may connect into it,
//   - a list of matched wire ID pairs identifying wires from above the circuit that may connect into it,
//   - a list of wire ID pairs for all wires matched so far.
// Returns:
//   - True if the circuits are equal, False otherwise,
//   - a list of matched wire ID pairs identifying the wires from below or in the circuit that connect above it,
//   - a list of matched wire ID pairs identifying the wires from above or in the circuit that connect below it,
//   - a list of wire ID pairs for all wires matched.
// NOTE:  Does not check for equivalency of types.
MatchCircuits :: WireID !Circuit WireID Circuit [WireSub] [WireSub] [WireSub] -> (Bool, [WireSub], [WireSub], [WireSub])
