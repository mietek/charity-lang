// Contains code used in rewiring thinning links in proof mode.

definition module rewire

import StdEnv, deltaPicture, circuitDefs

// Rewires a thinning link from one wire to another if possible.
//
// Takes as arguments:
//   - the thinning link to be rewired,
//   - the mouse position identifying the thinning link's new position,
//   - the circuit (including the thinning link),
//   - the circuit's wires,
//   - a list of position pairs giving the relative positions of the inputs in proof mode,
//   - a list of position pairs giving the relative positions of the outputs in proof mode.
// Returns:
//   - an ErrorVal indicating what error, if any, occurred,
//   - the new circuit (with the thinning link rewired),
//   - the new circuit's wires,
//   - the newly rewired thinning link,
//   - a list of functions to be applied to redraw the circuit.
//   - the new list of input position pairs, with wire IDs changed as necessary,
//   - the new list of output position pairs, with wire IDs changed as necessary.
Rewire :: Component Point Circuit ![Wire] [(Int, Connection)] [(Int, Connection)]
            -> (ErrorVal, Circuit, [Wire], Component, [DrawFunction], [(Int, Connection)], [(Int, Connection)])
