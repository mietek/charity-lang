// Contains code used in creating new rewrite rules and in performing a rewrite in a circuit.

definition module rewrite

import StdEnv
import circuitDefs, rewriteDefs

/////////// CreateRewrite
// Takes as arguments:
//   - the name of the new rewrite rule,
//   - the circuit for the left side of the rewrite rule,
//   - the circuit for the right side of the rewrite rule,
//   - the wire ID of the live wire (the wire that the user selects to indicate where in a circuit
//     the rewrite should take place) in the left side of the rewrite rule,
//   - the next unused wire ID for the left-side circuit (used to make sure that the only wire IDs
//     the two sides of the rule have in common are for their input and output wires).
// Returns:
//   - an error value indicating what error, if any, occurred,
//   - the new rewrite, if no error occurred.
CreateRewrite :: String !Circuit Circuit WireID WireID -> (ErrorVal, Rewrite)


/////////// DoRewrite
// Takes as arguments:
//   - the rewrite rule,
//   - the wire ID of the wire selected as the live wire in the circuit where the rewrite is to be done,
//   - the mouse position that identified the live wire in the circuit,
//   - the circuit,
//   - the circuit's list of wires,
//   - the next unused wire ID for the circuit,
//   - the next unused component ID for the circuit,
//   - the next unused variable number for the circuit,
//   - the list of sorted output position pairs giving the horizontal positions and the connections of
//     the circuit's outputs.
// Returns:
//   - an error value indicating what error, if any, occurred,
//   - the new circuit after the rewrite, if no error occurred,
//   - the new circuit's wires,
//   - the new next unused wire ID for the circuit,
//   - the new next unused component ID for the circuit,
//   - the new next unused variable number for the circuit,
//   - a list of drawing functions to be applied to the window picture,
//   - the new list of output position pairs,
DoRewrite :: !Rewrite WireID Point !Circuit [Wire] WireID ComponentID Int [(Int, Connection)]
              -> (ErrorVal, Circuit, [Wire], WireID, ComponentID, Int, [DrawFunction], [(Int, Connection)])

/////////// DoBoxRewrite
// Performs the standard box-reduction rewrite, which removes a box and a lolly (leaving the box
// circuit) if they have a connection from the external output of the box to the second input
// of the lolly, using the wire between the box and the lolly as the live wire.  If the wire from
// the box's internal output or the wire between the box and the lolly has a user-specified type,
// the new input wire to the box circuit must have a user-specified type, and if the wire into the
// box's internal input or the wire between the box and the lolly has a user-specified type, the
// new output wire to the box circuit must have a user-specified type.
//
// Takes as arguments:
//   - the ID of the live wire selected in the circuit to identify where the rewrite should take place,
//   - the mouse position that identified the selected wire,
//   - the circuit,
//   - the circuit's wires,
//   - the output position pairs used to keep track of the horizontal positions of the circuit's outputs.
// Returns:
//   - an error value indicating what error, if any, occurred,
//   - the new circuit after the rewrite is performed (if no error occurred),
//   - the new circuit's wires,
//   - a list of drawing functions to erase old components and wires and draw replacements,
//   - the new output position pairs, with wire IDs changed as necessary,
//   - a list giving the old and new wire IDs for the inputs of connections in the circuits
//     whose wire IDs must be changed because of the rewrite (only used after returning from
//     a box),
//   - a list of wire IDs identifying wires from the circuit above the rewrite that must 
//     have user-specified types (only used after returning from a box),
//   - a list of wire IDs identifying wires into the circuit below the rewrite that must
//     have user-specified types (only used after returning from a box).
DoBoxRewrite :: WireID Point !Circuit [Wire] [(Int, Connection)]
                 -> (ErrorVal, Circuit, [Wire], [DrawFunction], [(Int, Connection)], [WireSub], [WireID], [WireID])
