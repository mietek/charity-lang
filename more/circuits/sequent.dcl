// Contains the code used to check whether a circuit sequentializes, and also to 
// produce a string used to show what a circuit sequentialized to.

definition module sequent

import StdEnv, circuitDefs

// Takes a sequentialized circuit (should contain exactly one sequent box) and
// a list of wires, and returns a string of the form "<inputTypes> |- <outputTypes>"
// with the input and output types for the circuit sorted according to the
// horizontal position of the terminal endpoints of the corresponding wires.
SequentString :: !Circuit [Wire] -> String

// Sequentializes a circuit, returning True if it was successful with 
// the sequentialized circuit.
FILLSequentialize :: !Circuit -> (Bool, Circuit)

// Takes the front part of a circuit (in reverse order) that has been sequentialized as far as possible
// as its first argument, and the remainder of the circuit as its second, and returns True if no fatal
// errors were encountered (e.g. two sequent boxes connected by more than one wire, a box with wires
// exiting its interior, or a TensorE connected directly to a SumI), with the circuit sequentialized
// as far as possible.
FILLSequentAux :: Circuit !Circuit -> (Bool, Circuit)
