// This module contains code for saving circuits to files and for reading them from files.

definition module circuitSave

import StdEnv, circuitDefs

startMark :== "Start\n"
endMark   :== "End\n"

SaveCircuit :: !Circuit ![Wire] !ComponentID !WireID !Int !*File -> *File
WriteCircuit :: !Circuit !*File -> *File
WriteComponent :: !Component !*File -> *File
WriteConnects :: ![Connection] !*File -> *File
WritePoint :: !Point !*File -> *File
WriteWireType :: !WireType !*File -> *File
WriteTypes :: ![Type] !*File -> *File

ReadCircuitFile :: !*File -> (Bool, Circuit, [Wire], ComponentID, WireID, Int, *File)
ReadStartMark :: !*File -> (Bool, *File)
ReadInt :: !*File -> (Bool, Int, *File)
ReadCircuit :: !*File -> (Bool, Circuit, *File)
ReadComponent :: !*File -> (Bool, Bool, Component, *File) // First Bool True if no problem, second True if endMark was read.
ReadPoint :: !*File -> (Bool, Point, *File)
ReadConnects :: !*File -> (Bool, [Connection], *File)
ReadConnect :: !*File -> (Bool, Bool, Connection, *File)  // First Bool True if no problem, second True if endMark was read.
ReadWireType :: !*File -> (Bool, WireType, *File)
ReadTypes :: !*File -> (Bool, [Type], *File)
