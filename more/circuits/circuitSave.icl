// This module contains code for saving circuits to files and for reading them from files.

implementation module circuitSave

import StdEnv, circuitDefs, typeDefs, utilities

startMark :== "Start\n"
endMark   :== "End\n"

dummySpec      :== StartTerminal
dummyConnect   :== {cWireID=(-1), cWireType=Free Unit}
dummyWire      :== {wireID=(-1), wireLine=((-1,-1),(-1,-1)), wireType=Free Unit}

///////// SaveCircuit
SaveCircuit :: !Circuit ![Wire] !ComponentID !WireID !Int !*File -> *File
SaveCircuit circuit wires compID nextWireID nextVar file
  = seq [fwrites startMark, WriteCircuit circuit, fwrites (endMark +++ startMark), WriteWires wires,
         fwrites (endMark +++ (toString compID) +++ "\n" +++ (toString nextWireID) +++ "\n" +++ (toString nextVar) +++ "\n")
        ] file

///////// WriteCircuit
WriteCircuit :: !Circuit !*File -> *File
WriteCircuit [comp : circuit] file = seq [WriteComponent comp, WriteCircuit circuit] file
WriteCircuit [] file = file

///////// WriteComponent
WriteComponent :: !Component !*File -> *File
WriteComponent {spec, id, inputs, outputs, pos} file
  = seq [WriteSpec spec, fwrites ((toString id) +++ "\n"), WritePos pos, fwrites startMark, WriteConnects inputs,
         fwrites (endMark +++ startMark), WriteConnects outputs, fwrites endMark
        ]
        file

///////// WriteSpec
//*^*WriteSpec :: !CompSpecifics !*File -> *File
WriteSpec :: !(CompSpecifics Component) !*File -> *File
WriteSpec StartTerminal file = fwrites "StartTerminal\n" file
WriteSpec EndTerminal   file = fwrites "EndTerminal\n" file
WriteSpec TensorI       file = fwrites "TensorI\n" file
WriteSpec TensorE       file = fwrites "TensorE\n" file
WriteSpec SumI          file = fwrites "SumI\n" file
WriteSpec SumE          file = fwrites "SumE\n" file
WriteSpec Lolly         file = fwrites "Lolly\n" file
WriteSpec UnitI         file = fwrites "UnitI\n" file
WriteSpec UnitEL        file = fwrites "UnitEL\n" file
WriteSpec UnitER        file = fwrites "UnitER\n" file
WriteSpec CounitE       file = fwrites "CounitE\n" file
WriteSpec CounitIL      file = fwrites "CounitIL\n" file
WriteSpec CounitIR      file = fwrites "CounitIR\n" file
WriteSpec (Box boxCirc) file = seq [fwrites ("Box\n" +++ startMark), WriteCircuit boxCirc, fwrites endMark] file
WriteSpec (Generic name inputTypes outputTypes) file
  = seq [fwrites ("Generic\n" +++ name +++ "\n" +++ startMark), WriteTypes inputTypes, fwrites (endMark +++ startMark),
         WriteTypes outputTypes, fwrites endMark
        ] file

///////// WriteTypes
WriteTypes :: ![Type] !*File -> *File
WriteTypes [type : types] file = seq [WriteType type, WriteTypes types] file
WriteTypes [] file = file

///////// WritePos
WritePos :: !Placement !*File -> *File
WritePos (PT point) file
  = seq [fwrites "PT\n", WritePoint point] file
WritePos (RCT (start, end)) file
  = seq [fwrites "RCT\n", WritePoint start, WritePoint end] file

///////// WriteConnects
WriteConnects :: ![Connection] !*File -> *File
WriteConnects [{cWireID, cWireType} : connects] file
  = seq [fwrites ((toString cWireID) +++ "\n"), WriteWireType cWireType, WriteConnects connects] file
WriteConnects [] file = file

///////// WriteWireType
WriteWireType :: !WireType !*File -> *File
WriteWireType (Free type) file
  = seq [fwrites "Free\n", WriteType type] file
WriteWireType (User type) file
  = seq [fwrites "User\n", WriteType type] file

///////// WriteType
WriteType :: !Type !*File -> *File
WriteType Unit         file = fwrites "Unit\n" file
WriteType Counit       file = fwrites "Counit\n" file
WriteType (Var x)      file = fwrites ("Var\n" +++ (toString x) +++ "\n") file
WriteType (UserVar1 s) file = fwrites ("UserVar1\n" +++ s +++ "\n") file
WriteType (UserVar2 x) file = fwrites ("UserVar2\n" +++ (toString x) +++ "\n") file
WriteType (Const s)    file = fwrites ("Const\n" +++ s +++ "\n") file
WriteType (Product (type1, type2)) file
  = seq [fwrites "Product\n", WriteType type1, WriteType type2] file
WriteType (Sum (type1, type2)) file
  = seq [fwrites "Sum\n", WriteType type1, WriteType type2] file
WriteType (Then (type1, type2)) file
  = seq [fwrites "Then\n", WriteType type1, WriteType type2] file
WriteType (UserFunc name types) file
  = seq [fwrites ("UserFunc\n" +++ name +++ "\n" +++ startMark), WriteTypes types, fwrites endMark] file

///////// WriteWires
WriteWires :: ![Wire] !*File -> *File
WriteWires [{wireID, wireLine=(start,end), wireType} : wires] file
  = seq [fwrites ((toString wireID) +++ "\n"), WritePoint start, WritePoint end, WriteWireType wireType, WriteWires wires] file
WriteWires [] file = file

///////// WritePoint
WritePoint :: !Point !*File -> *File
WritePoint (x,y) file
  = fwrites ((toString x) +++ "\n" +++ (toString y) +++ "\n") file

////////////////////////////////////////

///////// ReadCircuitFile
ReadCircuitFile :: !*File -> (Bool, Circuit, [Wire], ComponentID, WireID, Int, *File)
ReadCircuitFile file1
  | not success1
      = (False, [], [], 0, 0, 0, file2)
  | not success2
      = (False, [], [], 0, 0, 0, file3)
  | not success3
      = (False, [], [], 0, 0, 0, file4)
  | not success4
      = (False, [], [], 0, 0, 0, file5)
  | not success5
      = (False, [], [], 0, 0, 0, file6)
  | not success6
      = (False, [], [], 0, 0, 0, file7)
  | not success7
      = (False, [], [], 0, 0, 0, file8)
  | otherwise
      = (True, newCircuit, newWires, newCompID, newNextWireID, newNextVar, file8)
where
  (success1, file2)                = ReadStartMark file1
  (success2, newCircuit, file3)    = ReadCircuit file2
  (success3, file4)                = ReadStartMark file3
  (success4, newWires, file5)      = ReadWires file4
  (success5, newCompID, file6)     = ReadInt file5
  (success6, newNextWireID, file7) = ReadInt file6
  (success7, newNextVar, file8)    = ReadInt file7

///////// ReadStartMark
ReadStartMark :: !*File -> (Bool, *File)
ReadStartMark file1
  = (mark==startMark, file2)
where
  (mark, file2) = freadline file1

///////// ReadInt
ReadInt :: !*File -> (Bool, Int, *File)
ReadInt file1
  | not success
      = (False, n, file2)
  | otherwise
      = (remainder=="\n", n, file3)
where
  (success, n, file2) = freadi file1
  (remainder, file3)  = freadline file2

///////// ReadCircuit
ReadCircuit :: !*File -> (Bool, Circuit, *File)
ReadCircuit file1
  | not success1
      = (False, [], file2)
  | done
      = (True, [], file2)
  | otherwise
      = (success2, [comp : circuit], file3)
where
  (success1, done, comp, file2) = ReadComponent file1
  (success2, circuit, file3)    = ReadCircuit file2

///////// ReadComponent // First Bool is True if no problem, second is True if endMark was read
ReadComponent :: !*File -> (Bool, Bool, Component, *File)
ReadComponent file1
  | done         = (True, True, dummyComponent, file2)
  | not success1 = (False, False, dummyComponent, file2)
  | not success2 = (False, False, dummyComponent, file3)
  | not success3 = (False, False, dummyComponent, file4)
  | not success4 = (False, False, dummyComponent, file5)
  | not success5 = (False, False, dummyComponent, file6)
  | not success6 = (False, False, dummyComponent, file7)
  | not success7 = (False, False, dummyComponent, file8)
  | otherwise    = (True, False, {spec=newSpec, id=newID, pos=newPos, inputs=newInputs, outputs=newOutputs}, file8)
where
  (success1, done, newSpec, file2) = ReadSpec file1
  (success2, newID, file3)         = ReadInt file2
  (success3, newPos, file4)        = ReadPos file3
  (success4, file5)                = ReadStartMark file4
  (success5, newInputs, file6)     = ReadConnects file5
  (success6, file7)                = ReadStartMark file6
  (success7, newOutputs, file8)    = ReadConnects file7

///////// ReadSpec // First Bool is True if no problem, second is True if endMark was read
//*^*ReadSpec :: !*File -> (Bool, Bool, CompSpecifics, *File)
ReadSpec :: !*File -> (Bool, Bool, (CompSpecifics Component), *File)
ReadSpec file1
  | line==endMark           = (True, True, dummySpec, file2)
  | line=="StartTerminal\n" = (True, False, StartTerminal, file2)
  | line=="EndTerminal\n"   = (True, False, EndTerminal, file2)
  | line=="TensorI\n"       = (True, False, TensorI, file2)
  | line=="TensorE\n"       = (True, False, TensorE, file2)
  | line=="SumI\n"          = (True, False, SumI, file2)
  | line=="SumE\n"          = (True, False, SumE, file2)
  | line=="Lolly\n"         = (True, False, Lolly, file2)
  | line=="UnitI\n"         = (True, False, UnitI, file2)
  | line=="UnitEL\n"        = (True, False, UnitEL, file2)
  | line=="UnitER\n"        = (True, False, UnitER, file2)
  | line=="CounitE\n"       = (True, False, CounitE, file2)
  | line=="CounitIL\n"      = (True, False, CounitIL, file2)
  | line=="CounitIR\n"      = (True, False, CounitIR, file2)
  | line=="Box\n"           = if success1a
                                 (success2a, False, Box boxCirc, file4a)
                                 (False, False, dummySpec, file3a)
  | line=="Generic\n"       = if (nameSize < 2)
                                 (False, False, dummySpec, file3b)
                                 (if success1b
                                     (if success2b
                                         (if success3b
                                             (if success4b
                                                 (True, False, Generic name2 inputTypes outputTypes, file7b)
                                                 (False, False, dummySpec, file7b)
                                             )
                                             (False, False, dummySpec, file6b)
                                         )
                                         (False, False, dummySpec, file5b)
                                     )
                                     (False, False, dummySpec, file4b)
                                 )
  | otherwise               = (False, False, dummySpec, file2)
where
  (line, file2) = freadline file1

  (success1a, file3a)          = ReadStartMark file2
  (success2a, boxCirc, file4a) = ReadCircuit file3a

  (name1, file3b)                  = freadline file2
  nameSize                         = size name1
  name2                            = name1 % (0, nameSize - 2)
  (success1b, file4b)              = ReadStartMark file3b
  (success2b, inputTypes, file5b)  = ReadTypes file4b
  (success3b, file6b)              = ReadStartMark file5b
  (success4b, outputTypes, file7b) = ReadTypes file6b

///////// ReadTypes
ReadTypes :: !*File -> (Bool, [Type], *File)
ReadTypes file1
  | not success1 = (False, [], file2)
  | done         = (True, [], file2)
  | not success2 = (False, [], file3)
  | otherwise    = (True, [type : types], file3)
where
  (success1, done, type, file2) = ReadType file1
  (success2, types, file3)      = ReadTypes file2

///////// ReadPos
ReadPos :: !*File -> (Bool, Placement, *File)
ReadPos file1
  | line=="PT\n"  = (pointRead, PT point, file3a)
  | line=="RCT\n" = if startRead
                       (endRead, RCT (start, end), file4b)
                       (False, RCT (start, start), file3b)
  | otherwise     = (False, PT (-1,-1), file2)
where
  (line, file2)              = freadline file1
  (pointRead, point, file3a) = ReadPoint file2
  (startRead, start, file3b) = ReadPoint file2
  (endRead, end, file4b)     = ReadPoint file3b

///////// ReadPoint
ReadPoint :: !*File -> (Bool, Point, *File)
ReadPoint file1
  | xRead     = (yRead, (x,y), file3)
  | otherwise = (False, (-1,-1), file2)
where
  (xRead, x, file2) = ReadInt file1
  (yRead, y, file3) = ReadInt file2

///////// ReadConnects
ReadConnects :: !*File -> (Bool, [Connection], *File)
ReadConnects file1
  | done      = (True, [], file2)
  | success1  = (success2, [connect : connects], file3)
  | otherwise = (False, [], file2)
where
  (success1, done, connect, file2) = ReadConnect file1
  (success2, connects, file3)      = ReadConnects file2

///////// ReadConnect // First Bool is True if no problem, second is True if endMark was read
ReadConnect :: !*File -> (Bool, Bool, Connection, *File)
ReadConnect file1
  | not success1
      = if (line==endMark)
           (True, True, dummyConnect, file3a)
           (False, False, dummyConnect, file3a)
  | otherwise
      = (success2, False, {cWireID=newCWireID, cWireType=newCWireType}, file3b)
where
  (success1, newCWireID, file2)    = ReadInt file1
  (line, file3a)                   = freadline file2
  (success2, newCWireType, file3b) = ReadWireType file2

///////// ReadWireType
ReadWireType :: !*File -> (Bool, WireType, *File)
ReadWireType file1
  | line=="Free\n" = (success && (not done), Free type, file3)
  | line=="User\n" = (success && (not done), User type, file3)
  | otherwise      = (False, Free Unit, file2)
where
  (line, file2)                = freadline file1
  (success, done, type, file3) = ReadType file2

///////// ReadType // First Bool is True if no problem, second is True if endMark was read
ReadType :: !*File -> (Bool, Bool, Type, *File)
ReadType file1
  | line==endMark      = (True, True, Unit, file2)
  | line=="Unit\n"     = (True, False, Unit, file2)
  | line=="Counit\n"   = (True, False, Counit, file2)
  | line=="Var\n"      = (varRead, False, Var var, file3a)
  | line=="UserVar2\n" = (varRead, False, UserVar2 var, file3a)
  | line=="Product\n"  = if (type1Read && (not done1))
                            (type2Read && (not done2), False, Product (type1, type2), file4b)
                            (False, False, Unit, file3b)
  | line=="Sum\n"      = if (type1Read && (not done1))
                            (type2Read && (not done2), False, Sum (type1, type2), file4b)
                            (False, False, Unit, file3b)
  | line=="Then\n"     = if (type1Read && (not done1))
                            (type2Read && (not done2), False, Then (type1, type2), file4b)
                            (False, False, Unit, file3b)
  | line=="UserVar1\n"  = if (nameSize < 2)
                            (False, False, Unit, file3c)
                            (True, False, UserVar1 name2, file3c)
  | line=="Const\n"    = if (nameSize < 2)
                            (False, False, Unit, file3c)
                            (True, False, Const name2, file3c)
  | line=="UserFunc\n" = if (nameSize < 2)
                            (False, False, Unit, file3c)
                            (if (not startMarkRead)
                                (False, False, Unit, file4c)
                                (typesRead, False, UserFunc name2 types, file5c)
                            )
  | otherwise          = (False, False, Unit, file2)
where
  (line, file2) = freadline file1

  (varRead, var, file3a) = ReadInt file2

  (type1Read, done1, type1, file3b) = ReadType file2
  (type2Read, done2, type2, file4b) = ReadType file3b

  (name1, file3c) = freadline file2
  nameSize        = size name1
  name2           = name1 % (0, nameSize - 2)

  (startMarkRead, file4c)    = ReadStartMark file3c
  (typesRead, types, file5c) = ReadTypes file4c

///////// ReadWires
ReadWires :: !*File -> (Bool, [Wire], *File)
ReadWires file1
  | done      = (True, [], file2)
  | success1  = (success2, [wire : wires], file3)
  | otherwise = (False, [], file2)
where
  (success1, done, wire, file2) = ReadWire file1
  (success2, wires, file3)      = ReadWires file2

///////// ReadWire // First Bool is True if no problem, second is True if endMark was read
ReadWire :: !*File -> (Bool, Bool, Wire, *File)
ReadWire file1
  | not success1 = if (line==endMark)
                      (True, True, dummyWire, file3a)
                      (False, False, dummyWire, file3a)
  | not success2 = (False, False, dummyWire, file3b)
  | not success3 = (False, False, dummyWire, file4b)
  | not success4 = (False, False, dummyWire, file5b)
  | otherwise    = (True, False, {wireID=newWireID, wireLine=(start,end), wireType=newWireType}, file5b)
where
  (success1, newWireID, file2) = ReadInt file1
  (line, file3a)               = freadline file2
  (success2, start, file3b)    = ReadPoint file2
  (success3, end, file4b)      = ReadPoint file3b
  (success4, newWireType, file5b) = ReadWireType file4b
