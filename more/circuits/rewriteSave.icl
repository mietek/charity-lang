// This module contains code for saving rewrites to files and reading them from files.

implementation module rewriteSave

import StdEnv
import rewriteDefs, circuitSave

////////// SaveRewrite
// Takes a rewrite and a file (which should have its file pointer positioned at the end of the
// file) and returns the new file after writing the rewrite at the file-pointer position.
SaveRewrite :: Rewrite *File -> *File
SaveRewrite {rewriteName, liveWireID, leftComp, leftTop, leftMid, leftBot, rightSide, ruleRect=(topLeft, bottomRight),
             inConnects, outConnects, connectedPairs, brokenWireConnect, oldRightSide} file
  = seq [fwrites (rewriteName +++ "\n"),
	 fwrites ((toString liveWireID) +++ "\n"),
         WriteComponent leftComp,
         fwrites startMark,
         WriteCircuit leftTop,
         fwrites (endMark +++ startMark),
         WriteCircuit leftMid,
	 fwrites (endMark +++ startMark),
         WriteCircuit leftBot, 
	 fwrites (endMark +++ startMark), 
	 WriteCircuit rightSide, 
	 fwrites endMark,
         WritePoint topLeft, 
	 WritePoint bottomRight, 
	 fwrites startMark, 
	 WriteConnects inConnects,
         fwrites (endMark +++ startMark), 
	 WriteConnects outConnects, 
	 fwrites (endMark +++ startMark),
         WritePairs connectedPairs, 
         fwrites endMark, 
         WriteConnects [brokenWireConnect],
         fwrites startMark,
         WriteCircuit oldRightSide,
         fwrites endMark
        ] file

////////// WritePairs
WritePairs :: [(WireID, WireID, WireType)] *File -> *File
WritePairs [(wireID1, wireID2, wireType) : pairs] file
 = seq [fwrites ((toString wireID1) +++ "\n" +++ (toString wireID2) +++ "\n"), WriteWireType wireType, WritePairs pairs] file
WritePairs [] file = file

////////// ReadRewrite
// Takes a file (which should have its file pointer positioned at the beginning of a rewrite),
// and returns:
//   - True if it has reached the end of the file, False otherwise,
//   - True if it read a rewrite from the file, False otherwise (always False if the first argument is True),
//   - the rewrite that it read starting at the file-pointer position, if any,
//   - the new file.
ReadRewrite :: *File -> (Bool, Bool, Rewrite, *File)
ReadRewrite file
  | not success1              = if endOfFile
                                   (True, False, dummyRewrite, file1b)
                                   (False, False, dummyRewrite, file1b)
  | not success2              = (False, False, dummyRewrite, file2)
  | (not success3) || done3   = (False, False, dummyRewrite, file3)
  | not success4              = (False, False, dummyRewrite, file4)
  | not success5              = (False, False, dummyRewrite, file5)
  | not success6              = (False, False, dummyRewrite, file6)
  | not success7              = (False, False, dummyRewrite, file7)
  | not success8              = (False, False, dummyRewrite, file8)
  | not success9              = (False, False, dummyRewrite, file9)
  | not success10             = (False, False, dummyRewrite, file10)
  | not success11             = (False, False, dummyRewrite, file11)
  | not success12             = (False, False, dummyRewrite, file12)
  | not success13             = (False, False, dummyRewrite, file13)
  | not success14             = (False, False, dummyRewrite, file14)
  | not success15             = (False, False, dummyRewrite, file15)
  | not success16             = (False, False, dummyRewrite, file16)
  | not success17             = (False, False, dummyRewrite, file17)
  | not success18             = (False, False, dummyRewrite, file18)
  | not success19             = (False, False, dummyRewrite, file19)
  | (not success20) || done20 = (False, False, dummyRewrite, file20)
  | not success21             = (False, False, dummyRewrite, file21)
  | not success22             = (False, False, dummyRewrite, file22)
  | otherwise
      = (False, True,
         {rewriteName=rewriteName, liveWireID=liveWireID, leftComp=leftComp, leftTop=leftTop, leftMid=leftMid,
          leftBot=leftBot, rightSide=rightSide, ruleRect=(topLeft, bottomRight), inConnects=inConnects,
          outConnects=outConnects, connectedPairs=connectedPairs, brokenWireConnect=brokenWireConnect,
          oldRightSide=oldRightSide
         },
         file22
        )
where
  (success1, rewriteName, file1)                 = ReadWord file
  (endOfFile, file1b)                            = fend file1
  (success2, liveWireID, file2)                  = ReadInt file1
  (success3, done3, leftComp, file3)             = ReadComponent file2
  (success4, file4)                              = ReadStartMark file3
  (success5, leftTop, file5)                     = ReadCircuit file4
  (success6, file6)                              = ReadStartMark file5
  (success7, leftMid, file7)                     = ReadCircuit file6
  (success8, file8)                              = ReadStartMark file7
  (success9, leftBot, file9)                     = ReadCircuit file8
  (success10, file10)                            = ReadStartMark file9
  (success11, rightSide, file11)                 = ReadCircuit file10
  (success12, topLeft, file12)                   = ReadPoint file11
  (success13, bottomRight, file13)               = ReadPoint file12
  (success14, file14)                            = ReadStartMark file13
  (success15, inConnects, file15)                = ReadConnects file14
  (success16, file16)                            = ReadStartMark file15
  (success17, outConnects, file17)               = ReadConnects file16
  (success18, file18)                            = ReadStartMark file17
  (success19, connectedPairs, file19)            = ReadPairs file18
  (success20, done20, brokenWireConnect, file20) = ReadConnect file19
  (success21, file21)                            = ReadStartMark file20
  (success22, oldRightSide, file22)              = ReadCircuit file21

////////// ReadPairs
ReadPairs :: *File -> (Bool, [(WireID, WireID, WireType)], *File)
ReadPairs file
  | success1
      = if success2
           (if success3
               (if success4
                   (True, [(val1, val2, wType) : pairs], file4A)
                   (False, [], file4A)
               )
               (False, [], file3A)
           )
           (False, [], file2A)
  | line==endMark
      = (True, [], file2B)
  | otherwise
      = (False, [], file2B)
where
  (success1, val1, file1)   = ReadInt file
  (success2, val2, file2A)  = ReadInt file1
  (success3, wType, file3A) = ReadWireType file2A
  (success4, pairs, file4A) = ReadPairs file3A
  (line, file2B)            = freadline file1

////////// ReadWord
ReadWord :: *File -> (Bool, String, *File)
ReadWord file1
  | wordSize < 2 = (False, "", file2)                       // Word should have at least one char + newline.
  | otherwise    = (True, word % (0, wordSize - 2), file2)  // Removes the newline at the end.
where
  (word, file2) = freadline file1
  wordSize      = size word
