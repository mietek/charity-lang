// Contains the code used to parse a type from a string (used when the user specifies
// the type for a wire).

implementation module parseType

import StdEnv
import typeDefs

/////////// ParseType
ParseType :: !String -> (Bool, Type)
ParseType typeString
  | parsed && (isEmpty rest) = (True, type)
  | otherwise                = (False, Unit)
where
  (parsed, type, rest) = ParseFromPieces (RemoveEmpties (SplitOutPieces (fromString typeString)))

/////////// SplitOutPieces
SplitOutPieces :: ![Char] -> [[Char]]
SplitOutPieces [' ' : chars]                          = [[], [' '] : SplitOutPieces chars]
SplitOutPieces ['"' : chars]                          = [[], ['"'] : SplitOutPieces chars]
SplitOutPieces ['(', '*', ')' : chars]                = [[], ['(', '*', ')'] : SplitOutPieces chars]
SplitOutPieces ['(', '+', ')' : chars]                = [[], ['(', '+', ')'] : SplitOutPieces chars]
SplitOutPieces ['(', '-', 'o', ')' : chars]           = [[], ['(', '-', 'o', ')'] : SplitOutPieces chars]
SplitOutPieces ['(' : chars]                          = [[], ['('] : SplitOutPieces chars]
SplitOutPieces [')' : chars]                          = [[], [')'] : SplitOutPieces chars]
SplitOutPieces [',' : chars]                          = [[], [','] : SplitOutPieces chars]
SplitOutPieces ['U', 'n', 'i', 't' : chars]           = [[], ['U', 'n', 'i', 't'] : SplitOutPieces chars]
SplitOutPieces ['C', 'o', 'u', 'n', 'i', 't' : chars] = [[], ['C', 'o', 'u', 'n', 'i', 't'] : SplitOutPieces chars]
SplitOutPieces [c : chars]                            = [[c : (hd pieces)] : tl pieces]
where
  pieces = SplitOutPieces chars
SplitOutPieces []                                     = [[]]

/////////// RemoveEmpties
RemoveEmpties :: ![[Char]] -> [[Char]]
RemoveEmpties [[] : pieces]    = RemoveEmpties pieces
RemoveEmpties [[' '] : pieces] = RemoveEmpties pieces
RemoveEmpties [piece : pieces] = [piece : RemoveEmpties pieces]
RemoveEmpties []               = []

/////////// ParseFromPieces
ParseFromPieces :: ![[Char]] -> (Bool, Type, [[Char]])

ParseFromPieces [['U', 'n', 'i', 't'] : pieces] = ParseTypeEnd Unit pieces

ParseFromPieces [['C', 'o', 'u', 'n', 'i', 't'] : pieces] = ParseTypeEnd Counit pieces

ParseFromPieces [['"'], name, ['"'] : pieces]
  | all GoodChar name = ParseTypeEnd (Const (toString name)) pieces
  | otherwise         = (False, Unit, [])

ParseFromPieces [name, ['('] : pieces]
  | (not (all GoodChar name)) || (not parsed) = (False, Unit, [])
  | otherwise                                 = ParseTypeEnd (UserFunc (toString name) types) rest
where
  (parsed, types, rest) = ParseTypeList pieces

ParseFromPieces [['('] : pieces]
  | parsed    = if (isEmpty rest)
                   (False, Unit, [])
                   (if ((hd rest)==[')'])
                       (ParseTypeEnd type (tl rest))
                       (False, Unit, [])
                   )
  | otherwise = (False, Unit, [])
where
  (parsed, type, rest) = ParseFromPieces pieces

ParseFromPieces [name : pieces]
  | all GoodChar name = ParseTypeEnd (UserVar1 (toString name)) pieces
  | otherwise         = (False, Unit, [])

ParseFromPieces [] = (False, Unit, [])

/////////// ParseTypeEnd
// Parses the part of a type that follows a type.
ParseTypeEnd :: Type ![[Char]] -> (Bool, Type, [[Char]])
ParseTypeEnd type1 [['(', '*', ')'] : pieces]
  | parsed    = (True, Product (type1, type2), rest)
  | otherwise = (False, Unit, [])
where
  (parsed, type2, rest) = ParseFromPieces pieces

ParseTypeEnd type1 [['(', '+', ')'] : pieces]
  | parsed    = (True, Sum (type1, type2), rest)
  | otherwise = (False, Unit, [])
where
  (parsed, type2, rest) = ParseFromPieces pieces

ParseTypeEnd type1 [['(', '-', 'o', ')'] : pieces]
  | parsed    = (True, Then (type1, type2), rest)
  | otherwise = (False, Unit, [])
where
  (parsed, type2, rest) = ParseFromPieces pieces

ParseTypeEnd type1 pieces = (True, type1, pieces)

/////////// ParseTypeList
ParseTypeList :: [[Char]] -> (Bool, [Type], [[Char]])
ParseTypeList pieces
  | parsed1   = if ((hd rest1)==[','])
                   (parsed2, [type : types], rest2)
                   (if ((hd rest1)==[')'])
                       (True, [type], tl rest1)
                       (False, [], [])
                   )
  | otherwise = (False, [], [])
where
  (parsed1, type, rest1)  = ParseFromPieces pieces
  (parsed2, types, rest2) = ParseTypeList (tl rest1)

/////////// GoodChar
GoodChar :: !Char -> Bool
GoodChar c = (isMember c ['a'..'z']) || (isMember c ['A'..'Z']) || (c=='_')
