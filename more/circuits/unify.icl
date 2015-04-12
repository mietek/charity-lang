// Contains code used to unify or match two types.

implementation module unify

import StdEnv, typeDefs

:: Substitution :== (Int, Type)

////////// Unify
Unify :: !Type !Type -> (Bool, [Substitution])
Unify (Var a) (Var b)
  | a==b      = (True, [])
  | otherwise = (True, [(a, Var b)])
Unify (Var a) type2
  | OccursIn a type2 = (False, [])
  | otherwise        = (True, [(a, type2)])
Unify type1 (Var b)
  | OccursIn b type1 = (False, [])
  | otherwise        = (True, [(b, type1)])
Unify type1 type2
  | SameConstructor type1 type2 = UnifyArgs (Args type1) (Args type2)
  | otherwise                   = (False, [])

////////// UnifyArgs
UnifyArgs :: ![Type] ![Type] -> (Bool, [Substitution])
UnifyArgs [type1 : types1] [type2 : types2]
  | success1 && success2 = (True, SubsIntoSubs subs2 subs1 ++ subs2)
  | otherwise            = (False, [])
where
  (success1, subs1) = Unify type1 type2
  (success2, subs2) = UnifyArgs (map (SubsIntoType subs1) types1) (map (SubsIntoType subs1) types2)
UnifyArgs [] [] = (True, [])
UnifyArgs _ _   = (False, [])

////////// SameConstructor
SameConstructor :: !Type Type -> Bool
SameConstructor Unit Unit = True
SameConstructor Counit Counit = True
SameConstructor (Const a) (Const b) = a==b
SameConstructor (UserVar1 a) (UserVar1 b) = a==b
SameConstructor (UserVar2 a) (UserVar2 b) = a==b
SameConstructor (Product _) (Product _) = True
SameConstructor (Sum _) (Sum _) = True
SameConstructor (Then _) (Then _) = True
SameConstructor (UserFunc name1 _) (UserFunc name2 _) = name1==name2
SameConstructor _ _ = False

////////// Args
Args :: !Type -> [Type]
Args Unit                     = []
Args Counit                   = []
Args (Const _)                = []
Args (UserVar1 _)             = []
Args (UserVar2 _)             = []
Args (Product (type1, type2)) = [type1, type2]
Args (Sum (type1, type2))     = [type1, type2]
Args (Then (type1, type2))    = [type1, type2]
Args (UserFunc _ types)       = types

///////// SubsIntoSubs
SubsIntoSubs :: ![Substitution] [Substitution] -> [Substitution]
SubsIntoSubs subs1 subs2 = (foldl (flip (map o SubIntoSub)) subs2 subs1)

///////// SubsIntoType
SubsIntoType :: ![Substitution] Type -> Type
SubsIntoType subs type = foldl (flip SubIntoType) type subs

///////// SubIntoType
SubIntoType :: !Substitution !Type -> Type

SubIntoType (a, subType) (Var b)
  | a==b      = subType
  | otherwise = Var b

SubIntoType sub (Product (type1, type2))
  = Product ((SubIntoType sub type1), (SubIntoType sub type2))

SubIntoType sub (Sum (type1, type2))
  = Sum ((SubIntoType sub type1), (SubIntoType sub type2))

SubIntoType sub (Then (type1, type2))
  = Then ((SubIntoType sub type1), (SubIntoType sub type2))

SubIntoType sub (UserFunc name types)
  = UserFunc name (map (SubIntoType sub) types)

SubIntoType _ type = type

///////// SubIntoSub
SubIntoSub :: !Substitution !Substitution -> Substitution
SubIntoSub (a, subType) (b, Var c)
  | a==c      = (b, subType)
  | otherwise = (b, Var c)
SubIntoSub sub (b, type)
  = (b, SubIntoType sub type)

///////// OccursIn
OccursIn :: Int !Type -> Bool
OccursIn a (Var b) = a==b
OccursIn a type    = any (OccursIn a) (Args type)

//////////////////////////////////

//////// MatchTypes
// Matches two types.  Returns True if the first type is no more
// general than the second, with a list of variable substitutions that
// will make second type equal to the first.  The two types are assumed
// to have independent variables.  It won't allow a variable to be
// substituted by more than one type.
MatchTypes :: Type !Type [Substitution] -> (Bool, [Substitution])
MatchTypes type (Var a) subs = InsertSub (a, type) subs
MatchTypes type1 type2 subs
  | SameConstructor type1 type2 = MatchTypeArgs (Args type1) (Args type2) subs
  | otherwise                   = (False, [])

//////// MatchTypeArgs
MatchTypeArgs :: ![Type] ![Type] [Substitution] -> (Bool, [Substitution])
MatchTypeArgs [type1 : types1] [type2 : types2] subs1
  | success   = MatchTypeArgs types1 types2 subs2
  | otherwise = (False, [])
where
  (success, subs2) = MatchTypes type1 type2 subs1

MatchTypeArgs [] [] subs = (True, subs)

MatchTypeArgs _ _ _ = (False, [])

//////// InsertSub
InsertSub :: !Substitution ![Substitution] -> (Bool, [Substitution])
InsertSub sub subs = InsertSubAux sub subs []
where
  InsertSubAux (x, type1) [(y, type2) : subs] newSubs
    | x==y      = if (EqualTypes type1 type2)
                     (True, [(y, type2) : (subs ++ newSubs)])
                     (False, [])
    | otherwise = InsertSubAux (x, type1) subs [(y, type2) : newSubs]

  InsertSubAux sub [] newSubs = (True, [sub : newSubs])

