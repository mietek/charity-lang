// This module contains the standard rewrites (which always appear on the menu) as well as some
// information used in displaying the box-reduction rewrite (which is not stored as a rewrite).

definition module stdRewrites

import rewriteDefs

tensorReduct :: Rewrite
sumReduct :: Rewrite
unitReductLeft :: Rewrite
unitReductRight :: Rewrite
counitReductLeft :: Rewrite
counitReductRight :: Rewrite
tensorExp :: Rewrite
sumExp :: Rewrite
unitExpLeft :: Rewrite
unitExpRight :: Rewrite
counitExpLeft :: Rewrite
counitExpRight :: Rewrite
boxExp :: Rewrite

boxReductLiveWireID :: Int
boxReductLeftSide :: Circuit
boxReductLeftWires :: [Wire]
boxReductLeftCompID :: Int
boxReductLeftNextWireID :: Int
boxReductLeftNextVar :: Int
boxReductRightSide :: Circuit
boxReductRightWires :: [Wire]
boxReductRightCompID :: Int
boxReductRightNextWireID :: Int
boxReductRightNextVar :: Int
