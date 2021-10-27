{-# LANGUAGE GADTs #-}

module OrderedArray where

class Ord a => VerifiedOrd a where
  {-@ type Refl a = x:a -> {x <= x} @-}
  {-@ type AntiSym a = 
        x:a -> y:a -> _ltxy:{x <= y} -> _ltyx:{y <= x} -> {x == y} @-}
  {-@ type Total a = x:a -> y:a -> {x <= y || y <= x} @-}
  {-@ type Trans a = 
        x:a -> y:a -> _ltxy:{x <= y} -> z:a -> _ltyz:{y <= z} -> {x <= z} @-}

  {-@ refl :: Refl a @-}
  refl :: a -> ()
  {-@ antiSym :: AntiSym a @-}
  antiSym :: a -> a -> () -> () -> ()
  {-@ total :: Total a @-}
  total :: a -> a -> ()
  {-@ trans :: Trans a @-}
  trans :: a -> a -> () -> a -> () -> ()

mer
