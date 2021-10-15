{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MergeSort where
import Prelude.Linear (Ur(Ur), (&), (+))
import Data.Array.Mutable.Linear (Array, fromList)

data Path = Nil | L Path | R Path
data

mergeSort :: Ord a => Array a %1-> Array a
mergeSort xs = 
  
