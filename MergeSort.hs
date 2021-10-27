{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}

module MergeSort where

mergeSort :: (a -> a -> Int) -> Vector a -> Vector a
