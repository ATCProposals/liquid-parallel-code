{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{- Safe slices for linear types -}
module Slice where

{- Slices are typelevel constructs used to check the array for safe aliasing
 - and bounds. Slices are parameterized by a type to identify an underlying
 - Array and the bounds.

{-@
predicate IsRangeOf L Bs = snd Bs - fst Bs == L
data Slice ca where
  Slice :: ArrayLike c => 
    bs:(Int, Int) -> 
    {xs:(c a) | length xs = snd bs - fst bs + 1} ->
    Slice q (c a)
@-}
data Slice ca where
  Slice :: ArrayLike c => (Int, Int) -> c a -> Slice (c a)

-- Constructor
slice :: ArrayLike c => c a -> Slice (c a)
slice arr = 
  let (Ur len, arr) = length2 arr in
  Slice (0, len - 1) arr

-- Projectors
unwrap :: Slice ca -> ca
unwrap (Slice _ arr) = arr

{-@ reflect bounds @-}
bounds :: Slice ca -> (Int, Int)
bounds (Slice bs _) = bs

{-@ reflect leftBound @-}
leftBound :: Slice ca -> Int
leftBound = fst . bounds

{-@ reflect rightBound @-}
rightBound :: Slice ca -> Int
rightBound = snd . bounds

bounds2 :: Slice ca -> (Ur (Int, Int), Slice q ca)
bounds2 (Slice bs arr) = (Ur bs, Slice bs arr)

length2 :: [a] -> (Ur Int, [a])

-- Slices are arraylike
instance ArrayLike Slice where
  length = length . unwrap

  length2 (Slice bs xs) =
    let (Ur len, xs) = length2 xs in
    (Ur len, Slice bs xs)

  alloc = slice . alloc

  (!) = (!) . unwrap

  Slice bs xs <~ (i, x) = Slice bs $ xs <~ (i, x)

-- Additional functions
{-@
splitAt ::
  Int ->
  xs:(Slice (c a)) ->
  (l:{l | leftBound l == leftBound xs},
   {r | leftBound r == rightBound l + 1 && rightBound r == rightBound xs},
   {ys:(Slice (c a)) | xs === ys}) 
@-}
splitAt ::
  Int -> 
  Slice (c a) -> -- Parent array
  (Slice (c a),  -- Left subarray
   Slice (c a),  -- Right subarray
   Slice (c a))   -- Parent array
splitAt i (Slice (j, k) xs) = 
  let (l, r, xs) = unsafeSplitAt i xs in
  (Slice (j, i - 1) l, Slice (i, k) r, Slice (j, k) xs)
-- TODO: Define === for slices

{-@
mergeInto :: Ord a =>
  l:Slice (c a) ->
  len_l:{i | i == length l} ->
  r:{r | leftBound r == rightBound l + 1} ->
  len_r:{i | i == length r} ->
  {ret | length ret == len_l + len_r} ->
  {ret | length ret == len_l + len_r && ordered (0, length ret) ret}
@-}
mergeInto :: Ord a =>
  Slice (c a) ->     -- left subarray
  Int ->                    -- length_l
  Slice (c a) ->     -- right subarray
  Int ->                    -- length_r
  Slice (c a) ->     -- return buffer, length = length_l + length_r 
  Slice (c a)        -- return buffer
mergeInto (Slice bs xs) (Slice _ l) len_l (Slice _ r) len_r =
  Slice bs $ unsafeMergeInto xs l len_l r len_r
-- TODO: Define ordered


{-
{-@
lifeSteal ::
  p:(Slice 'Dead (c a)) ->
  l:{l | leftBound l == leftBound p} ->
  {r | rightBound r == rightBound p && leftBound r == rightBound l + 1} ->
  {p' | p' === p}
@-}
lifeSteal ::
  Slice 'Dead (c a) ->  -- Parent array
  Slice 'Alive (c a) -> -- Left subarray
  Slice 'Alive (c a) -> -- Right subarray
  Slice 'Alive (c a)    -- Parent array
lifeSteal xs _ _ = xs
-}
