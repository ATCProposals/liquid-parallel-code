{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module ArrayLike where
import Prelude hiding (splitAt)

data Ur a = Ur a

dup :: a -> a
dup x = x

class ArrayLike c where
  length :: c a -> Int
  length2 :: c a -> (Ur Int, c a)
  alloc :: Int -> c a
  (!) :: c a -> Int -> a
  (<~) :: c a -> (Int, a) -> c a

{- Slices can be dead or alive. Dead slices prevent you from accessing memory
 - that is owned by a child slice. -}
data ArrayLabel = Dead | Alive
data Slice (q :: ArrayLabel) ca where
  Slice :: ArrayLike c => (Int, Int) -> c a -> Slice Alive (c a)

-- Constructor
slice :: ArrayLike c => c a -> Slice Alive (c a)
slice arr = 
  let (Ur len, arr) = length2 arr in
  Slice (0, len - 1) arr

-- Projectors
unwrap :: ArrayLike c => Slice q (c a) -> c a
unwrap (Slice _ arr) = arr

bounds2 :: Slice q ca -> (Ur (Int, Int), Slice q ca)
bounds2 (Slice bs arr) = (Ur bs, Slice bs arr)

-- Slices are arraylike
instance ArrayLike (Slice 'Alive) where
  length = length . unwrap

  length2 (Slice bs xs) =
    let (Ur len, xs) = length2 xs in
    (Ur len, Slice bs xs)

  alloc = slice . alloc

  (!) = (!) . unwrap

  Slice bs xs <~ (i, x) =
    let xs = xs <~ (i, x) in
    slice xs

-- Additional functions
splitAt ::
  Int -> 
  Slice 'Alive (c a) -> -- Parent array
  (Slice 'Alive (c a),  -- Left subarray
   Slice 'Alive (c a),  -- Right subarray
   Slice 'Dead (c a))   -- Parent array
splitAt = undefined

lifeSteal ::
  Slice 'Dead (c a) ->  -- Parent array
  Slice 'Alive (c a) -> -- Left subarray
  Slice 'Alive (c a) -> -- Right subarray
  Slice 'Alive (c a)    -- Parent array
lifeSteal = undefined

mergeInto :: Ord a =>
  Slice 'Alive (c a) ->     -- return buffer, length = length_l + length_r 
  Slice 'Alive (c a) ->     -- left subarray
  Int ->                 -- length_l
  Slice 'Alive (c a) ->     -- right subarray
  Int ->                 -- length_r
  Slice 'Alive (c a)        -- return buffer
mergeInto = undefined
