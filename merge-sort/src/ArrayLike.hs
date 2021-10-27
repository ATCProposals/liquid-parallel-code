{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module ArrayLike where
import Prelude hiding (splitAt)

data Ur a = Ur a

dup :: a -> a
dup x = x

data PNat = 
  Zero | Suc PNat

data ArrayLabel = W | L ArrayLabel | R ArrayLabel

data Slice (q :: ArrayLabel) c a = Slice {
  _bounds :: (Int, Int),
  unwrap :: c a
}

bounds :: Slice q c a -> (Ur Int, Ur Int, Slice q c a)
bounds xs = 
  let (i, j) = _bounds $ dup xs in
  (Ur i, Ur j, xs)

class ArrayLike c where
  length :: c a -> Int
  length2 :: c a -> (Ur Int, c a)
  alloc :: Int -> c a
  (!) :: c a -> Int -> a
  (<~) :: c a -> (Int, a) -> c a
  
class ArrayLike c => Sliceable c where
  toSlice :: c a -> Slice W c a
  toSlice arr = 
    let (Ur len, arr) = length2 arr in
    Slice (0, len - 1) arr

  fromSlice :: Slice W c a -> c a
  fromSlice (Slice _ arr) = arr

  splitAt :: Int -> Slice p c a -> (Slice ('L p) c a, Slice ('R p) c a)
  merge :: Slice ('L p) c a -> Slice ('R p) c a -> Slice p c a

instance Sliceable c => ArrayLike (Slice p c) where
  length (Slice _ xs) = ArrayLike.length xs
  length2 (Slice bs xs) = 
    let (Ur len, xs) = ArrayLike.length2 xs in
    (Ur len, Slice bs xs)

  alloc len = undefined

  (!) (Slice _ xs) = (!) xs
  (Slice bs xs) <~ (i, e) = Slice bs $ xs <~ (i, e)

