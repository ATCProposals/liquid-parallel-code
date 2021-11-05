{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module ArrayLike where
import Prelude hiding (splitAt, length)

data Ur a = Ur a

dup :: a -> a
dup x = x

class ArrayLike c where
  length :: c a -> Int
  alloc :: Int -> a -> c a
  get :: Int -> c a -> a
  (<~) :: c a -> (Int, a) -> c a

class Linearizable c where
  from :: (c a -> b) -> c a -> (b, c a)

class (ArrayLike c, Linearizable c) => LinearArrayLike c

length2 :: LinearArrayLike c => c a -> (Ur Int, c a)
length2 = from (Ur . length)

(!) :: LinearArrayLike c => c a -> Int -> (Ur a, c a)
src ! i = (Ur . get i) `from` src

copyInto :: LinearArrayLike c => c a -> Int -> c a -> Int -> Int -> (c a, c a)
copyInto ret _ src _ 0 = (ret, src)
copyInto ret i src j len =
  let (Ur v, src) = src ! j in
  copyInto (ret <~ (i, v)) (i + 1) src (j + 1) (len - 1)

swapIndex :: LinearArrayLike c => c a -> Int -> Int -> c a
swapIndex src i j = 
  (src <~ (i, get j src)) <~ (j, get i src)
