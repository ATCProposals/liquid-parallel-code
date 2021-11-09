module LinearArray where
import Prelude hiding (length)
import qualified Data.Array.IO as A
import Data.Array.MArray
import System.IO.Unsafe

{-@ newtype Array a <p :: Array Int -> Bool> = Array (A.IOArray Int a) @-}
newtype Array a = Array (A.IOArray Int a)

newtype Ur a = Ur a

{-@ type Nat = {x:Int | 0 <= x} @-}
{-@ type IndexOf Xs = {x:Nat | x < length Xs} @-}

{-# NOINLINE length #-}
{-@ reflect length @-}
{-@ length :: Array a -> Nat @-}
length :: Array a -> Int
length (Array xs) = unsafePerformIO $ do
  (i, j) <- getBounds xs
  return (j - i + 1)

{-# NOINLINE get #-}
{-@ reflect get @-}
{-@ get :: xs:(Array a) -> IndexOf Xs -> a @-}
get :: Array a -> Int -> a
get (Array xs) i = unsafePerformIO $ readArray xs i

{-# NOINLINE (<~) #-}
{-@ (<~) :: 
      xs:(Array <p> a) -> 
      (i:IndexOf xs, x:a) -> 
      Array <\arr -> get arr i == x && p arr> @-}
(<~) :: Array a -> (Int, a) -> Array a
Array xs <~ (i, x) = Array $ unsafePerformIO $ do
  writeArray xs i x
  return xs

{-# NOINLINE alloc #-}
{-@ alloc :: len:Int -> Array <{\arr -> length arr == len}> a @-}
alloc :: Int -> Array a
alloc len = Array $ unsafePerformIO $ newArray_ (0, len - 1)

{-@ reflect equalInRange @-}
{-@ equalInRange ::
      xs:(Array a) -> i:(IndexOf xs) ->
      ys:(Array a) -> j:(IndexOf ys) ->
      {k:Nat | i + k <= length xs && j + k <= length ys} ->
      Bool @-}
equalInRange :: Eq a => Array a -> Int -> Array a -> Int -> Int -> Bool
equalInRange xs i ys j 0 = True
equalInRange xs i ys j k = 
  xs `get` i == ys `get` j && 
  equalInRange xs (i + 1) ys (j + 1) (k - 1)

{-@ type Sized N = {xs:(Array a) | length xs = N} @-}

{-@ reflect equal @-}
{-@ equal :: 
      xs:(Array a) -> 
      ys:(Sized (length xs)) -> 
      {b:Bool | b <=> equal xs ys} @-}
equal :: Eq a => Array a -> Array a -> Bool
equal xs ys = equalInRange xs 0 ys 0 (length xs)

{-@ type EqualTo Xs = {xs:_ | equal xs Xs} @-}
{-@ type LengthOf Xs = {n:Int | n = length xs} @-}

{-@ length2 :: xs:(Array a) -> (Ur (LengthOf xs), EqualTo a xs) @-}
length2 :: Array a -> (Ur Int, Array a)
length2 xs = (Ur (length xs), xs)

{-@ type Get Xs I = {x:_ | get xs i = x} @-}

{-@ (!) :: xs:(Array a) -> i:(IndexOf xs) -> (Ur (Get xs i), EqualTo xs) @-}
(!) :: Array a -> Int -> (Ur a, Array a)
xs ! i = (Ur (xs `get` i), xs)
