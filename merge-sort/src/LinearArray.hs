{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LinearArray where
import Prelude hiding (length)

import qualified Data.Array.IO as A
import Data.Array.MArray as M
import Data.Unrestricted.Internal.Ur
-- import Data.Unrestricted.Linear
import System.IO.Unsafe
-- import Unsafe.Linear

import qualified Set as S

{-@ type Nat = Int <(\n -> 0 <= n)> @-}

type RawArray a = A.IOArray Int a

{-@ newtype Array a id <dom :: Set Int, rng :: Set (Int <dom>, a)> = 
      Array (RawArray a) @-}
newtype Array a id = Array (RawArray a)

{-# NOINLINE alloc #-}
{-@ assume alloc ::
      n:Nat ->
      Array <(\i -> 0 <= i && i < n), empty> a id @-}
alloc :: Int -> Array a id
alloc n = Array $ unsafePerformDupableIO $ newArray_ (0, n - 1)

{-# NOINLINE get #-}
{-@ assume get ::
      Array <dom, rng> a id ->
      i:(Int <dom>) ->
      Ur (a <{\v -> rng (i, v)}>) @-}
get :: Array a id -> Int -> Ur a
get =  go
  where go :: Array a id -> Int -> Ur a
        go (Array arr) i = Ur $ unsafePerformDupableIO $ do
          v <- M.readArray arr i
          return v

{-# NOINLINE length #-}
{-@ assume length :: Array <dom, rng> a id -> Ur Nat @-}
length :: Array a id -> Ur Int
length =  go
  where go :: Array a id -> Ur Int
        go (Array arr) = Ur $ unsafePerformDupableIO $ do 
          (i, j) <- getBounds arr
          return (j - i + 1)

{-@ newtype UnsafeAlias a id <dom :: Set Int, rng :: Set (Int, a)> =
      UnsafeAlias (Array <dom, rng> a id) @-}
newtype UnsafeAlias a id = UnsafeAlias (Array a id)
 
{-@ assume unsafeAlias :: 
      Array <dom, rng> a id -> 
      (Array <dom, rng> a id, Array <dom, rng> a id) @-}
unsafeAlias :: Array a id -> (Array a id, Array a id)
unsafeAlias =  unsafeAlias'
  where unsafeAlias' :: Array a id -> (Array a id, Array a id)
        unsafeAlias' arr = (arr, arr)

{-@ get2 :: 
      Array <dom, rng> a id -> 
      i:(Int <dom>) -> 
      (Ur (a <{\v -> rng (i, v)}>), Array <dom, rng> a id) @-}
get2 :: Array a id -> Int -> (Ur a, Array a id)
get2 arr i =  go $ unsafeAlias arr
  where go :: (Array a id, Array a id) -> (Ur a, Array a id)
        go (arr1, Array arr2) = (get arr1 i, Array arr2)

{-# NOINLINE set #-}
{-@ assume set ::
      Array <dom, rng> a id ->
      i:(Int <dom>) ->
      v:a ->
      Array <dom, rng -+ (i, v)> @-}
set :: Array a id -> Int -> a -> Array a id
set arr i v =  go v $ unsafeAlias arr
  where go :: a -> (Array a id, Array a id) -> Array a id
        go v (Array arr1, Array arr2) = Array $ unsafePerformDupableIO $ do 
          () <- M.writeArray arr1 i v
          return arr2

{-@ type Length Xs = Int <\n -> n == length Xs> @-}
{-@ length2 :: 
      xs:(Array <dom, rng> a id) -> 
      (Ur (Length xs), Array <dom, rng> a id) @-}
length2 :: Array a id -> (Ur Int, Array a id)
length2 arr =  go $ unsafeAlias arr
  where go :: (Array a id, Array a id) -> (Ur Int, Array a id)
        go (arr1, arr2) = (length arr1, arr2)

{-@ assume split ::
      Array <dom, rng> id a ->
      i:(Int <dom>) ->
      (Array <dom `and` (>=) i, rng // (dom `and` (>=) i)> a 10,
       Array <dom `and` (<) i, rng // (dom `and` (<) i)> id a) @-}
split :: Array id a -> Int -> (Array id a, Array id a)
split arr _ = (arr1, arr2)
  where (arr1, arr2) = unsafeAlias arr

{-@ assume (+:+) ::a
      Array <dom, rng> a id ->
      Array <dom', rng'> a id ->
      Array <dom `or` dom', rng `or` rng'> a id @-}
(+:+) :: Array a id -> Array a id -> Array a id
xs +:+ ys = xs `seq` ys
