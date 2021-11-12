{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LinearArray where
import Prelude hiding (length)

import qualified Data.Array.IO as A
import Data.Array.MArray as M
import Data.Unrestricted.Internal.Ur
import Data.Unrestricted.Linear
import System.IO.Unsafe
import Unsafe.Linear

import qualified Set as S

type RawArray a = A.IOArray Int a

{-@ newtype Array a id <dom :: Set Int, rng :: Set (Int <dom>, a)> = 
      Array (RawArray a) @-}
newtype Array a id = Array (RawArray a)

{-# INLINE get #-}
{-@ assume get ::
      Array <dom, rng> a id ->
      i:(Int <dom>) ->
      Ur (a <{\v -> rng (i, v)}>) @-}
get :: Array a id %1-> Int -> Ur a
get = toLinear go
  where go :: Array a id -> Int -> Ur a
        go (Array arr) i = Ur $ unsafePerformIO $ do
          v <- M.readArray arr i
          return v

{-# INLINE length #-}
{-@ assume length :: Array <dom, rng> a id -> Ur (Int <{\n -> 0 <= n}>) @-}
length :: Array a id %1-> Ur Int
length = toLinear go
  where go :: Array a id -> Ur Int
        go (Array arr) = Ur $ unsafePerformIO $ do 
          (i, j) <- getBounds arr
          return (j - i + 1)

{-@ newtype UnsafeAlias a id <dom :: Set Int, rng :: Set (Int, a)> =
      UnsafeAlias (Array <dom, rng> a id) @-}
newtype UnsafeAlias a id = UnsafeAlias (Array a id)
 
{-@ assume unsafeAlias :: 
      Array <dom, rng> a id -> 
      (Array <dom, rng> a id, Array <dom, rng> a id) @-}
unsafeAlias :: Array a id %1-> (Array a id, Array a id)
unsafeAlias = toLinear unsafeAlias'
  where unsafeAlias' :: Array a id -> (Array a id, Array a id)
        unsafeAlias' arr = (arr, arr)

{-@ get2 :: 
      Array <dom, rng> a id -> 
      i:(Int <dom>) -> 
      (Ur (a <{\v -> rng (i, v)}>), Array <dom, rng> a id) @-}
get2 :: Array a id %1-> Int -> (Ur a, Array a id)
get2 arr i = toLinear go $ unsafeAlias arr
  where go :: (Array a id, Array a id) %1-> (Ur a, Array a id)
        go (arr1, Array arr2) = (get arr1 i, Array arr2)

{-# INLINE set #-}
{-@ assume set ::
      Array <dom, rng> a id ->
      i:(Int <dom>) ->
      v:a ->
      Array <dom, rng -+ (i, v)> @-}
set :: Array a id %1-> Int -> a -> Array a id
set arr i v = toLinear go v $ unsafeAlias arr
  where go :: a -> (Array a id, Array a id) -> Array a id
        go v (Array arr1, Array arr2) = Array $ unsafePerformIO $ do 
          M.writeArray arr1 i v
          return arr2

{-@ type Length Xs = Int <\n -> n == length Xs> @-}
{-@ length2 :: 
      xs:(Array <dom, rng> a id) -> 
      (Ur (Length xs), Array <dom, rng> a id) @-}
length2 :: Array a id -> (Ur Int, Array a id)
length2 arr = toLinear go $ unsafeAlias arr
  where go :: (Array a id, Array a id) %1-> (Ur Int, Array a id)
        go (arr1, arr2) = (length arr1, arr2)

{-@ assume split ::
      Array <dom, rng> id a ->
      i:(Int <dom>) ->
      (Array <dom `and` (>) i, rng // (dom `and` (>) i)> id a,
       Array <dom `and` (<=) i, rng // (dom `and` (<=) i)> id a) @-}
split :: Array id a %1-> Int -> (Array id a, Array id a)
split arr _ = (arr1, arr2)
  where (arr1, arr2) = unsafeAlias arr

{-@ assume (+:+) ::a
      Array <dom, rng> a id ->
      Array <dom', rng'> a id ->
      Array <dom `or` dom', rng `or` rng'> a id @-}
(+:+) :: Array a id %1-> Array a id %1-> Array a id
xs +:+ ys = xs `lseq` ys
