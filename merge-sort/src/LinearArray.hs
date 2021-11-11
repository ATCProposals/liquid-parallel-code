{-# LANGUAGE LinearTypes #-}

module LinearArray where
import Prelude hiding (length)
import qualified Unsafe.Linear as L
import qualified Data.Array.Mutable.Linear as A

import qualified Set as S

type RawArray a = L.Array a

{-@ newtype Array a id <dom :: Set Int, rng :: Set (Int, a)> = 
      Array (A.IOArray Int a) @-}
newtype Array a id = Array (RawArray a)

{-@ type Nat = Int <\x -> 0 <= x> @-}

{-@ length :: Array <dom, rng> a id -> (Ur Nat, Array <dom, rng> a id) @-}
length :: Array a id %1-> (Ur Int, Array a id)
length (Array arr) = A.size arr

{-@ set :: 
      Array <dom, rng> a id ->
      (i:Int <dom>, x:a) ->
      Array <dom, (rng \\ allFst i) & (i :-> x)> a id @-}
set :: Array a id %1-> (Int, a) -> Array a id
set (Array arr) (i, v) = A.unsafeSet i v arr

{-@ newtype UnsafeAlias a id <dom :: Set Int, rng :: Set (Int, a)> =
      UnsafeAlias (Array <dom, rng> a id) @-}
newtype UnsafeAlias a id = UnsafeAlias (Array a id)
 
{-@ unsafeAlias :: 
      Array <dom, rng> a id -> 
      (UnsafeAlias <dom, rng> a id, UnsafeAlias <dom, rng> a id) @-}
unsafeAlias :: Array a id %1-> (UnsafeAlias a id, UnsafeAlias a id)
unsafeAlias  = coerce unsafeAlias'
  where unsafeAlias' :: Array id a -> (UnsafeAlias a id, UnsafeAlias a id)
        unsafeAlias' arr = (UnsafeAlias arr, UnsafeAlias arr)

{-@ split ::
      Array <dom, rng> id a ->
      i:(Int <dom>) ->
      (Array <dom & (<=) i, rng & {\ix -> i <= fst ix}> id a,
       Array <dom & (>) i, rng & {\ix -> i > fst ix}> id a) @-}
split :: Array id a %1-> Int -> (Array id a, Array id a)
split arr _ = (arr1, arr2)
  where (UnsafeAlias arr1, UnsafeAlias arr2) = unsafeAlias arr

{-@ assume (:+:) ::a
      Array <dom, rng> a id ->
      Array <dom', rng'> a id ->
      Array <dom `u` dom', rng `u` rng'> a id @-}
(:+:) :: Array a id %1-> Array a id %1-> Array a id
xs :+: _ = xs
