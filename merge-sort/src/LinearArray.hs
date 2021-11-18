{-# LANGUAGE NoImplicitPrelude #-}
module LinearArray where
import Prelude hiding (length)
import Data.List hiding (length)
import qualified Data.Vector as V
import Data.Unrestricted.Internal.Ur

type RawArray a = V.Vector a

data Array a id = Array {
  unwrap :: RawArray a,
  bounds :: (Int, Int)
} deriving Eq

{-@ type Nat = {i:Int | 0 <= i} @-}
{-@ type BoundNat N = {i:Nat | i <= N} @-}
{-@ type Exact X = {x:_ | x == X} @-}

{-@ type Length Xs = Exact (length xs) @-}
{-@ length :: Array a id -> Length xs @-}
length :: Array a id -> Int
length (Array _ (i, j)) = j - i + 1

{-@ type SizedArray N a id = {xs:(Array a id) | length xs == N} @-}
{-@ type IndexOf Xs = BoundNat (length Xs) @-}

{-@ alloc :: n:Int -> a -> SizedArray n a id @-}
alloc :: Int -> a -> Array a id
alloc n v = Array (V.replicate n v) (0, n - 1)

{-@ type Get Xs I = Exact (get Xs I) @-}
{-@ get :: xs:(Array a id) -> k:(IndexOf xs) -> Get xs k @-}
get :: Array a id -> Int -> Ur a
get (Array xs (j, k)) i
  | 0 <= i && i <= k - j = Ur $ xs V.! i
  | otherwise = error "Get out of bounds"

{-@ get2 :: 
      xs:(Array a id) -> 
      k:(IndexOf xs) -> 
      (Ur (Get xs k), Exact xs) @-}
get2 :: Array a id -> Int -> (Ur a, Array a id)
get2 xs i = (get xs i, xs)

{-@ set ::
      xs:(SizedArray n a id) ->
      IndexOf xs ->
      a ->
      SizedArray n a id @-}   
set :: Array a id -> Int -> a -> Array a id
set (Array xs (j, k)) i x
  | 0 <= i && i <= k - j = Array (xs V.// [(i, x)]) (j, k)
  | otherwise = error "Set out of bounds"

{-@ length2 :: xs:(Array a id) -> (Ur (Length xs), Exact xs) @-}
length2 :: Array a id -> (Ur Int, Array a id)
length2 xs = (Ur (length xs), xs)

{-@ split :: 
      xs:(Array a id) ->
      IndexOf xs ->
      (Array a id, Array a id) @-}
split :: Array a id -> Int -> (Array a id, Array a id)
split (Array xs (j, k)) i = (Array xs1 (j, j + i - 1), Array xs2 (j + i, k))
  where (xs1, xs2) = V.splitAt i xs

{-@ (+:+) :: SizedArray n a id -> SizedArray m a id -> SizedArray (m + n) a id @-}
(+:+) :: Array a id -> Array a id -> Array a id
Array xs (i, j) +:+ Array ys (j', k)
  | j + 1 == j' = Array (xs V.++ ys) (i, k)
  | otherwise = error "Bad bounds!"
