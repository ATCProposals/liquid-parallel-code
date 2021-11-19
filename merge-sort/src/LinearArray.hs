{-# LANGUAGE NoImplicitPrelude #-}

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--no-adt"         @-}
{-@ LIQUID "--short-names" @-}

module LinearArray where
import Prelude hiding (length)
import Data.List hiding (length)
import qualified Data.Vector as V
import Data.Unrestricted.Internal.Ur

{-@ type Nat = {i:Int | 0 <= i} @-}

{-@ die :: {v:String | False} -> a @-}
die :: String -> a
die msg = error msg

{-@ inline inRange @-}
{-@ inRange :: Int -> Int -> Int -> Bool @-}
inRange :: Int -> Int -> Int -> Bool
inRange i j k = i <= k && k <= j

type RawArray a = V.Vector a

{-@ data Slice a id = Slice {
      unwrap :: RawArray a,
      leftBound :: Nat,
      rightBound :: {i:Int | i - leftBound + 1 >= 0}
    } @-}
data Slice a id = Slice {
  unwrap :: RawArray a,
  leftBound :: Int,
  rightBound :: Int
} deriving Eq

{-@ type InRange I J = {i:Int | inRange I J i} @-}
{-@ type Exact a X = {x:a | x = X} @-}

{-@ inline length @-}
{-@ length :: Slice a id -> Nat @-}
length :: Slice a id -> Int
length (Slice _ i j) = j - i + 1

{-@ type Length Xs = Exact Nat (length xs) @-}
{-@ type SizedSlice a id N = {xs:(Slice a id) | length xs == N} @-}
{-@ type IndexOf Xs = {i:Int | 0 <= i && i < length Xs} @-}

{-@ alloc :: n:{m:Int | 0 < n} -> a -> SizedSlice a id n @-}
alloc :: Int -> a -> Slice a id
alloc n v = Slice (V.replicate n v) 0 (n - 1)

{-@ type Get a Xs I = Exact a (get Xs I) @-}
{-@ measure get :: Eq a => xs:(Slice a id) -> k:(IndexOf xs) -> Get a xs k @-}
get :: Eq a => Slice a id -> Int -> a
get arr i = unwrap arr V.! i

{-@ get2 :: Eq a =>
      xs:(Slice a id) -> 
      k:(IndexOf xs) -> 
      Get a xs k @-}
get2 :: Eq a => Slice a id -> Int -> a
get2 xs i = get xs i

{-@ set ::
      xs:(Slice a id) ->
      IndexOf xs ->
      a ->
      {ys:(Slice a id) | length xs == length ys} @-}   
set :: Slice a id -> Int -> a -> Slice a id
set arr@(Slice xs j k) i x
  | inRange 0 (length arr - 1) i = Slice (xs V.// [(i, x)]) j k
  | otherwise = die "Set out of bounds"

{-@ length2 :: xs:(Slice a id) -> (Ur (Length xs), Exact (Slice a id) xs) @-}
length2 :: Slice a id -> (Ur Int, Slice a id)
length2 xs = (Ur (length xs), xs)

{-@ split :: 
      xs:(Slice a id) ->
      InRange 0 (length xs) ->
      (Slice a id, Slice a id) @-}
split :: Slice a id -> Int -> (Slice a id, Slice a id)
split (Slice xs j k) i = (Slice xs1 j (j + i), Slice xs2 (j + i) k)
  where (xs1, xs2) = V.splitAt i xs

{-@ (+:+) :: 
      xs:(Slice a id) -> 
      ys:{arr:(Slice a id) | rightBound xs + 1 == leftBound ys} -> 
      {arr:(Slice a id) | length arr == length xs + length ys} @-}
(+:+) :: Slice a id -> Slice a id -> Slice a id
Slice xs i j +:+ Slice ys j' k
  | j + 1 == j' = Slice (xs V.++ ys) i k
  | otherwise = die "Bad bounds!"
