{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
module Cilksort where
import ArrayLike

splitDepth :: Int
splitDepth = undefined

insThres :: Int
insThres = undefined

quickThres :: Int
quickThres = undefined -- Need to add quicksort

insertionSort :: (Ord a, ArrayLike c) => Slice p c a -> Slice p c a
insertionSort _ = undefined

spawn :: a -> a
spawn = undefined -- Put in parallel module

sync :: ()
sync = ()

split :: Slice p c a -> Slice q l b -> 
         (Slice ('L p) c a, Slice ('R p) c a, 
          Slice ('L q) l b, Slice ('R q) l b)
split (Slice bs xs) (Slice bs' tmp) = 
  let (Ur len, xs) = length2 xs in
  let (xs1, xs2) = ArrayLike.splitAt (len / 2) (Slice bs xs) in
  let (tmp1, tmp2) = ArrayLike.splitAt (len / 2) (Slice bs' tmp) in
  (xs1, xs2, tmp1, tmp2)

mergeSort' :: 
  (Ord a, ArrayLike c) => Slice p c a -> Int -> Slice p c a -> Slice p c a
mergeSort' xs k tmp =
  let (Ur l, xs) = length2 xs in
  if l < insThres
  then insertionSort xs
  else
    let (xs1, xs2, tmp1, tmp2) = split xs (dup tmp) in
    let xs1' = spawn $ mergeSort' (k - 1) tmp1 xs1 in
    let xs2' = mergeSort' (k - 1) tmp2 xs2 in
    let _ = sync in
    let merge xs1' xs2' tmp

mergeSort :: (Ord a, ArrayLike c) => Slice p c a -> Slice p c a
mergeSort xs = 
  let (Ur len, xs) = length2 xs in
  mergeSort' xs splitDepth $ toSlice (alloc len)

cilksort :: (Ord a, ArrayLike c) => c a -> c a
cilksort = fromSlice . mergeSort . toSlice
