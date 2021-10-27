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

insertionSort :: (Ord a, Sliceable c) => Slice p c a -> Slice p c a
insertionSort _ = undefined

spawn :: a -> a
spawn = undefined -- Put in parallel module

sync :: ()
sync = ()

split :: (Sliceable c, Sliceable l) =>
  Slice p c a -> -- first array to split
  Slice q l b -> -- second array of equal length
  (Slice ('L p) c a, Slice ('R p) c a,
   Slice ('L q) l b, Slice ('R q) l b,
   Ur Int, -- length of L slices
   Ur Int) -- length of R slices
split (Slice bs xs) (Slice bs' tmp) = 
  let (Ur len, xs) = length2 xs in
  let pivot = len `div` 2 in
  let (xs1, xs2) = ArrayLike.splitAt pivot (Slice bs xs) in
  let (tmp1, tmp2) = ArrayLike.splitAt pivot (Slice bs' tmp) in
  (xs1, xs2, tmp1, tmp2, Ur pivot, Ur (len - pivot))

-- 2-way merge sort inplace
-- TODO: Generalize to n-way
mergeSort' ::
  (Ord a, Sliceable c) => Slice p c a -> Slice p c a -> Slice p c a

-- 2-way merge sort destructive in tmp
mergeSort'' ::
  (Ord a, Sliceable c) => Slice p c a -> Slice p c a -> Slice p c a

mergeSort' xs tmp =
  let (Ur l, xs) = length2 xs in
  if l < insThres
  then insertionSort xs
  else
    let (xs1, xs2, tmp1, tmp2, Ur l1, Ur l2) = split (dup xs) tmp in
    let tmp1 = spawn $ mergeSort'' xs1 tmp1 in
    let tmp2 = mergeSort'' xs2 tmp2 in
    let () = sync in
    mergeInto xs tmp1 l1 tmp2 l2

mergeSort'' xs tmp =
  let (Ur l, xs) = length2 xs in
  if l < insThres
  then 
    let tmp = copy (0, l) xs (0, 1) tmp in
    insertionSort tmp
  else
    let (tmp1, tmp2, xs1, xs2, Ur l1, Ur l2) = split (dup tmp) xs in
    let xs1 = spawn $ mergeSort' xs1 tmp1 in
    let xs2 = mergeSort' xs2 tmp2 in
    let () = sync in
    mergeInto tmp xs1 l1 xs2 l2

mergeSort :: (Ord a, Sliceable c) => Slice 'W c a -> Slice 'W c a
mergeSort xs = 
  let (Ur len, xs) = length2 xs in
  mergeSort' xs $ toSlice (alloc len)

cilksort :: (Ord a, Sliceable c) => c a -> c a
cilksort = fromSlice . mergeSort . toSlice
