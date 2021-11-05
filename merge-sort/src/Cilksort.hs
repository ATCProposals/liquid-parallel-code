{-# LANGUAGE BlockArguments, ScopedTypeVariables #-}
module Cilksort where

import ArrayLike

splitNum :: Int
splitNum = undefined

insThres :: Int
insThres = undefined

spawn :: a -> a
spawn = undefined -- Put in parallel module

sync :: ()
sync = ()

smallestIndex :: (Ord a, LinearArrayLike c) =>
  c a -> -- src array
  (Int, Int) -> -- bounds
  Int -> -- assumed smallest
  (Ur Int, c a)
smallestIndex src (i, j) k
  | i == j = (Ur k, src)
  | otherwise = 
      let (Ur vi, src) = src ! i in
      let (Ur vk, src) = src ! k in
      smallestIndex src (i + 1, j) $ if vi < vk then i else k

insertionSortInplace :: (Ord a, LinearArrayLike c) => 
  c a -> -- src array
  (Int, Int) -> -- bounds
  c a
insertionSortInplace src (i, j) 
  | i == j = src
  | otherwise = 
      let (Ur k, src) = smallestIndex src (i + 1, j) i in
      let src = swapIndex src i k in
      insertionSortInplace src (i + 1, j)

partitionInplace :: (Ord a, LinearArrayLike c) => 
  c a -> -- src
  (Int, Int) -> -- bounds
  (Ur Int, c a)
partitionInplace xs (i, j) =
  let pivot = j in -- TODO: Get a better pivot
  let xs = swapIndex xs j pivot in
  aux xs i pivot
  where aux :: (Ord a, ArrayLike c) => c a -> Int -> Int -> (Ur Int, c a)
        aux xs i pivot =
          if i == pivot 
          then (Ur pivot, xs)
          else
            let (Ur vi, xs) = xs ! i in
            let (Ur vp, xs) = xs ! pivot in
            if vi <= vp
            then aux xs (i + 1) pivot
            else
              let xs = swapIndex xs i (pivot - 1) in
              let xs = swapIndex xs (pivot - 1) pivot in
              aux xs i (pivot - 1)

quickSortInplace :: (Ord a, LinearArrayLike c) => 
  Int ->  -- threshold to start insertion sort
  c a ->  -- src
  (Int, Int) -> -- bounds
  c a
quickSortInplace k src (i, j) = 
  let (Ur len, src) = length2 src in
  if len <= k
  then insertionSortInplace src (i, j)
  else
    let (Ur pivot, src) = partitionInplace src (i, j) in
    let src = spawn $ quickSortInplace k src (i, pivot - 1) in
    let src = quickSortInplace k src (pivot + 1, j) in
    let () = sync in
    src

mergeInto :: (Ord a, ArrayLike c) =>
  c a -> -- return buffer
  Int -> -- start index in return buffer
  c a -> -- src buffer
  (Int, Int) -> -- left subarray
  (Int, Int) -> -- right subarray
  (c a, c a)
mergeInto ret i src (i1, j1) (i2, j2)
  | i1 == j1 = copyInto ret i src i2 (j2 - i2 + 1)
  | i2 == j2 = copyInto ret i src i1 (j1 - i1 + 1)
  | otherwise = 
      let (Ur v1, src) = src ! i1 in
      let (Ur v2, src) = src ! i2 in
      if v1 < v2 
      then mergeInto (ret <~ (i, v1)) (i + 1) src (i1 + 1, j1) (i2, j2)
      else mergeInto (ret <~ (i, v2)) (i + 1) src (i1, j1) (i2 + 1, j2)

mergeSortInplace :: (Ord a, ArrayLike c) => 
  Int -> -- number of times to split
  Int -> -- insertion sort threshold
  c a -> -- array to be sorted
  (Int, Int) -> -- bounds
  c a -> -- temporary buffer
  (c a, c a) -- src and temporary buffers

-- Destructively sort into return buffer
mergeSortInto :: (Ord a, ArrayLike c) =>
  Int -> -- number of times to split
  Int -> -- insertion sort threshold
  c a -> -- array to be sorted
  (Int, Int) -> -- bounds
  c a -> -- return buffer
  (c a, c a) -- src and return buffers

mergeSortInplace 0 k src (i, j) tmp = (quickSortInplace k src (i, j), tmp)
mergeSortInplace n k src (i, j) tmp = 
  let pivot = (j - i + 1) `div` 2 in
  let (src, tmp) = spawn $ mergeSortInto (n - 1) k src (i, pivot) tmp in
  let (src, tmp) = mergeSortInto (n - 1) k src (pivot + 1, j) tmp in
  let () = sync in
  mergeInto src i tmp (i, pivot) (pivot + 1, j)

mergeSortInto 0 k src (i, j) ret = 
  let (ret, src) = copyInto src i ret i (j - i + 1) in
  (src, quickSortInplace k ret (i, j))
mergeSortInto n k src (i, j) ret =
  let pivot = (j - i + 1) `div` 2 in
  let (src, ret) = spawn $ mergeSortInplace (n - 1) k src (i, pivot) ret in
  let (src, ret) = mergeSortInplace (n - 1) k src (pivot + 1, j) ret in
  let () = sync in
  let (ret, src) = mergeInto ret i src (i, pivot) (pivot + 1, j) in
  (src, ret)

mergeSort :: (Ord a, ArrayLike c) => Int -> Int -> c a -> (Int, Int) -> c a
mergeSort n k xs (i, j) = 
  fst $ mergeSortInplace n k xs (i, j) $ alloc (j - i + 1) 0

cilkSort :: (Ord a, ArrayLike c) => c a -> c a
cilkSort xs = 
  let (Ur len, xs) = length2 xs in
  mergeSort splitNum insThres xs (0, len - 1)
