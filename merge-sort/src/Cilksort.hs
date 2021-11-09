{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cilksort where 

import LinearArray

splitNum :: Int
splitNum = 0

insThres :: Int
insThres = 10

spawn :: a -> a
spawn x = x -- Put in parallel module

sync :: ()
sync = ()

copyInto :: Array a -> Int -> Array a -> Int -> Int -> (Array a, Array a)
copyInto xs _ ys _ 0 = (xs, ys)
copyInto xs i ys j k = 
  let (Ur x, xs') = xs ! i in
  copyInto xs' (i + 1) (ys <~ (j, x)) (j + 1) (k - 1)

swapIndex :: Array a -> Int -> Int -> Array a
swapIndex xs i j = 
  let (Ur vi, xs') = (xs ! i) in
  let (Ur vj, xs'1) = (xs' ! j) in
  let xs'2 = xs'1 <~ (i, vj) in
  let xs'3 =  xs'2 <~ (j, vi) in
  xs'3

smallestIndex :: 
  (a -> a -> Bool) -> -- cmp
  Array a -> -- src array
  (Int, Int) -> -- bounds
  Int -> -- assumed smallest
  (Ur Int, Array a)
smallestIndex lt src (i, j) k
  | i > j = (Ur k, src)
  | otherwise = 
      let (Ur vi, src') = src ! i in
      let (Ur vk, src'1) = src' ! k in
      smallestIndex lt src'1 (i + 1, j) $ if vi `lt` vk then i else k

insertionSortInplace ::
  (a -> a -> Bool) -> -- cmp
  Array a -> -- src array
  (Int, Int) -> -- bounds
  Array a
insertionSortInplace lt src (i, j) 
  | i == j = src
  | otherwise = 
      let (Ur k, src') = smallestIndex lt src (i + 1, j) i in
      let src'1 = swapIndex src' i k in
      insertionSortInplace lt src'1 (i + 1, j)

insertionSort ::
  (a -> a -> Bool) ->
  Array a ->
  Array a
insertionSort lt src =
  let (Ur len, src') = length2 src in
  insertionSortInplace lt src' (0, len - 1)

partitionInplace ::
  (a -> a -> Bool) -> 
  Array a -> -- src
  (Int, Int) -> -- bounds
  (Ur Int, Array a)
partitionInplace lt xs (i, j) =
  let pivot = j in -- TODO: Get a better pivot
  let xs' = swapIndex xs j pivot in
  aux xs' i pivot
  where aux xs'1 i pivot =
          if i == pivot 
          then (Ur pivot, xs'1)
          else
            let (Ur vi, xs'2) = xs'1 ! i in
            let (Ur vp, xs'3) = xs'2 ! pivot in
            if vi `lt` vp
            then aux xs'3 (i + 1) pivot
            else
              let xs'4 = swapIndex xs'3 i (pivot - 1) in
              let xs'5 = swapIndex xs'4 (pivot - 1) pivot in
              aux xs'5 i (pivot - 1)

quickSortInplace ::  
  Int ->  -- threshold to start insertion sort
  (a -> a -> Bool) -> -- cmp
  Array a ->  -- src
  (Int, Int) -> -- bounds
  Array a
quickSortInplace k lt src (i, j) = 
  let (Ur len, src') = length2 src in
  if len <= k
  then insertionSortInplace lt src' (i, j)
  else
    let (Ur pivot, src'1) = partitionInplace lt src' (i, j) in
    let src'2 = spawn $ quickSortInplace k lt src'1 (i, pivot - 1) in
    let src'3 = quickSortInplace k lt src'2 (pivot + 1, j) in
    let () = sync in
    src'3

mergeInto :: 
  (a -> a -> Bool) ->
  Array a -> -- return buffer
  Int -> -- start index in return buffer
  Array a -> -- src buffer
  (Int, Int) -> -- left subarray
  (Int, Int) -> -- right subarray
  (Array a, Array a)
mergeInto lt ret i src (i1, j1) (i2, j2)
  | i1 == j1 = copyInto ret i src i2 (j2 - i2 + 1)
  | i2 == j2 = copyInto ret i src i1 (j1 - i1 + 1)
  | otherwise = 
      let (Ur v1, src') = src ! i1 in
      let (Ur v2, src'1) = src' ! i2 in
      if v1 `lt` v2 
      then mergeInto lt (ret <~ (i, v1)) (i + 1) src'1 (i1 + 1, j1) (i2, j2)
      else mergeInto lt (ret <~ (i, v2)) (i + 1) src'1 (i1, j1) (i2 + 1, j2)

mergeSortInplace ::  
  Int -> -- number of times to split
  Int -> -- insertion sort threshold
  (a -> a -> Bool) -> -- cmp
  Array a -> -- array to be sorted
  (Int, Int) -> -- bounds
  Array a -> -- temporary buffer
  (Array a, Array a) -- src and temporary buffers

-- Destructively sort into return buffer
mergeSortInto :: 
  Int -> -- number of times to split
  Int -> -- insertion sort threshold
  (a -> a -> Bool) -> -- cmp
  Array a -> -- array to be sorted
  (Int, Int) -> -- bounds
  Array a -> -- return buffer
  (Array a, Array a) -- src and return buffers

mergeSortInplace 0 k lt src (i, j) tmp = (quickSortInplace k lt src (i, j), tmp)
mergeSortInplace n k lt src (i, j) tmp = 
  let pivot = (j - i + 1) `div` 2 in
  let (src', tmp') = spawn $ mergeSortInto (n - 1) k lt src (i, pivot) tmp in
  let (src'1, tmp'1) = mergeSortInto (n - 1) k lt src' (pivot + 1, j) tmp' in
  let () = sync in
  mergeInto lt src i tmp (i, pivot) (pivot + 1, j)

mergeSortInto 0 k lt src (i, j) ret = 
  let (ret, src') = copyInto src i ret i (j - i + 1) in
  (src', quickSortInplace k lt ret (i, j))
mergeSortInto n k lt src (i, j) ret =
  let pivot = (j - i + 1) `div` 2 in
  let (src', ret) = spawn $ mergeSortInplace (n - 1) k lt src (i, pivot) ret in
  let (src'1, ret) = mergeSortInplace (n - 1) k lt src' (pivot + 1, j) ret in
  let () = sync in
  let (ret, src'2) = mergeInto lt ret i src'1 (i, pivot) (pivot + 1, j) in
  (src, ret)

mergeSort ::  Int -> Int -> (a -> a -> Bool) -> Array a -> (Int, Int) -> Array a
mergeSort n k lt xs (i, j) = 
  fst $ mergeSortInplace n k lt xs (i, j) $ alloc (j - i + 1)

cilkSort :: (a -> a -> Bool) -> Array a -> Array a
cilkSort lt xs = 
  let (Ur len, xs') = length2 xs in
  mergeSort splitNum insThres lt xs' (0, len - 1) 
