{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cilksort where 

import LinearArray
import Data.Unrestricted.Internal.Ur

splitNum :: Int
splitNum = 0

insThres :: Int
insThres = 10

spawn :: a -> a
spawn x = x -- Put in parallel module

sync :: ()
sync = ()

copyInto :: 
  Array a id -> -- return array
  Int -> -- start index
  Array a id' -> -- src array
  Int -> -- src start index
  (Array a id, Array a id')
copyInto ret i src j = 
  let (Ur lenSrc, src') = length2 src in
  if j < lenSrc
  then
    let (Ur v, src'1) = get2 src' j in
    copyInto (set ret i v) (i + 1) src'1 (j + 1)
  else (ret, src)

{-
swapIndex :: Array a id -> Int -> Int -> Array a id
swapIndex xs i j = 
  let (Ur vi, xs') = (xs ! i) in
  let (Ur vj, xs'1) = (xs' ! j) in
  let xs'2 = xs'1 <~ (i, vj) in
  let xs'3 =  xs'2 <~ (j, vi) in
  xs'3

smallestIndex :: 
  (a -> a -> Bool) -> -- cmp
  Array a id -> -- src array
  (Int, Int) -> -- bounds
  Int -> -- assumed smallest
  (Ur Int, Array a id)
smallestIndex lt src (i, j) k
  | i > j = (Ur k, src)
  | otherwise = 
      let (Ur vi, src') = src ! i in
      let (Ur vk, src'1) = src' ! k in
      smallestIndex lt src'1 (i + 1, j) $ if vi `lt` vk then i else k

insertionSortInplace ::
  (a -> a -> Bool) -> -- cmp
  Array a id -> -- src array
  (Int, Int) -> -- bounds
  Array a id
insertionSortInplace lt src (i, j) 
  | i == j = src
  | otherwise = 
      let (Ur k, src') = smallestIndex lt src (i + 1, j) i in
      let src'1 = swapIndex src' i k in
      insertionSortInplace lt src'1 (i + 1, j)

insertionSort ::
  (a -> a -> Bool) ->
  Array a id ->
  Array a id
insertionSort lt src =
  let (Ur len, src') = length2 src in
  insertionSortInplace lt src' (0, len - 1)

partitionInplace ::
  (a -> a -> Bool) -> 
  Array a id -> -- src
  (Int, Int) -> -- bounds
  (Ur Int, Array a id)
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
  Array a id ->  -- src
  (Int, Int) -> -- bounds
  Array a id
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
-}

mergeInto' ::
  Array Int id ->
  Int ->
  Array Int id' ->
  Int ->
  Array Int id' ->
  Int ->
  (Array Int id, Array Int id')
mergeInto' ret i src1 j1 src2 j2 =
  let (Ur len1, src1') = length2 src1 in
  let (Ur len2, src2') = length2 src2 in
  if j1 >= len1
  then 
    let (ret', src2'1) = copyInto ret i src2 j2 in
    (ret', src1' +:+ src2'1)
  else if j2 >= len2
  then 
    let (ret', src1'1) = copyInto ret i src1 j1 in
    (ret', src1'1 +:+ src2')
  else
    let (Ur v1, src1'1) = get2 src1' j1 in
    let (Ur v2, src2'1) = get2 src2' j2 in
    if v1 < v2
    then mergeInto' (set ret i v1) (i + 1) src1'1 (j1 + 1) src2'1 j2
    else mergeInto' (set ret i v2) (i + 1) src1'1 j1 src2'1 (j2 + 1)
    
mergeInto :: 
  Array Int id -> -- return buffer
  Array Int id' -> -- src buffer 1
  Array Int id' -> -- src buffer 2
  (Array Int id, Array Int id')
mergeInto ret src1 src2 = mergeInto' ret 0 src1 0 src2 0

mergeSortInplace ::
  Array Int id -> -- array to be sorted
  Array Int id' -> -- temporary buffer
  (Array Int id, Array Int id') -- src and temporary buffers

-- Destructively sort into return buffer
mergeSortInto :: 
  Array Int id' -> -- return buffer
  Array Int id -> -- src array
  (Array Int id', Array Int id) -- return and src buffers

mergeSortInplace src tmp =
  let (Ur len, src') = length2 src in
  if len <= 1
  then (src', tmp)
  else
    let mid = (len - 1) `div` 2 in
    let (src1, src2) = split src (mid + 1) in
    let (tmp1, tmp2) = split tmp (mid + 1) in
    let (tmp1', src1') = mergeSortInto tmp1 src1 in
    let (tmp2', src2') = mergeSortInto tmp2 src2 in
    let src'1 = src1' +:+ src2' in
    mergeInto src'1 tmp1' tmp2'

mergeSortInto ret src =
  let (Ur len, src') = length2 src in
  if len <= 1 
  then copyInto ret 0 src' 0
  else
    let mid = (len - 1) `div` 2 in
    let (src1, src2) = split src' (mid + 1) in
    let (ret1, ret2) = split ret (mid + 1) in
    let (src1', ret1') = mergeSortInplace src1 ret1 in
    let (src2', ret2') = mergeSortInplace src2 ret2 in
    let ret' = ret1' +:+ ret2' in
    mergeInto ret' src1' src2'

mergeSort :: Array Int id -> Array Int id
mergeSort xs = 
  let (Ur len, xs') = length2 xs in
  fst $ mergeSortInplace xs' $ alloc len 0

cilkSort :: Array Int id -> Array Int id
cilkSort xs = mergeSort xs
