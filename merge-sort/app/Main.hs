{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import LinearArray
import Cilksort
import Data.Unrestricted.Internal.Ur

main :: IO ()
main  = test2

-- Error
test2 = let xs :: Array Int () = alloc 3 in
        let xs' = set xs 0 2 in
        let xs'1 = set xs' 1 3 in
        let xs'2 = set xs'1 2 (-200) in
        let (xs'3, tmp) = mergeSortInplace xs'2 (0, 2) $ alloc 3 in
        let (Ur x, src'1) = get2 xs'3 0 in
        let (Ur y, src'2) = get2 src'1 1 in
        let (Ur z, src'3) = get2 src'2 2 in
        do print (x, y, z) 

-- Inlined test2. Fails with error.
test1 :: IO ()
test1 = let xs :: Array Int () = alloc 3 in
        let xs' = set xs 0 2 in
        let xs'1 = set xs' 1 3 in
        let xs'2 = set xs'1 2 (-200) in
        let (xs1, xs2) = split xs'2 1 in
        let (tmp1, tmp2) = split (alloc 3) 1 in
        let (tmp1', xs1') = mergeSortInto tmp1 (0, 1) xs1 in
        let (tmp2', xs2') = mergeSortInto tmp2 (2, 2) xs2 in
        let src = xs1' +:+ xs2' in
        let tmp = tmp1' +:+ tmp2' in
        let (Ur a, tmp'') = get2 tmp 0 in
        let (Ur b, tmp''1) = get2 tmp'' 1 in
        let (Ur c, tmp''2) = get2 tmp2' 2 in
        let (src', tmp') = mergeInto src 0 tmp''2 (0, 1) (2, 2) in
        let (Ur x, src'1) = get2 src' 0 in
        let (Ur y, src'2) = get2 src'1 1 in
        let (Ur z, src'3) = get2 src'2 2 in
        do print (x, y, z) 

-- Same as test1, with nothing but an extra print. Succeeds behaviorally.
test3 :: IO ()
test3 = let xs :: Array Int () = alloc 3 in
        let xs' = set xs 0 2 in
        let xs'1 = set xs' 1 3 in
        let xs'2 = set xs'1 2 (-200) in
        let (xs1, xs2) = split xs'2 1 in
        let (tmp1, tmp2) = split (alloc 3) 1 in
        let (tmp1', xs1') = mergeSortInto tmp1 (0, 1) xs1 in
        let (tmp2', xs2') = mergeSortInto tmp2 (2, 2) xs2 in
        let src = xs1' +:+ xs2' in
        let tmp = tmp1' +:+ tmp2' in
        let (Ur a, tmp'') = get2 tmp 0 in
        let (Ur b, tmp''1) = get2 tmp'' 1 in
        let (Ur c, tmp''2) = get2 tmp2' 2 in
        let (src', tmp') = mergeInto src 0 tmp''2 (0, 1) (2, 2) in
        let (Ur x, src'1) = get2 src' 0 in
        let (Ur y, src'2) = get2 src'1 1 in
        let (Ur z, src'3) = get2 src'2 2 in
        do print (a, b, c); print (x, y, z) 
