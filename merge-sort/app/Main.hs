{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import LinearArray
import Cilksort
import Data.Unrestricted.Internal.Ur

intAlloc n = alloc n 0

main :: IO ()
main  = test1

-- Passes after fixing split
test4 = let xs = intAlloc 2 in
        let xs' = set xs 0 4 in
        let xs'1 = set xs' 1 (-100) in
        let xs'2 = cilkSort xs'1 in
        let (Ur n1, xs'3) = get2 xs'2 0 in
        let (Ur n2, xs'4) = get2 xs'3 1 in
        print (n1, n2)

-- Passes as expected
test3 = let xs = intAlloc 1 in
        let xs' = set xs 0 5 in
        let xs'1 = cilkSort xs' in
        let (Ur n, xs'2) = get2 xs'1 0 in
        print n


-- Set out of bounds like expected
test2 = let xs = intAlloc 0 in
        let xs' = set xs 0 1 in
        let (Ur n, xs'1) = get2 xs' 0 in
        print n

-- Vector out of bounds
test1 = let xs = intAlloc 5 in
        let xs' = set xs 0 3000 in
        let xs'1 = set xs' 1 100 in
        let xs'2 = set xs'1 2 2 in
        let xs'3 = set xs'2 3 (-100) in
        let xs'4 = set xs'3 4 (-2000) in
        let xs'5 = cilkSort xs'4 in
        let (Ur v0, xs'6) = get2 xs'5 0 in
        let (Ur v1, xs'7) = get2 xs'6 1 in
        let (Ur v2, xs'8) = get2 xs'7 2 in
        let (Ur v3, xs'9) = get2 xs'8 3 in
        let (Ur v4, xs'10) = get2 xs'9 4 in
        print (v0, v1, v2, v3, v4)

