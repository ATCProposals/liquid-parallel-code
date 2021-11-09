module Main where

import Lib
import qualified LinearArray as A
import Cilksort
import Data.Array.MArray

main :: IO ()
main = let xs = A.alloc 2 in
       let xs' = xs A.<~ (0, 1) in
       let xs'1 = xs' A.<~ (1, 0) in
       let xs'2 = swapIndex xs'1 0 1 in
       let x = xs'2 `A.get` 0 in
       print x
