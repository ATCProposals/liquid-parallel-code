module Array where
import qualified Data.List as L hiding (splitAt)

import ArrayLike hiding (unwrap)

newtype List a = List { unwrap :: [a] }

instance ArrayLike List where
  length = L.length . unwrap
  length2 xs = (Ur (ArrayLike.length xs), xs)
  alloc len = List $ L.replicate len undefined

  (List xs) ! i = xs L.!! i

  (List xs) <~ (i, e) = List $ write xs i e 
    where write (x : xs') i e = if i == 0 then e : xs' else write xs' (i - 1) e
          write [] _ _ = undefined

  splitAt i (Slice (j, k) (List xs)) = (ls1, ls2)
    where xs1 = take i xs
          xs2 = drop i xs
          ls1 = Slice (j, j + i) (List xs1)
          ls2 = Slice (j + i + 1, k) (List xs2)

  mergeInto tmp (Slice (j, _) (List xs)) lx (Slice (_, k) (List ys)) ly = 
    Slice (j, k) $ List $ aux xs ys
    where aux [] ys = ys
          aux xs [] = xs
          aux (x : xs) (y : ys) =
            if x < y 
            then x : aux xs (y : ys)
            else y : aux (x : xs) ys       

  
