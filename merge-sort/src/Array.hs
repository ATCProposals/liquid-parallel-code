module Array where
import qualified Data.List as L hiding (splitAt)

import ArrayLike hiding (unwrap)

newtype List a = List { unwrap :: [a] }

instance ArrayLike List where
  length = L.length . unwrap
  alloc len x = List $ L.replicate len x

  get i (List xs) = xs L.!! i

  List xs <~ (i, e) = List $ write xs i e 
    where write (x : xs') i e = if i == 0 then e : xs' else write xs' (i - 1) e
          write [] _ _ = undefined

instance Linearizable List where
  from f xs = (f xs, xs)


