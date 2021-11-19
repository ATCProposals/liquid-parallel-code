module Order where
import LinearArray

{-@ ordered' :: Ord a => 
      xs:(Array a id) ->
      l:{n:Nat | l <= length xs} ->
      {b:Bool | b == ordered' xs k l} @-}
ordered' :: Ord a => Array a id -> Int -> Bool
ordered' arr 0 = True
ordered' arr 1 = True
ordered' arr n = 
  let (Ur v', arr') = get2 arr $ n - 2
  let (Ur v, arr'1) = get2 arr' $ n - 1
  v <= v' && ordered' arr (n - 1)

{-@ reflect ordered @-}
ordered :: Ord a => Array a id -> Bool
ordered arr = 
  let (Ur len, arr) = length2 arr in
  ordered' arr len
  
{-@ p_ordered_len0 :: xs:(SizedArray 0 a id) -> {ordered xs} @-}
p_ordered :: Array a id -> ()
p_ordered arr = _
