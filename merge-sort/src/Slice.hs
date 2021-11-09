module Slice where
import LinearArray as A

unsafeAlias :: a -> (a, a)
unsafeAlias x = (x, x)

data Slice id a b where Slice :: A.Array a -> Slice id a b
{-@
data Slice id a <p :: Int -> Bool> where
  Slice :: Array a -> Slice <p> id a
@-}

{-@ fromArray :: xs:(Array a) -> Slice <{\i -> 0 <= i && i < length xs}> id a @-}
fromArray :: Array a -> Slice id a
fromArray = Slice

{-@ split :: 
      xs:(Slice <p> id a) -> 
      i:(Int <p>) -> 
      (Slice <{\j -> p j && j < i}> id a,
       Slice <{\j -> p j && i <= j}> id a) @-}
split :: Slice id a -> Int -> (Slice id a, Slice id a)
split (Slice arr) =
