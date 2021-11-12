module Set where
import Prelude hiding (and, or)

type Set a = a -> Bool

{-@ reflect empty @-}
empty :: Set a
empty _ = False

{-@ reflect single @-}
single :: Eq a => a -> Set a
single x y = x == y

{-@ reflect and @-}
and :: Eq a => Set a -> Set a -> Set a
and p q x = p x && q x

{-@ reflect or  @-}
or :: Eq a => Set a -> Set a -> Set a
or p q x = p x || q x

{-@ reflect neg @-}
neg :: Eq a => Set a -> Set a
neg q x = not (q x)

{-@ relect diff @-}
diff :: Eq a => Set a -> Set a -> Set a
diff p q = p `and` neg q

{-@ reflect (-+) @-}
(-+) :: (Eq i, Eq a) => Set (i, a) -> (i, a) -> Set (i, a)
p -+ (i, x) = (p `diff` \(j, _) -> i == j) `and` single (i, x)

{-@ assume (//) :: (Eq i, Eq a) => 
      ixs:(Set (i, a)) -> 
      xs:(Set i) ->
      Set (i <xs>, a) @-}
(//) :: (Eq i, Eq a) => Set (i, a) -> Set i -> Set (i, a)
p // q = p `diff` \(i, _) -> not (q i)

