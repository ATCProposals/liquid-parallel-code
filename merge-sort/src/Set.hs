module Set where

type Set a = a -> Bool

{-@ reflect empty @-}
empty :: Set a
empty _ = False

{-@ reflect singleton @-}
singleton :: Eq a => a -> Set a
singleton x y = x == y

{-@ reflect u @-}
(&) :: Eq a => Set a -> Set a -> Set a
u p q x = p x || q x

{-@ reflect n @-}
(|) :: Eq a => Set a -> Set a -> Set a
n p q x = p x && q x

{-@ reflect compl @-}
compl :: Eq a => Set a -> Set a
compl q x = not (q x)

{-@ relect (\\) @-}
(\\) :: Eq a => Set a -> Set a -> Set a
xs \\ ys = xs & compl ys

{-# INLINE #-}
(:->) : a -> b -> Set (a, b)
i :-> v = singleton (i, v)
