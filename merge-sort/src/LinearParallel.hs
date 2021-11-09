{-# LANGUAGE DataKinds #-}
module LinearParallel where

data SlicePart = W | Tru | Fls 

{-@ newtype Slice i (q :: SlicePart) c <p :: i -> Bool> = Slice (c a) @-}
newtype Slice i (q :: SlicePart) c = Slice c

{-@ split :: 
      Slice <p> i 'W c %1-> 
      (Slice <p> i 'Tru c, 
       Slice <{~p}> i 'Fls c) @-}

{-@ merge :: 
      Slice <p> i 'Tru c %1-> 
      Slice <p'> i 'Fls c %1-> 
      Slice <p> i 'W c @-}

{-@ merge_join :: {uncurry merge (split xs) = xs} @-}

{-@ pure :: c -> Slice <p> i 'W c @-}

{-@ join :: Slice <p> i 'W (Slice <p'> i q c) -> Slice <p'> i q c @-}
