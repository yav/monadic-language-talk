{-# Language DataKinds, BlockArguments #-}

import ML


data Older = Older
data Old   = Old

type FibFL =
  Language Pure
    '[ Mut Old Int
     , Mut Older Int
     ]



fibM :: Int -> FibFL ()
fibM n =
  replicateM_ n
  do old   <- getMut Old
     older <- getMut Older
     setMut Older old
     setMut Old (old + older)

fib n = run setup
  do fibM n
     getMut Old
  where
  setup =
    Old   := 1 :&
    Older := 1 :&
    EndInit

