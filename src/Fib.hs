{-# Language DataKinds, BlockArguments #-}

import ML

-- Names
data Older = Older
data Old   = Old

-- The languae
type FibFL =
  DeclareLanguage
    [ Mut Old   Int
    , Mut Older Int
    ]
    Pure

-- The program
fibCode :: Int -> FibFL Int
fibCode n =
  do replicateM_ n
      do old   <- getMut Old
         older <- getMut Older
         setMut Older old
         setMut Old (old + older)
     getMut Older


-- Running the program
fib :: Int -> Int
fib n = result
  where
  ((result, _), _) =
    compile
      (fibCode n)
      (Old    := 1)
      (Older  := 0)

