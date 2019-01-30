{-# Language DataKinds #-}
import ML



type PL1 =
  Language Pure
    [ Mut  X Int
    , Val Y Int
    , Backtracks
    , Throws Char
    ]

data X = X
data Y = Y



test :: PL1 Int
test = do a <- readVal Y
          b <- getMut  X
          when (a > b) (throw 'a')
          setMut X 11
          pure (a + b)

example = run (X := 3 :& Y := 5 :& EndInit)
          test




