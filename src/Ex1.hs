{-# Language DataKinds, NoMonomorphismRestriction, TypeFamilies #-}
{-# Language FlexibleContexts #-}
import ML



type PL1 =
  Language Pure
    [ Mut X Int
    , Val Y Int
    , Collector X Int
    , Backtracks
    , Throws Char
    ]

data X = X
data Y = Y



-- test :: PL1 Int
test = do a <- readVal Y
          b <- getMut  X
          when (a > b) (throw 'a')
          setMut X 11
          pure (a + b)

{-
example = run test
  (X := 3)
  (Y := 2)
-}

example =
  noEffects    $
  throws       $
  backtracks   $
  collector    $
  val (Y := 4) $
  mut (X := 3) $
    test
  






