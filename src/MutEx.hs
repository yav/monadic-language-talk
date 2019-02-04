{-# Language DataKinds, FlexibleContexts #-}
import ML

type PL1 =
  DeclareLanguage
    [ Throws String
    , Mut X Char
    ] Pure

type PL2 =
  DeclareLanguage
    [ Mut X Char
    , Throws String
    ] Pure

data X = X

sample :: (CanThrow String m, HasMut X Char m) => m ()
sample =
  do setMut X 'b'
     throw "Exception!"

ex1 = run (sample :: PL1 ()) (X := 'a')
ex2 = run (sample :: PL2 ()) (X := 'a')

sample2 :: (CanCatch String m, HasMut X Char m) => m String
sample2 =
  do res <- try sample
     case res of
       Success _   -> pure "OK"
       Exception c -> pure c


ex3 = run (sample2 :: PL1 String) (X := 'a')
ex4 = run (sample2 :: PL2 String) (X := 'a')
