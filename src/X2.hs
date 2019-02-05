{-# Language DataKinds #-}

import ML

type X2 = DeclareLanguage
  [ Throws Bool
  , Throws Char
  ] Pure


example :: X2 Char
example
  = do throw True
       throw 'a'

