{-# Language DataKinds #-}

import ML
import Tree

type BTL =
  DeclareLanguage
    [ Throws String
    , Backtracks
    ]
    Pure

example :: Tree -> BTL ()
example t =
  case t of
    Empty -> backtrack
    Node a l r ->
      if a >= 0
        then example l `orElse` example r
        else throw "Negative"

