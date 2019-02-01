{-# Language DataKinds #-}

import ML
import Tree

type BTL =
  Language Pure
    [ Throws String
    , Backtracks
    ]

example :: Tree -> BTL ()
example t =
  case t of
    Empty -> backtrack
    Node a l r ->
      if a >= 0
        then example l `orElse` example r
        else throw "Negative"

