{-# Language DataKinds #-}

import ML

type BL =
  Language IO
    '[ Backtracks ]

say :: String -> BL ()
say x = prim (putStrLn x)


data Tree = Node Int Tree Tree | Empty


search :: Tree -> BL Int
search t =
  case t of
    Node n l r -> do say (show n)
                     pure n `orElse` search l `orElse` search r
    Empty      -> backtrack


