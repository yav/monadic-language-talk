:f

{-# Language DataKinds, BlockArguments, OverloadedStrings #-}

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.String

import ML

type Var    = String
type Con    = String
data Term   = Con Con [Term] | Var Var
data Clause = Term :- [Term]

type Subst  = Map Var Term


apSubstTerm :: Subst -> Term -> Term
apSubstTerm su t =
  case t of
    Var x     -> Map.findWithDefault t x su
    Con c xs  -> Con c (map (apSubstTerm su) xs)

fvsTerm :: Term -> Set Var
fvsTerm t =
  case t of
    Var x     -> Set.singleton x
    Con _ xs  -> Set.unions (map fvsTerm xs)

fvsClause :: Clause -> Set Var
fvsClause (t :- ts) = Set.unions (map fvsTerm (t : ts))

apSubstClause :: Subst -> Clause -> Clause
apSubstClause su (t :- ts) = apSubstTerm su t :- map (apSubstTerm su) ts

apSubstSubst :: Subst -> Subst -> Subst
apSubstSubst su otherSu = apSubstTerm su <$> otherSu

--------------------------------------------------------------------------------

data Su       = Su
data NameSeed = NameSeed
data Db       = Db

type Prolog =
  DeclareLanguage
    [ Val Db [Clause]
    , Mut Su Subst
    , Backtracks
    , Mut NameSeed Int
    ] Pure

freshVar :: Prolog Var
freshVar =
  do n <- getMut NameSeed
     setMut NameSeed (n+1)
     pure ("X" ++ show n)

freshCaluse :: Clause -> Prolog Clause
freshCaluse c =
  do vs <- forM (Set.toList (fvsClause c)) \x ->
             do y <- freshVar
                pure (x, Var y)
     pure (apSubstClause (Map.fromList vs) c)


zonk :: Term -> Prolog Term
zonk t =
  do su <- getMut Su
     pure (apSubstTerm su t)

unify :: Term -> Term -> Prolog ()
unify t1' t2' =
  do t1 <- zonk t1'
     t2 <- zonk t2'
     case (t1,t2) of
       (Var x, _) -> bindVar x t2
       (_, Var x) -> bindVar x t1
       (Con c xs, Con d ys)
          | c == d -> unifyMany xs ys
          | otherwise -> backtrack

unifyMany :: [Term] -> [Term] -> Prolog ()
unifyMany xs' ys' =
  case (xs',ys') of
    (x : xs, y : ys) ->
      do unify x y
         unifyMany xs ys
    ([],[]) -> pure ()
    _       -> backtrack


bindVar :: Var -> Term -> Prolog ()
bindVar x t =
  case t of
    Var y | x == y -> pure ()
    _     | x `Set.member` fvsTerm t -> backtrack
          | otherwise ->
            do su <- getMut Su
               let small = Map.singleton x t
               setMut Su (Map.insert x t (apSubstSubst small su))


solveUsing :: Clause -> Term -> Prolog ()
solveUsing c goal =
  do conc :- subGoals <- freshCaluse c
     unify goal conc
     forM_ subGoals solve


solve :: Term -> Prolog ()
solve t =
  do cs <- readVal Db
     options [ solveUsing c t | c <- cs ]

cut :: Prolog a -> Prolog a
cut m =
  do as <- findUpTo (Just 1) m
     case as of
       []    -> backtrack
       a : _ -> pure a


prolog :: [Clause] -> Maybe Int -> Term -> [ Subst ]
prolog cs lim t = map restrict ans
  where
  (ans,_) = run (solve t)
                (Db       := cs)
                (Su       := Map.empty)
                lim
                (NameSeed := 0)
  restrict (_,su) = Map.restrictKeys su vs
  vs = fvsTerm t


--------------------------------------------------------------------------------

ppTerm :: Term -> String
ppTerm = go (0::Int)
  where
  go p t =
    case t of
      Var x -> "?" ++ x
      Con x ts ->
        case ts of
          [] -> x
          _  -> let doc = unwords (x : map (go 1) ts)
                in if p > 0 then "(" ++ doc ++ ")" else doc

ppSubst :: Subst -> String
ppSubst = unlines . map sh . Map.toList
  where sh (x,y) = x ++ " = " ++ ppTerm y



instance IsString Term where
  fromString x = case x of
                    '?' : y -> Var y
                    _       -> Con x []


testRules :: [Clause]
testRules =
  [ "A" :- [ "B" ]
  , "B" :- []
  , Con "IsT" [ "A" ] :- []
  , Con "IsT" [ "B" ] :- []
  , Con "C"  [ "?X", "?X" ] :- [ Con "IsT" ["?X"] ]
  ]

prove :: Term -> IO ()
prove t =
  case prolog testRules Nothing t of
    []   -> putStrLn "No solution."
    slns ->
      do putStrLn "Solutions:"
         forM_ slns \sln ->
            do putStr (ppSubst sln)
               putStrLn "-----------"

