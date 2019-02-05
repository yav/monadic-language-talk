{-# Language TypeFamilies, DataKinds, TypeOperators #-}
{-# Language MultiParamTypeClasses, FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}
{-# Language RankNTypes #-}
{-# Language ConstraintKinds #-}
{-# Language ScopedTypeVariables #-}
{-# Language QuantifiedConstraints #-}

-- | This module defines components for modular construction of
-- small domain specific languages.
module ML
  ( -- * Declaring Languages
    DeclareLanguage
  , Feature

    -- ** Primitives
  , Pure, IO

    -- ** Language Features
  , Val
  , Mut
  , Col
  , Throws
  , Backtracks


    -- * Writing Programs

    -- ** Generic Language Combinators
  , replicateM_
  , replicateM
  , forM_
  , forM
  , forever
  , when
  , unless

    -- ** Introducing Specific Effects
  , HasPrim(..)
  , HasVal(..)
  , HasMut(..)
  , HasCol(..)
  , CanThrow(..)
  , CanBacktrack(..), options

    -- * Compiling Programs
  , val, (:=)(..)
  , mut
  , collector
  , throws, Except(..)
  , backtracks
  , noEffects
  , withIO
  , Run(..)

    -- * Nested Effects
  , LetVal(..)
  , CanCollect(..)
  , CanCatch(..), catch
  , CanSearch(..)

    -- * Misc
  , Language
  ) where

import qualified Control.Monad as M
import GHC.TypeLits

-- | A language is another word for monad.
type Language = Monad

-- | A language with no-built in primitives.
newtype Pure a            = P { unP :: a }

-- | Extends language @m@ with an immutable variable @x@ of type @t@.
newtype Val x t m a       = V { unV :: t -> m a }

-- | Extends language @m@ with an mutable variable @x@ of type @t@.
newtype Mut x t m a       = M { unM :: t -> m (a,t) }

-- | Extends language @m@ with a write-only variable @x@ of type @t@.
newtype Col x t m a = C { unC :: m (a, [t] -> [t]) }

-- | Extends language @m@ with support for exception of type @t@.
newtype Throws t m a  =
  T { unT :: forall r. (a -> m (Except t r)) -> m (Except t r) }

-- | Extends language @m@ with support for backtracking.
newtype Backtracks m a =
  B { unB :: forall r. (a -> m (Answers m r)) -> m (Answers m r) }

-- | The result of a program that may throw exceptions.
data Except t a   = Exception t   -- ^ The program threw an exception.
                  | Success a     -- ^ The program completed successfully.
                    deriving Show

xPure :: Language m => a -> m (Except t a)
xPure = pure . Success

xThen ::
  Language m => m (Except t a) -> (a -> m (Except t b)) -> m (Except t b)
xThen m f =
  do res <- m
     case res of
       Exception t -> pure (Exception t)
       Success a   -> f a


-- | The result of a program that may backtrack.
data Answers m a  = NoAnswer      -- ^ There are no more answers.
                  | Answer a (m (Answers m a))
                    -- ^ The program produced an answer, and an another
                    -- program which can produce other answers.

bPure :: Language m => a -> m (Answers m a)
bPure a = pure (Answer a (pure NoAnswer))

bPlus :: Language m => m (Answers m a) -> m (Answers m a) -> m (Answers m a)
bPlus m n = do a1 <- m
               case a1 of
                 NoAnswer -> n
                 Answer a more -> pure (Answer a (more `bPlus` n))

bThen ::
  Language m => m (Answers m a) -> (a -> m (Answers m b)) -> m (Answers m b)
bThen m f =
  do res <- m
     case res of
       NoAnswer -> pure NoAnswer
       Answer a more -> bPlus (f a) (more `bThen` f)

findAll :: Language m => [a] -> m (Answers m a) -> m [a]
findAll xs m = do ans <- m
                  case ans of
                    NoAnswer -> pure (reverse xs)
                    Answer a more -> findAll (a:xs) more

findN :: Language m => Int -> [a] -> m (Answers m a) -> m [a]
findN n xs m = if n <= 0
                then pure (reverse xs)
                else do ans <- m
                        case ans of
                          NoAnswer -> pure (reverse xs)
                          Answer a more -> findN (n-1) (a:xs) more



instance Monad Pure where
  return a  = P a
  m >>= f   = f (unP m)

instance Language m => Monad (Val x t m) where
  m >>= f   = V $ \r -> do a <- unV m r
                           unV (f a) r

instance Language m => Monad (Mut x t m) where
  m >>= f   = M $ \s -> do (a,s1) <- unM m s
                           unM (f a) s1

instance Language m => Monad (Col x t m) where
  m >>= f   = C $ do (a,xs) <- unC m
                     (b,ys) <- unC (f a)
                     pure (b, xs . ys)

instance Language m => Monad (Throws t m) where
  m >>= f   = T (\k -> unT m (\a -> unT (f a) k))

instance Language m => Monad (Backtracks m) where
  m >>= f   = B (\k -> unB m (\a -> unB (f a) k))


-- | Declare a new language with the given primitives, and some additional
-- features.
type family DeclareLanguage features prim :: * -> * where
  DeclareLanguage (f ': more) prim = f (DeclareLanguage more prim)
  DeclareLanguage '[]         prim = prim


--------------------------------------------------------------------------------

-- | Promotes statements in a language to an extended language.
class (forall m. Language m => Language (f m)) => Feature f where
  lift :: Language m => m a -> f m a

instance Feature (Val x t) where
  lift m = V (\_ -> m)

instance Feature (Mut x t) where
  lift m = M (\s -> do a <- m
                       pure (a,s))

instance Feature (Col x t) where
  lift m = C (m >>= \a -> pure (a,id))

instance Feature (Throws t) where
  lift m = T (\k -> m >>= k)

instance Feature Backtracks where
  lift m = B (\k -> m >>= k)


--------------------------------------------------------------------------------


-- | Language @m@ supports an immutable variable @x@ of type @t@.
class Language m => HasVal x t m | m x -> t where
  readVal :: x -> m t
  -- ^ Get the value of variable @x@.

instance {-# OVERLAPPING #-}
  (Language m, t ~ t') => HasVal x t (Val x t' m) where
  readVal _ = V (\t -> pure t)

instance {-# OVERLAPPING #-} (HasVal x t m, Feature f) => HasVal x t (f m) where
  readVal y = lift (readVal y)

instance ( HasVal x t m, Language m
         , TypeError ('Text "Undefined variable " ':<>: 'ShowType x ':<>:
                      'Text " of type " ':<>: 'ShowType t)) =>
         HasVal x t m where
  readVal = undefined




-- | Language @m@ supports a mutable variable @x@ of type @t@.
class Language m => HasMut x t m | m x -> t where
  getMut :: x -> m t
  -- ^ Get the value of variable @x@.

  setMut :: x -> t -> m ()
  -- ^ Set the value of variable @x@.

instance {-# OVERLAPPING #-}
  (Language m, t ~ t') => HasMut x t (Mut x t' m) where
  getMut _ = M (\t -> pure (t,t))
  setMut _ = \s -> M (\_ -> pure ((),s))

instance {-# OVERLAPPING #-}
  (HasMut x t m, Feature f) => HasMut x t (f m) where
  getMut x   = lift (getMut x)
  setMut x s = lift (setMut x s)

instance (TypeError ('Text "Undefined mutable variable " ':<>: 'ShowType x ':<>:
                     'Text " of type " ':<>: 'ShowType t)
         , Language m, HasMut x t m
         ) => HasMut x t m where
  getMut = undefined
  setMut = undefined

-- | Language @m@ supports a write-only variable @x@ of type @t@.
class Language m => HasCol x t m | m x -> t where
  appendTo :: x -> t -> m ()
  -- ^ Append the given value to variable @x@.

instance {-# OVERLAPPING #-}
  (Language m, t ~ t') => HasCol x t (Col x t' m) where
  appendTo _ t = C (pure ((), (t:)))

instance {-# OVERLAPPING #-}
  (HasCol x t m, Feature f) => HasCol x t (f m) where
  appendTo x t = lift (appendTo x t)

instance (TypeError ('Text "Undefined collector variable " ':<>: 'ShowType x
               ':<>: 'Text " of type " ':<>: 'ShowType t)
         , Language m, HasCol x t m
         ) => HasCol x t m where
  appendTo = undefined


-- | Language @m@ supports throwing exceptions of type @t@.
class Language m => CanThrow t m where
  throw :: t -> m a
  -- ^ Throw an exception of type @t@.

instance {-# OVERLAPPING #-}
  Language m => CanThrow t (Throws t m) where
  throw x = T (\_ -> pure (Exception x))

instance {-# OVERLAPPING #-}
  (CanThrow t m, Feature f) => CanThrow t (f m) where
  throw x = lift (throw x)

instance (TypeError ('Text "Program may not throw exceptions of type "
                  ':<>: 'ShowType x )
         , Language m, CanThrow x m
         ) => CanThrow x m where
  throw = undefined


-- | Language @m@ supports backtracking.
class Language m => CanBacktrack m where
  backtrack :: m a
  -- ^ Backtrack, abandoning the current execution path.

  orElse    :: m a -> m a -> m a
  -- ^ Introduce a choice point---if the first computation backtracks,
  -- then proceed with the second.

-- | Combine multiple options using 'orElse'.
options :: CanBacktrack m => [m a] -> m a
options opts =
  case opts of
    []     -> backtrack
    m : ms -> m `orElse` options ms

instance
  Language m => CanBacktrack (Backtracks m) where
  backtrack     = B (\_ -> pure NoAnswer)
  m `orElse` n  = B (\k -> bPlus (unB m k) (unB n k))

instance CanBacktrack m => CanBacktrack (Val x t m) where
  backtrack    = lift backtrack
  m `orElse` n = V $ \t -> unV m t `orElse` unV n t

instance CanBacktrack m => CanBacktrack (Mut x t m) where
  backtrack    = lift backtrack
  m `orElse` n = M $ \t -> unM m t `orElse` unM n t

instance CanBacktrack m => CanBacktrack (Col x t m) where
  backtrack    = lift backtrack
  m `orElse` n = C $ unC m `orElse` unC n

instance CanBacktrack m => CanBacktrack (Throws t m) where
  backtrack    = lift backtrack
  m `orElse` n = T $ \k -> unT m k `orElse` unT n k



--------------------------------------------------------------------------------

-- | Language @m@ supports the primitives defined by language @p@.
class (Language p, Language m) => HasPrim p m | m -> p where
  prim :: p a -> m a
  -- ^ Promote a primitive statement to language @m@

instance HasPrim IO IO where
  prim = id

instance (HasPrim p m, Feature f) => HasPrim p (f m) where
  prim = lift . prim


--------------------------------------------------------------------------------
-- Nested effects

-- | Language @m@ supports temporary modifications to an immutable variable
-- for the duration of the given program block.
class HasVal x t m => LetVal x t m | x m -> t where
  letVal :: x -> t -> m a -> m a

instance {-# OVERLAPPING #-}
  (t ~ t', Language m) => LetVal x t (Val x t' m) where
  letVal _ t m = lift (unV m t)

instance LetVal x t m => LetVal x t (Val y t' m) where
  letVal x t m = V $ \t' -> letVal x t (unV m t')

instance LetVal x t m => LetVal x t (Mut y t' m) where
  letVal x t m = M $ \t' -> letVal x t (unM m t')

instance LetVal x t m => LetVal x t (Col y t' m) where
  letVal x t m = C $ letVal x t (unC m)

instance LetVal x t m => LetVal x t (Throws t' m) where
  letVal x t m = T $ \k -> letVal x t (unT m xPure) `xThen` k

instance LetVal x t m => LetVal x t (Backtracks m) where
  letVal x t m = B $ \k -> letVal x t (unB m bPure) `bThen` k




-- | Language @m@ supports collecting the output of a nested program block.
-- The resulting statement produces no additional output to @x@.
class HasCol x t m => CanCollect x t m where
  collect :: x -> m a -> m (a,[t])

instance CanCollect x t m => CanCollect x t (Val y t' m) where
  collect x m = V $ \t' -> collect x (unV m t')

instance CanCollect x t m => CanCollect x t (Mut y t' m) where
  collect x m = M $ \t' -> do res <- collect x (unM m t')
                              pure (swap res)
    where swap ((a,t'),t) = ((a,t),t')

instance {-# OVERLAPPING #-}
  (t ~ t', Language m) => CanCollect x t (Col x t' m) where
  collect _ m = C $ do (a,xs) <- unC m
                       pure ((a, xs []), id)

instance CanCollect x t m => CanCollect x t (Col y t' m) where
  collect x m = C $ do res <- collect x (unC m)
                       pure (swap res)
    where swap ((a,t'),t) = ((a,t),t')

-- | Note that if an exception is thrown, the output is lost!
-- To avoid this, wrap the inner block in 'try' to ensure that
-- no exception may occur.
instance CanCollect x t m => CanCollect x t (Throws t' m) where
  collect x m = T $ \k -> do res <- collect x (unT m xPure)
                             swap k res
    where swap k (res,o) = case res of
                             Exception t' -> pure (Exception t')
                             Success a    -> k (a,o)


{- | When collecting, each "thread" sees the input that has been produced
from the start of the collection up to when the result was produced,
in a left-most depth-first ordering based on 'orElse'.
Example:

> do appendTo X 1
>    collect X (appendTo X 2 `orElse` appendTo X 3)

The whole comutations has only 1 as an output (2 and 3 are collected),
and then it has two possible results: the left one, where only 2 is observed,
and the right one where both 2 and 3 are observed (1 is not seen as it is
outside the collection).
-}
instance CanCollect x t m => CanCollect x t (Backtracks m) where
  collect x m = B $ \k ->
    let go prev stmt =
          do (res,o) <- collect x stmt
             let new = prev ++ o
             case res of
               NoAnswer      -> pure NoAnswer
               Answer a more -> bPlus (k (a, new)) (go new more)
    in go [] (unB m bPure)




-- | Language @m@ supports runing a local computation and "catching" 
-- exceptions.  The answer of type 'Except' indicates if an exception
-- occured.  The resulting program is guarnateed to not throw an excpetion
-- of the given type.
class CanThrow t m => CanCatch t m where
  try :: m a -> m (Except t a)

instance CanCatch t m => CanCatch t (Val y t' m) where
  try m = V $ \t' -> try (unV m t')

instance CanCatch t m => CanCatch t (Mut y t' m) where
  try m = M $ \t' -> do res <- try (unM m t')
                        pure (swap t' res)
    where swap t' res = case res of
                          Exception t      -> (Exception t, t')
                          Success (a,newT) -> (Success a, newT)

instance CanCatch t m => CanCatch t (Col y t' m) where
  try m = C $ do res <- try (unC m)
                 pure (swap res)
    where swap res = case res of
                       Exception t    -> (Exception t, id)
                       Success (a,xs) -> (Success a, xs)

instance {-# OVERLAPPING #-}
  Language m => CanCatch t (Throws t m) where
  try m = T $ \k -> k =<< unT m xPure

instance CanCatch t m => CanCatch t (Throws t' m) where
  try m = T $ \k -> do res <- try (unT m xPure)
                       swap k res
    where swap k res = case res of
                         Exception t -> k (Exception t)
                         Success mb  -> case mb of
                                          Exception t' -> pure (Exception t')
                                          Success a    -> k (Success a)

instance CanCatch t m => CanCatch t (Backtracks m) where
  try m = B $ \k ->
     let go stmt = do res <- try stmt
                      case res of
                        Exception t -> k (Exception t)
                        Success ans ->
                          case ans of
                            NoAnswer -> pure NoAnswer
                            Answer a more -> bPlus (k (Success a)) (go more)

     in go (unB m bPure)

catch :: CanCatch t m => m a -> (t -> m a) -> m a
m `catch` h =
  do x <- try m
     case x of
       Success a   -> pure a
       Exception t -> h t


-- | Language @m@ supports finding the answers of a local backtracking block.
class CanBacktrack m => CanSearch m where
  findUpTo :: Maybe Int -> m a -> m [a]
  -- ^ The input parameter optionally imposes an upper bound on the number
  -- of answers to return.

instance CanSearch m => CanSearch (Val x t m) where
  findUpTo lim m = V $ \t -> findUpTo lim (unV m t)

-- | This operation does not modify the mutable variable, and the final
-- values of the variable in each branch are discarded.  If they are of
-- interest, return them as part of the result of the computation.
instance CanSearch m => CanSearch (Mut x t m) where
  findUpTo lim m = M $ \t -> do xs <- findUpTo lim (unM m t)
                                pure (map fst xs, t)

-- | This operation does not produce any output.  The outputs of the
-- individaul threads are discarded.  If the output is of interest,
-- then use 'collect' to make it part of the result.
instance CanSearch m => CanSearch (Col x t m) where
  findUpTo lim m = C $ do xs <- findUpTo lim (unC m)
                          pure (map fst xs, id)

-- | Threads that throw an exception count towards the limit, but
-- they do not produce an entry in the final list.  If you'd like to
-- know if a thread would throw an exception, use 'try' to make that
-- part of the result.
instance CanSearch m => CanSearch (Throws t m) where
  findUpTo lim m = T $ \k -> do xs <- findUpTo lim (unT m xPure)
                                k [ x | Success x <- xs ]

instance Language m => CanSearch (Backtracks m) where
  findUpTo lim m = B $ \k ->
    do let stmt = unB m bPure
       ans <- case lim of
                Nothing -> findAll [] stmt
                Just n  -> findN n [] stmt
       k ans

--------------------------------------------------------------------------------
-- Removing feaures, one at a time

-- | Just a convenient notatino for initializing variables.
data x := t = x := t

-- | Given a value for @x@, compile a program that relies on immutable
-- variable @x@, to one that does not use @x@ explicitly.
val :: Language m => x := t -> Val x t m a -> m a
val (_ := t) m = unV m t

-- | Give an initial value for @x@, compile a program that relies on
-- a mutable variable @x@, to one that does not use @x@ explicitly.
-- The resulting program returns the final value of @x@ in addition
-- to its result.
mut :: Language m => x := t -> Mut x t m a -> m (a, t)
mut (_ := t) m = do (a,t') <- unM m t
                    pure (a, t')

-- | Compile a program with a collector @x@ to one that does not
-- collect explicitly.  The new program returns the collected values,
-- in addition to the original program's result.
collector :: Language m => Col x t m a -> m (a, [t])
collector m = do (a,xs) <- unC m
                 pure (a, xs [])

-- | Compile a program that may throw an exception of type @t@ to
-- one that does not do so explicitly.
-- The resulting program will wither produce a sucessful result,
-- or an exception of one was thrown.  See 'Except'.
throws :: Language m => Throws t m a -> m (Except t a)
throws m = unT m (pure . Success)

{- | Compile a program that may backtrack, to one that does not do so
explictly. The resulting program may return zero or more answers.
The input parameter optionally specifies an upper bound on the
nubmer of required answers. -}
backtracks :: Language m => Maybe Int -> Backtracks m a -> m [a]
backtracks lim m0 = case lim of
                      Nothing -> findAll [] prog
                      Just n  -> findN n [] prog
  where prog = unB m0 bPure

-- | A program with no effects can be compiled to a value.
noEffects :: Pure a -> a
noEffects = unP

-- | Primitive operations are not compiled any further.
withIO :: IO a -> IO a
withIO = id


-- | Compile a program all the way to the primitive language.
class Language m => Run m where
  type ExeResult m a
  run :: m a -> ExeResult m a

instance Run m => Run (Val x t m) where
  type ExeResult (Val x t m) a = x := t -> ExeResult m a
  run m x = run (val x m)

instance Run m => Run (Mut x t m) where
  type ExeResult (Mut x t m) a = x := t -> ExeResult m (a,t)
  run m x = run (mut x m)

instance Run m => Run (Col x t m) where
  type ExeResult (Col x t m) a = ExeResult m (a, [t])
  run m = run (collector m)

instance Run m => Run (Throws t m) where
  type ExeResult (Throws t m) a = ExeResult m (Except t a)
  run m = run (throws m)

instance Run m => Run (Backtracks m) where
  type ExeResult (Backtracks m) a = Maybe Int -> ExeResult m [a]
  run m n = run (backtracks n m)

instance Run Pure where
  type ExeResult Pure a = a
  run = noEffects

instance Run IO where
  type ExeResult IO a = IO a
  run = withIO
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

instance               Functor Pure              where fmap = M.liftM
instance Language m => Functor (Val x t m)       where fmap = M.liftM
instance Language m => Functor (Mut x t m)       where fmap = M.liftM
instance Language m => Functor (Col x t m) where fmap = M.liftM
instance Language m => Functor (Throws t m)      where fmap = M.liftM
instance Language m => Functor (Backtracks m)    where fmap = M.liftM

instance Applicative Pure where
  pure  = return
  (<*>) = M.ap

instance Language m => Applicative (Val x t m) where
  pure  = lift . pure
  (<*>) = M.ap

instance Language m => Applicative (Mut x t m) where
  pure  = lift . pure
  (<*>) = M.ap

instance Language m => Applicative (Throws t m) where
  pure  = lift . pure
  (<*>) = M.ap

instance Language m => Applicative (Backtracks m) where
  pure  = lift . pure
  (<*>) = M.ap

instance Language m => Applicative (Col x t m) where
  pure  = lift . pure
  (<*>) = M.ap


--------------------------------------------------------------------------------

-- | Repeat a statement some number of times, ignoring its result.
replicateM_ :: Language m => Int -> m a -> m ()
replicateM_ = M.replicateM_

-- | Repeat a statement some number of times, collecting the results.
replicateM :: Language m => Int -> m a -> m [a]
replicateM = M.replicateM

-- | Repeat a statement for each member of a collection, ignoring the
-- statemnt's result.
forM_ :: (Foldable col, Language m) => col a -> (a -> m b) -> m ()
forM_ = M.forM_

-- | Repeat a statement for each member of a collection, and produce a
-- new collection containing the answers.
forM :: (Traversable col, Language m) => col a -> (a -> m b) -> m (col b)
forM = M.forM

-- | Repeat the given statement forever, ignore its result.
forever :: Language m => m a -> m b
forever = M.forever

-- | Do the statement when the condition is true.
when :: Language m => Bool -> m () -> m ()
when = M.when

-- | Do the statement unless the condition is true (i.e., when it is false).
unless :: Language m => Bool -> m () -> m ()
unless = M.unless


