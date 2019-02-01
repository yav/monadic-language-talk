{-# Language TypeFamilies, DataKinds, TypeOperators #-}
{-# Language MultiParamTypeClasses, FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}
{-# Language RankNTypes #-}
{-# Language ConstraintKinds #-}
{-# Language ScopedTypeVariables #-}
module ML
  ( -- * Basic building blocks
    Pure, IO, HasPrim(..)
  , Val, HasVal(..)
  , Mut, HasMut(..)
  , Collector, HasCollector(..)
  , Throws, CanThrow(..)
  , Backtracks, CanBacktrack(..)
  , Language

    -- * Running programs
  , val, mut, collector
  , throws, backtracks
  , noEffects, withIO
  , Run(..)
  , (:=)(..)
  , Except(..), Answers(..)

    -- * Nested Effects
  , LetVal(..)
  , NewMut(..)
  , CanCollect(..)
  , CanHandle(..)
  -- CanSearch?

    -- * Monadic combinators
  , module Control.Monad
  ) where

import Control.Monad
import GHC.TypeLits

newtype Pure a            = P { unP :: a }
newtype Val x t m a       = V { unV :: t -> m a }
newtype Mut x t m a       = M { unM :: t -> m (a,t) }
newtype Collector x t m a = C { unC :: m (a, [t] -> [t]) }

newtype Throws x m a  =
  T { unT :: forall r. (a -> m (Except x r)) -> m (Except x r) }
newtype Backtracks m a =
  B { unB :: forall r. (a -> m (Answers m r)) -> m (Answers m r) }

data Except t a   = Exception t | Success a
                    deriving Show

data Answers m a  = NoAnswer | Answer a (m (Answers m a))

instance Show a => Show (Answers m a) where
  showsPrec n x = case x of
                    NoAnswer -> showString "NoAnswer"
                    Answer a _ ->
                      showParen (n > 1) (showString "Answer " . showsPrec 1 a)


instance Monad Pure where
  return a  = P a
  m >>= f   = f (unP m)

instance Monad m => Monad (Val x t m) where
  m >>= f   = V $ \r -> do a <- unV m r
                           unV (f a) r

instance Monad m => Monad (Mut x t m) where
  m >>= f   = M $ \s -> do (a,s1) <- unM m s
                           unM (f a) s1

instance Monad m => Monad (Collector x t m) where
  m >>= f   = C $ do (a,xs) <- unC m
                     (b,ys) <- unC (f a)
                     pure (b, xs . ys)

instance Monad m => Monad (Throws t m) where
  m >>= f   = T (\k -> unT m (\a -> unT (f a) k))

instance Monad m => Monad (Backtracks m) where
  m >>= f   = B (\k -> unB m (\a -> unB (f a) k))


type family Language base features :: * -> * where
  Language base (f ': more) = f (Language base more)
  Language base '[]         = base


--------------------------------------------------------------------------------
class Lift f where
  lift :: Monad m => m a -> f m a

instance Lift (Val x t) where
  lift m = V (\_ -> m)

instance Lift (Mut x t) where
  lift m = M (\s -> do a <- m
                       pure (a,s))

instance Lift (Collector x t) where
  lift m = C (m >>= \a -> pure (a,id))

instance Lift (Throws t) where
  lift m = T (\k -> m >>= k)

instance Lift Backtracks where
  lift m = B (\k -> m >>= k)


--------------------------------------------------------------------------------

type LiftCtxt c f m = (c m, Lift f, Monad (f m))


class Monad m => HasVal x t m | m x -> t where
  readVal :: x -> m t

instance {-# OVERLAPPING #-}
  (Monad m, t ~ t') => HasVal x t (Val x t' m) where
  readVal _ = V (\t -> pure t)

instance {-# OVERLAPPING #-}
  LiftCtxt (HasVal x t) f m => HasVal x t (f m) where
  readVal y = lift (readVal y)

instance ( HasVal x t m, Monad m
         , TypeError ('Text "Undefined variable " ':<>: 'ShowType x ':<>:
                      'Text " of type " ':<>: 'ShowType t)) =>
         HasVal x t m where
  readVal = undefined




class Monad m => HasMut x t m | m x -> t where
  getMut :: x -> m t
  setMut :: x -> t -> m ()

instance {-# OVERLAPPING #-}
  (Monad m, t ~ t') => HasMut x t (Mut x t' m) where
  getMut _ = M (\t -> pure (t,t))
  setMut _ = \s -> M (\_ -> pure ((),s))

instance {-# OVERLAPPING #-}
  LiftCtxt (HasMut x t) f m => HasMut x t (f m) where
  getMut x   = lift (getMut x)
  setMut x s = lift (setMut x s)

instance (TypeError ('Text "Undefined mutable variable " ':<>: 'ShowType x ':<>:
                     'Text " of type " ':<>: 'ShowType t)
         , Monad m, HasMut x t m
         ) => HasMut x t m where
  getMut = undefined
  setMut = undefined

class Monad m => HasCollector x t m | m x -> t where
  appendTo :: x -> t -> m ()

instance {-# OVERLAPPING #-}
  (Monad m, t ~ t') => HasCollector x t (Collector x t' m) where
  appendTo _ t = C (pure ((), (t:)))

instance {-# OVERLAPPING #-}
  LiftCtxt (HasCollector x t) f m => HasCollector x t (f m) where
  appendTo x t = lift (appendTo x t)

instance (TypeError ('Text "Undefined collector variable " ':<>: 'ShowType x
               ':<>: 'Text " of type " ':<>: 'ShowType t)
         , Monad m, HasCollector x t m
         ) => HasCollector x t m where
  appendTo = undefined



class Monad m => CanThrow x m where
  throw :: x -> m a

instance {-# OVERLAPPING #-}
  Monad m => CanThrow t (Throws t m) where
  throw x = T (\_ -> pure (Exception x))

instance {-# OVERLAPPING #-}
  LiftCtxt (CanThrow t) f m => CanThrow t (f m) where
  throw x = lift (throw x)

instance (TypeError ('Text "Program may not throw exceptions of type "
                  ':<>: 'ShowType x )
         , Monad m, CanThrow x m
         ) => CanThrow x m where
  throw = undefined



class Monad m => CanBacktrack m where
  backtrack :: m a
  orElse    :: m a -> m a -> m a

instance
  Monad m => CanBacktrack (Backtracks m) where
  backtrack     = B (\_ -> pure NoAnswer)
  m `orElse` n  = B (\k -> bPlus (unB m k) (unB n k))

bOne :: Monad m => a -> Answers m a
bOne a = Answer a (pure NoAnswer)

bPlus :: Monad m => m (Answers m a) -> m (Answers m a) -> m (Answers m a)
bPlus m n = do a1 <- m
               case a1 of
                 NoAnswer -> n
                 Answer a more -> pure (Answer a (more `bPlus` n))

instance CanBacktrack m => CanBacktrack (Val x t m) where
  backtrack    = lift backtrack
  m `orElse` n = V $ \t -> unV m t `orElse` unV n t

instance CanBacktrack m => CanBacktrack (Mut x t m) where
  backtrack    = lift backtrack
  m `orElse` n = M $ \t -> unM m t `orElse` unM n t

instance CanBacktrack m => CanBacktrack (Collector x t m) where
  backtrack    = lift backtrack
  m `orElse` n = C $ unC m `orElse` unC n

instance CanBacktrack m => CanBacktrack (Throws t m) where
  backtrack    = lift backtrack
  m `orElse` n = T $ \k -> unT m k `orElse` unT n k



--------------------------------------------------------------------------------

class (Monad p, Monad m) => HasPrim p m | m -> p where
  prim :: p a -> m a


instance HasPrim Pure Pure where
  prim = id

instance HasPrim IO IO where
  prim = id

instance LiftCtxt (HasPrim p) f m => HasPrim p (f m) where
  prim = lift . prim


--------------------------------------------------------------------------------
-- Nested effects

class HasVal x t m => LetVal x t m | x m -> t where
  letVal :: x -> t -> m a -> m a

instance {-# OVERLAPPING #-}
  (t ~ t', Monad m) => LetVal x t (Val x t' m) where
  letVal _ t m = lift (unV m t)


class NewMut x t m where
  newMut :: x -> t -> m a -> m (a,t)

class HasCollector x t m => CanCollect x t m where
  collect :: x -> m a -> m (a,[t])

class CanThrow t m => CanHandle t m where
  try :: m a -> m (Except t a)





--------------------------------------------------------------------------------
-- Removing feaures, one at a time

data x := t = x := t

val :: Monad m => x := t -> Val x t m a -> m a
val (_ := t) m = unV m t

mut :: Monad m => x := t -> Mut x t m a -> m (a,t)
mut (_ := t) m = unM m t

collector :: Monad m => Collector x t m a -> m (a,[t])
collector m = do (a,xs) <- unC m
                 pure (a, xs [])

throws :: Monad m => Throws t m a -> m (Except t a)
throws m = unT m (pure . Success)

backtracks :: Monad m => Backtracks m a -> m (Answers m a)
backtracks m = unB m (pure . bOne)

noEffects :: Pure a -> a
noEffects = unP

withIO :: IO a -> IO a
withIO = id


class Monad m => Run m where
  type Result m a
  run :: m a -> Result m a

instance Run m => Run (Val x t m) where
  type Result (Val x t m) a = x := t -> Result m a
  run m x = run (val x m)

instance Run m => Run (Mut x t m) where
  type Result (Mut x t m) a = x := t -> Result m (a,t)
  run m x = run (mut x m)

instance Run m => Run (Collector x t m) where
  type Result (Collector x t m) a = Result m (a,[t])
  run m = run (collector m)

instance Run m => Run (Throws t m) where
  type Result (Throws t m) a = Result m (Except t a)
  run m = run (throws m)

instance Run m => Run (Backtracks m) where
  type Result (Backtracks m) a = Result m (Answers m a)
  run m = run (backtracks m)

instance Run Pure where
  type Result Pure a = a
  run = noEffects

instance Run IO where
  type Result IO a = IO a
  run = withIO
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

instance            Functor Pure              where fmap = liftM
instance Monad m => Functor (Val x t m)       where fmap = liftM
instance Monad m => Functor (Mut x t m)       where fmap = liftM
instance Monad m => Functor (Collector x t m) where fmap = liftM
instance Monad m => Functor (Throws t m)      where fmap = liftM
instance Monad m => Functor (Backtracks m)    where fmap = liftM

instance Applicative Pure where
  pure  = return
  (<*>) = ap

instance Monad m => Applicative (Val x t m) where
  pure  = lift . pure
  (<*>) = ap

instance Monad m => Applicative (Mut x t m) where
  pure  = lift . pure
  (<*>) = ap

instance Monad m => Applicative (Throws t m) where
  pure  = lift . pure
  (<*>) = ap

instance Monad m => Applicative (Backtracks m) where
  pure  = lift . pure
  (<*>) = ap

instance Monad m => Applicative (Collector x t m) where
  pure  = lift . pure
  (<*>) = ap





