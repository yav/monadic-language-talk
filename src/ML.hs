{-# Language TypeFamilies, DataKinds, TypeOperators #-}
{-# Language MultiParamTypeClasses, FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}
{-# Language RankNTypes #-}
module ML
  ( -- * Basic building blocks
    Pure, IO
  , Val, HasVal(..)
  , Mut, HasMut(..)
  , Throws, CanThrow(..)
  , Backtracks, CanBacktrack
  , Language

    -- * Running programs
  , Run(..)
  , (:=)(..), (:&)(..), EndInit(..)
  , Except(..), Answers(..)

    -- * Monadic combinators
  , module Control.Monad
  ) where

import Control.Monad
import GHC.TypeLits

newtype Pure a        = Pure { unP :: a }
newtype Val x t m a   = Val { unV :: t -> m a }
newtype Mut x t m a   = Mut { unM :: t -> m (a,t) }
newtype Throws x m a  =
  Throws { unT :: forall r. (a -> m (Except x r)) -> m (Except x r) }
newtype Backtracks m a =
  Backtracks { unB :: forall r. (a -> m (Answers m r)) -> m (Answers m r) }

data Except t a   = Exception t | Success a
                    deriving Show

data Answers m a  = NoAnswer | Answer a (Backtracks m a)



instance Monad Pure where
  return a  = Pure a
  m >>= f   = f (unP m)

instance Monad m => Monad (Val x t m) where
  m >>= f   = Val (\r -> unV m r >>= \a -> unV (f a) r)

instance Monad m => Monad (Mut x t m) where
  m >>= f   = Mut (\s -> unM m s >>= \(a,s1) -> unM (f a) s1)

instance Monad m => Monad (Throws t m) where
  m >>= f   = Throws (\k -> unT m (\a -> unT (f a) k))

instance Monad m => Monad (Backtracks m) where
  m >>= f   = Backtracks (\k -> unB m (\a -> unB (f a) k))



type family Language base features :: * -> * where
  Language base (f ': more) = f (Language base more)
  Language base '[]         = base



--------------------------------------------------------------------------------
class Lift t where
  lift :: Monad m => m a -> t m a

instance Lift (Val x t) where
  lift m = Val (\_ -> m)

instance Lift (Mut x t) where
  lift m = Mut (\s -> do a <- m
                         pure (a,s))

instance Lift (Throws t) where
  lift m = Throws (\k -> m >>= k)

instance Lift Backtracks where
  lift m = Backtracks (\k -> m >>= k)


--------------------------------------------------------------------------------
class Monad m => HasVal x t m | m x -> t where
  readVal :: x -> m t

instance {-# OVERLAPPING #-}
  (Monad m, t ~ t') => HasVal x t (Val x t' m) where
  readVal _ = Val (\t -> pure t)

instance {-# OVERLAPPING #-}
  (HasVal x t m, Lift f, Monad (f m)) => HasVal x t (f m) where
  readVal y = lift (readVal y)

instance (TypeError ('Text "Undefined variable " ':<>: 'ShowType x ':<>:
                     'Text " of type " ':<>: 'ShowType t)
         , Monad m, HasVal x t m
         ) => HasVal x t m where
  readVal = undefined




class Monad m => HasMut x t m | m x -> t where
  getMut :: x -> m t
  setMut :: x -> t -> m ()

instance {-# OVERLAPPING #-}
  (Monad m, t ~ t') => HasMut x t (Mut x t' m) where
  getMut _ = Mut (\t -> pure (t,t))
  setMut _ = \s -> Mut (\_ -> pure ((),s))

instance {-# OVERLAPPING #-}
  (HasMut x t m, Lift f, Monad (f m)) =>HasMut x t (f m) where
  getMut x   = lift (getMut x)
  setMut x s = lift (setMut x s)

instance (TypeError ('Text "Undefined mutable variable " ':<>: 'ShowType x ':<>:
                     'Text " of type " ':<>: 'ShowType t)
         , Monad m, HasMut x t m
         ) => HasMut x t m where
  getMut = undefined
  setMut = undefined



class Monad m => CanThrow x m where
  throw :: x -> m a

instance {-# OVERLAPPING #-}
  Monad m => CanThrow x (Throws x m) where
  throw x = Throws (\_ -> pure (Exception x))

instance {-# OVERLAPPING #-}
  (Lift t, Monad (t m), CanThrow x m) => CanThrow x (t m) where
  throw x = lift (throw x)

instance (TypeError ('Text "Program may not throw exceptions of type "
                  ':<>: 'ShowType x )
         , Monad m, CanThrow x m
         ) => CanThrow x m where
  throw = undefined



class Monad m => CanBacktrack m where
  backtrack :: m a
  orElse    :: m a -> m a -> m a

instance Monad m => CanBacktrack (Backtracks m) where
  backtrack     = Backtracks (\_ -> pure NoAnswer)
  m `orElse` n  = Backtracks (\k -> bPlus (unB m k) (unB n k))


bNone :: Monad m => m (Answers m a)
bNone = pure NoAnswer

bPlus :: Monad m => m (Answers m a) -> m (Answers m a) -> m (Answers m a)
bPlus m n = do a1 <- m
               case a1 of
                 NoAnswer -> n
                 Answer a more -> pure (Answer a (more `orElse` bPack n))

bPack :: Monad m => m (Answers m a) -> Backtracks m a
bPack m = Backtracks $ \k ->
            do a <- m
               case a of
                 NoAnswer -> bNone
                 Answer a more -> bPlus (k a) (unB more k)


--------------------------------------------------------------------------------
-- Running all the way

data x := t = x := t
data a :& b = a :& b
data EndInit = EndInit

infixr 2 :&

class Monad m => Run m where
  type family Input  (m :: * -> *)
  type family Output (m :: * -> *) a
  run :: Input m -> m a -> Output m a

instance Run m => Run (Val x t m) where
  type instance Input (Val x t m)     = x := t :& Input m
  type instance Output (Val x t m) a  = Output m a
  run (_ := t :& i) m = run i (unV m t)

instance Run m => Run (Mut x t m) where
  type instance Input (Mut x t m)     = x := t :& Input m
  type instance Output (Mut x t m) a  = Output m (a,t)
  run (_ := t :& i) m = run i (unM m t)

instance Run m => Run (Throws t m) where
  type instance Input (Throws t m)    = Input m
  type instance Output (Throws t m) a = Output m (Except t a)
  run i m = run i (unT m (pure . Success))

instance Run m => Run (Backtracks m) where
  type instance Input (Backtracks m)    = Input m
  type instance Output (Backtracks m) a = Output m (Answers m a)
  run i m = run i (unB m (pure . (`Answer` backtrack)))



instance Run IO where
  type instance Input IO    = EndInit
  type instance Output IO a = IO a
  run _ m = m

instance Run Pure where
  type instance Input Pure    = EndInit
  type instance Output Pure a = a
  run _ m = unP m



--------------------------------------------------------------------------------

instance            Functor Pure            where fmap = liftM
instance Monad m => Functor (Val x t m)     where fmap = liftM
instance Monad m => Functor (Mut x t m)     where fmap = liftM
instance Monad m => Functor (Throws t m)    where fmap = liftM
instance Monad m => Functor (Backtracks m)  where fmap = liftM

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






