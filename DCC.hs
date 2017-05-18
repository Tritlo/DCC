{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module DCC where

import Control.Monad
import Control.Monad.Trans

import Lattice

-- The `T` monad family from the DCC paper
newtype T (l :: Lattice) a = T { unT :: a }

-- Conjunction
type family x && y where
  True  && True  = True
  False && b     = False
  b     && False = False

{- Notion of protected at -}
type family ProtectedAt (t :: *) (l :: Lattice) where
  ProtectedAt (T l' a) l = l <= l'

  -- This diverges from the original DCC calculus,
  -- this is not a problem as () can not transmit any
  -- information
  ProtectedAt ()       l = True

  -- This line requires `UndecidableInstances`, but since the type family is closed
  -- and _obviously_ total, we don't need to worry!
  ProtectedAt (s, t) l = (ProtectedAt s l) && (ProtectedAt t l)
  ProtectedAt (s -> t) l = ProtectedAt t l
  ProtectedAt s l = False

-- The bind from the DCC paper
infixl >>>=

-- Superbind
(>>>=) :: (s `ProtectedAt` l) ~ True => T l a -> (a -> s) -> s
t >>>= f = f (unT t)

-- T is most surely a monad
instance Monad (T l) where
  return = T
  (>>=)  = (>>>=)

instance Applicative (T l) where
  pure  = return
  (<*>) = ap

instance Functor (T l) where
  fmap = liftM

class MonadT mt l where
  liftT :: T l a -> mt a

instance MonadT (T l) l where
  liftT = id

instance (MonadT inner l, Monad inner, MonadTrans mt) => MonadT (mt inner) l where
  liftT = lift . liftT

-- Example from the "Translating Dependency into Parametricity" paper
example :: T L Bool -> T H Bool
example t = t >>>= return
