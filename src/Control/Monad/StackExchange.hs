{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | 'StackExchangeT' monad transformer
module Control.Monad.StackExchange
  ( StackExchangeT, StackExchange, runStackExchange
  ) where

import Control.Applicative (Alternative, Applicative)
import Control.Monad (MonadPlus)

import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State (StateT, MonadState(..), evalStateT)
import Control.Monad.Trans (MonadIO, MonadTrans)
import Control.Monad.Writer.Class (MonadWriter)

import Network.StackExchange.URI


-- | Encapsulates all libstackexchange functions
newtype StackExchangeT m a = StackExchangeT { runStackExchangeT ∷ StateT URI m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadTrans, MonadState URI)


-- | Convenient type alias since vast majority of users won't need anything except IO
type StackExchange = StackExchangeT IO


deriving instance MonadCont m ⇒ MonadCont (StackExchangeT m)


deriving instance MonadError e m ⇒ MonadError e (StackExchangeT m)


deriving instance MonadReader r m ⇒ MonadReader r (StackExchangeT m)


deriving instance MonadWriter w m ⇒ MonadWriter w (StackExchangeT m)


-- | Run a bunch of libstackexchange functions
runStackExchange ∷ Monad m ⇒ StackExchangeT m a → m a
runStackExchange = flip evalStateT stackexchange . runStackExchangeT
{-# SPECIALIZE runStackExchange ∷ StackExchangeT IO a → IO a #-}
