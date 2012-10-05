{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | 'StackExchangeT' monad transformer
module Control.Monad.StackExchange
  ( StackExchangeT, StackExchange, runStackExchange
  , SEError(..)
  ) where

import Control.Applicative (Alternative, Applicative)
import Control.Monad (MonadPlus)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)

import Control.Monad.Error (ErrorT(..), Error(..), MonadError(..), runErrorT)
import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Control.Monad.Trans (MonadIO, MonadTrans(..))

import Network.StackExchange.URI


-- | Encapsulates all libstackexchange functions
newtype StackExchangeT m a = StackExchangeT
  { runStackExchangeT ∷ ErrorT SEError (StateT URI m) a } deriving
    ( Functor, Applicative, Alternative, Monad, MonadPlus
    , MonadIO
    , MonadState URI
    , MonadError SEError
    )


instance MonadTrans StackExchangeT where
  lift ∷ Monad m ⇒ m a → StackExchangeT m a
  lift m = StackExchangeT . ErrorT $ do
    a ← StateT $ \s → m >>= \a → return (a, s)
    return (Right a)


-- | Convenient type alias since vast majority of users won't need anything except IO
type StackExchange = StackExchangeT IO


-- | Run a bunch of libstackexchange functions
runStackExchange ∷ Monad m ⇒ StackExchangeT m a → m (Either SEError a)
runStackExchange = (\v → evalStateT v stackexchange) . runErrorT . runStackExchangeT
{-# SPECIALIZE runStackExchange ∷ StackExchangeT IO a → IO (Either SEError a) #-}


-- | StackExchange API errors
newtype SEError = SEError String
  deriving (Show, Typeable)


instance Error SEError where
  noMsg = SEError "libstackexchange: empty error message"
  strMsg str = SEError $ "libstackexchange: " <> str
