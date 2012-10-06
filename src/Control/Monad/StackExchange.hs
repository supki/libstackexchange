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
--
-- Phantom type @a@ represents authentification mode
newtype StackExchangeT a m α = StackExchangeT
  { runStackExchangeT ∷ ErrorT SEError (StateT URI m) α } deriving
    ( Functor, Applicative, Alternative, Monad, MonadPlus
    , MonadIO
    , MonadState URI
    , MonadError SEError
    )


instance MonadTrans (StackExchangeT a) where
  lift ∷ Monad m ⇒ m α → StackExchangeT a m α
  lift m = StackExchangeT . ErrorT $ do
    a ← StateT $ \s → m >>= \a → return (a, s)
    return (Right a)


-- | Convenient type alias since vast majority of users won't need anything except IO
type StackExchange a = StackExchangeT a IO


-- | Run a bunch of libstackexchange functions
runStackExchange ∷ Monad m ⇒ StackExchangeT a m α → m (Either SEError α)
runStackExchange = (\v → evalStateT v stackexchange) . runErrorT . runStackExchangeT
{-# SPECIALIZE runStackExchange ∷ StackExchangeT a IO α → IO (Either SEError α) #-}


-- | StackExchange API errors
newtype SEError = SEError String
  deriving (Show, Typeable)


instance Error SEError where
  noMsg = SEError "libstackexchange: empty error message"
  strMsg str = SEError $ "libstackexchange: " <> str
