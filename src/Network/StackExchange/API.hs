{-# LANGUAGE UnicodeSyntax #-}
module Network.StackExchange.API
  ( localState, request
  ) where

import Control.Monad.State (MonadState, get, put)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit (simpleHttp)

import Control.Monad.StackExchange (StackExchangeT)
import Network.StackExchange.URI


localState ∷ MonadState s m ⇒ m α → m α
localState m = get >>= \s → m >>= \v → put s >> return v
{-# INLINE localState #-}


request ∷ MonadIO m ⇒ StackExchangeT a m ByteString
request = get >>= liftIO . simpleHttp . render
{-# INLINE request #-}
