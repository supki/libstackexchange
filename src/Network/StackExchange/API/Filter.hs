{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- | StackExchange API calls returning filters.
--
-- This module is not intended for importing, but you can import it if you want.
-- All API modules are re-exported by "Network.StackExchange"
--
-- Filters allow to exclude (or include) every field from responses. They are particularly useful for decreasing response time, simplifing responses, etc.
--
-- More information is available at <https://api.stackexchange.com/docs/filters> and <https://api.stackexchange.com/docs/types/filter>
module Network.StackExchange.API.Filter
  ( createFilter, readFilter
  ) where

import Control.Monad (liftM)

import           Control.Lens
import           Control.Monad.Trans (MonadIO)
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Lazy as AP
import           Data.Monoid.Lens ((<>=))
import           Data.Text.Lazy (Text, intercalate)

import Control.Monad.StackExchange (StackExchangeT)
import Network.StackExchange.API
import Network.StackExchange.JSON
import Network.StackExchange.URI
import Network.StackExchange.Types


-- | <https://api.stackexchange.com/docs/create-filter>
createFilter ∷ MonadIO m ⇒ [Text] → [Text] → Text → StackExchangeT a m (SE Filter)
createFilter (intercalate ";" → include) (intercalate ";" → exclude) base = localState $ do
  uriPath <>= ["filter", "create"]
  uriQuery <>= [("include",include),("exclude",exclude),("base",base)]
  AP.parse A.json `liftM` request >>= \case
    AP.Done _ s → return $ SE s
    _           → fail ".filter/create: Malformed JSON, cannot parse"


-- | <https://api.stackexchange.com/docs/read-filter>
readFilter ∷ MonadIO m ⇒ [Text] → StackExchangeT a m [SE Filter]
readFilter (intercalate ";" → filters) = localState $ do
  uriPath <>= ["filters", filters]
  AP.parse A.json `liftM` request >>= \case
    AP.Done _ s → map SE `liftM` (s ^! field "items")
    _           → fail ".filter/create: Malformed JSON, cannot parse"
