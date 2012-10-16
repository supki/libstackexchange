{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Some lenses for convenient JSON parsing
module Network.StackExchange.JSON
  ( -- * Generalized combinators
    aeson
    -- * Convenience functions
  , field, fields
  ) where

import Control.Category ((>>>))
import Control.Monad ((<=<))

import           Control.Lens
import           Data.Aeson (FromJSON, Value, (.:), parseJSON)
import qualified Data.Aeson.Types as A
import           Data.Text (Text)


-- | Generalized combinator, useful if user wants full power of Aeson
aeson ∷ Monad m ⇒ (a → A.Parser b) → Action m a b
aeson p = act $ A.parse p >>> \case
  A.Success v → return v
  A.Error g → fail g
{-# INLINE aeson #-}


-- | Select specific field in JSON
field ∷ (Monad m, FromJSON a) ⇒ Text → Action m Value a
field xs = aeson $ (.: xs) <=< parseJSON
{-# INLINE field #-}


-- | Select specific fields in an array in JSON
fields ∷ (Monad m, FromJSON a) ⇒ Text → Action m Value [a]
fields xs = aeson $ mapM (.: xs) <=< parseJSON
{-# INLINE fields #-}
