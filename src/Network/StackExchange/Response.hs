{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- | Get response from Request and retrieve data from it
module Network.StackExchange.Response
  ( -- * Schedule request
    SEException(..), askSE, render
    -- * Generalized combinator
  , aeson
    -- * Convenience functions
  , field, fields
  ) where

import Control.Applicative ((<$>))
import Control.Exception (Exception, throwIO)
import Control.Category ((>>>))
import Control.Monad ((<=<))
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)

import           Data.ByteString.Lazy (ByteString, toStrict)
import           Control.Lens
import           Data.Aeson (FromJSON, Value, (.:), parseJSON)
import qualified Data.Aeson.Types as A
import           Data.Default (Default(..))
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Network.HTTP.Conduit as C

import Network.StackExchange.Request


-- | StackExchange invalid response exception
data SEException = SEException
  { _data ∷ ByteString -- ^ Recieved data
  , _error ∷ String -- ^ Parser/libstackexchange errors
  } deriving (Show, Typeable)


instance Exception SEException


-- | Send Request and parse response
askSE ∷ Request Ready n r → IO r
askSE (mappend def → q@Request {_method, _parse}) = do
  r ← C.withManager $ \m → C.parseUrl (render q) >>= \url →
    C.responseBody <$> C.httpLbs (url {C.method = toStrict $ encodeUtf8 _method}) m
  case _parse of
    Just f → return $ f r
    Nothing → throwIO $
      SEException r "libstackexchange.askSE: no parsing function registered"


-- | Render Request as string for networking
render ∷ Request a n r → String
render Request {_host, _path, _query} = T.unpack $ mconcat [_host, "/", _path, "?", argie _query]
 where
  argie = T.intercalate "&" . M.foldrWithKey (\k v m → T.concat [k, "=", v] : m) mempty


-- | Generalized combinator, useful if full power of Aeson is needed
aeson ∷ Monad m ⇒ (a → A.Parser b) → Action m a b
aeson p = act $ A.parse p >>> \case
  A.Success v → return v
  A.Error g → fail g
{-# INLINE aeson #-}


-- | Select specific field in JSON
field ∷ (Monad m, FromJSON a) ⇒ Text → Action m (SE x) a
field xs = aeson ((.: xs) <=< parseJSON . unSE)
{-# INLINE field #-}


-- | Select specific fields of an array in JSON
fields ∷ (Monad m, FromJSON a) ⇒ Text → Action m Value [a]
fields xs = aeson $ mapM (.: xs) <=< parseJSON
{-# INLINE fields #-}
