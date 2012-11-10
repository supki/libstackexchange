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
    -- * Iso lens
  , se
  ) where

import Control.Applicative ((<$>))
import Control.Exception (Exception, throwIO)
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)

import           Control.Lens
import           Data.Aeson (Value)
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.Default (Default(..))
import qualified Data.Map as M
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


-- | Isomorphism lens for the ease of interaction with generic aeson parser lenses
se ∷ (Functor f, Isomorphic k) ⇒ k (SE a → f (SE a)) (Value → f Value)
se = iso SE unSE
