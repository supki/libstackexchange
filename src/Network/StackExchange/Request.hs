{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | StackExchange API server URI manipulation routines.
module Network.StackExchange.Request
  ( -- * Types
    Request(..), Auth(..)
    -- * Construct request
  , host, path, parse, query, site, filter, key
    -- * Schedule request
  , render, askSE
  ) where

import Data.Monoid (Monoid(..), (<>))
import GHC.TypeLits
import Prelude hiding (filter)

import           Data.ByteString.Lazy (ByteString)
import           Data.Default (Default(..))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Network.HTTP.Conduit (simpleHttp)


-- | Whether to use authentication at all. Currently isn't used
data Auth = Yes | No


-- | StackExchange API Request data type.
--
-- @a@ is a phantom type showing whether authentication is enabled
--
-- @i@ is a phantom type dissallowing combination of
-- different API calls in one request
--
-- @r@ is a type of parsed API call result
data Request (a ∷ Auth) (i ∷ Nat) r = Request
  { _host ∷ Text -- ^ API host link
  , _path ∷ Text -- ^ API call link
  , _query ∷ Map Text Text -- ^ API call query parameters
  , _parse ∷ Maybe (ByteString → Either (ByteString, String) r) -- ^ API call result parsing function
  }


-- | Subject to monoid and idempotent laws, they all are checked in request test suite
instance Monoid (Request a i r) where
  mempty = Request
    { _host = mempty
    , _path = mempty
    , _query = mempty
    , _parse = Nothing
    }
  l `mappend` r = Request
    { _host = _host $ if T.null $ _host r then l else r
    , _path = _path $ if T.null $ _path r then l else r
    , _query = _query r <> _query l
    , _parse = _parse $ case _parse r of Just _ → r; Nothing → l
    }


-- | Default StackExchange API request, defines only host link
instance Default (Request a i r) where
  def = mempty {_host = "https://api.stackexchange.com/2.1"}
  {-# INLINE def #-}


-- | Request defining only API call host
--
-- Primarily used in Auth, not intended for usage by library user
host ∷ Text → Request a i r
host p = mempty {_host = p}
{-# INLINE host #-}


-- | Request defining only API call path
--
-- Primarily used in API call wrappers, not intended for usage by library user
path ∷ Text → Request a i r
path p = mempty {_path = p}
{-# INLINE path #-}


-- | Request defining only API call result parsing function
--
-- Primarily used in API call wrappers, not intended for usage by library user
parse ∷ (ByteString → Either (ByteString, String) r) → Request a i r
parse f = mempty {_parse = Just f}
{-# INLINE parse #-}


-- | Request defining only API call query parameters
--
-- Rather low level interface. For more specific usage 'site',
-- 'filter', etc calls may be more convenient
--
--
-- Takes a list of (key, value) parameters such as @[("order", "asc"), ("sort", "rank")]@
query ∷ [(Text, Text)] → Request a i r
query q = mempty {_query = M.fromList q}
{-# INLINE query #-}


-- | Request defining only API call site query parameter
site ∷ Text → Request a i r
site s = mempty {_query = M.singleton "site" s}
{-# INLINE site #-}


-- | Request defining only API call filter query parameter
filter ∷ Text → Request a i r
filter f = mempty {_query = M.singleton "filter" f}
{-# INLINE filter #-}


-- | Request defining only App key
key ∷ Text → Request a i r
key s = mempty {_query = M.singleton "key" s}
{-# INLINE key #-}


askSE ∷ Request a i r → IO (Either (ByteString, String) r)
askSE q = do
  let q' = def <> q
  r ← simpleHttp $ render q'
  return $ maybe (Left (r, "libstackexchange.askSE: no parsing function registered")) ($ r) (_parse q')


-- | Render Request as string for networking
render ∷ Request a i r → String
render r = T.unpack . mconcat $ [_host r, "/", _path r, "?", argie $ _query r]
 where
  argie = T.intercalate "&" . M.foldrWithKey (\k v m → T.concat [k, "=", v] : m) mempty
