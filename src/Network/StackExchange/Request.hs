{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | StackExchange API request manipulation routines.
module Network.StackExchange.Request
  ( -- * Type
    Request(..), Auth(..)
    -- * Constructing requests
  , host, path, method, parse
  , query, token, key, site, filter, state, Scope(..), scope
  , id, redirectURI, secret, code
  ) where

import Data.Monoid (Monoid(..), (<>))
import GHC.TypeLits
import Prelude hiding (filter, id)

import           Data.ByteString.Lazy (ByteString)
import           Data.Default (Default(..))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)


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
  , _method ∷ Text
  , _query ∷ Map Text Text -- ^ API call query parameters
  , _parse ∷ Maybe (ByteString → r) -- ^ API call result parsing function
  }


-- | Subject to monoid and idempotent laws, they all are checked in request test suite
instance Monoid (Request a i r) where
  mempty = Request
    { _host = mempty
    , _path = mempty
    , _method = mempty
    , _query = mempty
    , _parse = Nothing
    }
  l `mappend` r = Request
    { _host = _host $ if T.null $ _host r then l else r
    , _path = _path $ if T.null $ _path r then l else r
    , _method = _method $ if T.null $ _method r then l else r
    , _query = _query r <> _query l
    , _parse = _parse $ case _parse r of Just _ → r; Nothing → l
    }


-- | Default StackExchange API request, defines only host link
instance Default (Request a i r) where
  def = mempty {_host = "https://api.stackexchange.com/2.1", _method = "GET"}
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


-- | Request defining only call method
--
-- Primarily used in API call wrappers, not intended for usage by library user
method ∷ Text → Request a i r
method m = mempty {_method = m}
{-# INLINE method #-}


-- | Request defining only API call result parsing function
--
-- Primarily used in API call wrappers, not intended for usage by library user
parse ∷ (ByteString → r) → Request a i r
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


-- | Request defining only API call access token
--
-- Primarily used in API call wrappers, not intended for usage by library user
token ∷ Text → Request Yes i r
token t = mempty {_query = M.singleton "access_token" t}
{-# INLINE token #-}


-- | Request defining only App key
key ∷ Text → Request a i r
key s = mempty {_query = M.singleton "key" s}
{-# INLINE key #-}


-- | Request defining only API call site query parameter
site ∷ Text → Request a i r
site s = mempty {_query = M.singleton "site" s}
{-# INLINE site #-}


-- | Request defining only API call filter query parameter
filter ∷ Text → Request a i r
filter f = mempty {_query = M.singleton "filter" f}
{-# INLINE filter #-}


-- | Request defining only API call state query parameter
state ∷ Text → Request a i r
state s = mempty {_query = M.singleton "state" s}
{-# INLINE state #-}


-- | Scope defines permission granted for application by user
data Scope = ReadInbox | NoExpiry | WriteAccess | PrivateInfo


-- | Request defining only API call scope query parameter
scope ∷ [Scope] → Request a i r
scope ss = mempty {_query = M.singleton "scope" $ scopie ss}
 where
  scopie xs = T.intercalate " " . flip map xs $ \case
    ReadInbox   → "read_inbox"
    NoExpiry    → "no_expiry"
    WriteAccess → "write_access"
    PrivateInfo → "private_info"


-- | Request defining only Authentication API call application id
--
-- Primarily used in Authentication API call wrappers, not intended for usage by library user
id ∷ Int → Request a i r
id c = mempty {_query = M.singleton "client_id" (toLazyText $ decimal c)}


-- | Request defining only Authentication API call redirect url
--
-- Primarily used in Authentication API call wrappers, not intended for usage by library user
redirectURI ∷ Text → Request a i r
redirectURI r = mempty {_query = M.singleton "redirect_uri" r}


-- | Request defining only Authentication API call application secret
--
-- Primarily used in Authentication API call wrappers, not intended for usage by library user
secret ∷ Text → Request a i r
secret c = mempty {_query = M.singleton "client_secret" c}


-- | Request defining only Authentication API call code
--
-- Primarily used in Authentication API call wrappers, not intended for usage by library user
code ∷ Text → Request a i r
code c = mempty {_query = M.singleton "code" c}
