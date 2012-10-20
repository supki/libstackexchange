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
  , path, parse, query, site, filter
    -- * Schedule request
  , askSE
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


data Auth = Yes | No


data Request (a ∷ Auth) (i ∷ Nat) r = Request
  { _host ∷ Text
  , _path ∷ Text
  , _query ∷ Map Text Text
  , _parse ∷ Maybe (ByteString → Maybe r)
  }


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


-- | default StackExchange API request
instance Default (Request a i r) where
  def = mempty {_host = "https://api.stackexchange.com/2.1"}


path ∷ Text → Request a i r
path p = mempty {_path = p}


parse ∷ (ByteString → Maybe r) → Request a i r
parse f = mempty {_parse = Just f}


query ∷ [(Text, Text)] → Request a i r
query q = mempty {_query = M.fromList q}


site ∷ Text → Request a i r
site s = mempty {_query = M.singleton "site" s}


filter ∷ Text → Request a i r
filter f = mempty {_query = M.singleton "filter" f}


-- | To use Request in http-conduit we need to convert it to a string
render ∷ Request a i r → String
render r = T.unpack . mconcat $ [_host r, "/", _path r, "?", argie $ _query r]
 where
  argie = T.intercalate "&" . M.foldrWithKey (\k v m → T.concat [k, "=", v] : m) mempty


askSE ∷ Request a i r → IO (Either ByteString r)
askSE q = do
  let q' = def <> q
  putStrLn $ render q'
  r ← simpleHttp $ render q'
  case _parse q' of
    Just f → case f r of
      Just r' → return (Right r')
      Nothing → return (Left r)
    Nothing → return (Left r)
{-# INLINE askSE #-}
