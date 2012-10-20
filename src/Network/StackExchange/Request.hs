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
  , path, parse, query, site
    -- * Schedule request
  , askSE
  ) where

import Data.Foldable (foldMap)
import Data.Monoid (Monoid(..), (<>))
import GHC.TypeLits

import           Data.ByteString.Lazy (ByteString)
import           Data.Default (Default(..))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Network.HTTP.Conduit (simpleHttp)


data Auth = Yes | No


data Request (a ∷ Auth) (i ∷ Nat) r = Request
  { _host ∷ Text
  , _path ∷ Text
  , _filter ∷ Text
  , _site ∷ Text
  , _query ∷ [(Text, Text)]
  , _parse ∷ Maybe (ByteString → Maybe r)
  }


instance Monoid (Request a i r) where
  mempty = Request
    { _host = mempty
    , _path = mempty
    , _site = mempty
    , _filter = mempty
    , _query = mempty
    , _parse = Nothing
    }
  l `mappend` r = Request
    { _host = _host $ if T.null $ _host r then l else r
    , _path = _path $ if T.null $ _path r then l else r
    , _site = _site $ if T.null $ _site r then l else r
    , _filter = _filter $ if T.null $ _filter r then l else r
    , _query = _query l <> _query r
    , _parse = _parse $ case _parse r of Just _ → r; Nothing → l
    }


-- | default StackExchange API request
instance Default (Request a i r) where
  def = Request
    { _host = "https://api.stackexchange.com/2.1"
    , _path = ""
    , _filter = ""
    , _site = ""
    , _query = []
    , _parse = Nothing
    }


path ∷ Text → Request a i r
path p = mempty {_path = p}


parse ∷ (ByteString → Maybe r) → Request a i r
parse f = mempty {_parse = Just f}


query ∷ [(Text, Text)] → Request a i r
query q = mempty {_query = q}


site ∷ Text → Request a i r
site s = mempty {_site = s}


-- | To use Request in http-conduit we need to convert it to a string
render ∷ Request a i r → String
render r = T.unpack . mconcat $ [_host r, "/", _path r, "?site=", _site r, argie $ _query r]
 where
  argie = foldMap (\(a, b) → mconcat ["&", a, "=", b])


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
