{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Get response from Request and retrieve data from it
module Network.StackExchange.Response
  ( -- * Schedule request
    SEException(..), askSE
    -- * Iso lens
  , se
  ) where

import Control.Applicative ((<$>))
import Control.Exception (Exception, throwIO)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

import           Data.Profunctor
import           Data.Aeson (Value(..))
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.Default (Default(..))
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
askSE ∷ Request 'Ready n r → IO r
askSE q = do
  let R {_method, _parse} = unwrap q def
  m ← C.newManager C.tlsManagerSettings
  url ← C.parseUrlThrow (render q)
  r ← C.responseBody <$> C.httpLbs (url {C.method = toStrict $ encodeUtf8 _method}) m
  case _parse of
    Just f → return $ f r
    Nothing → throwIO $
      SEException r "libstackexchange.askSE: no parsing function registered"


-- | Isomorphism for the ease of interaction with aeson-lens
se :: (Functor f, Profunctor p) => p (SE a) (f (SE t)) -> p (Maybe Value) (f (Maybe Value))
se = dimap sa (fmap bt)
 where
  sa = SE . fromMaybe Null

  bt (SE Null) = Nothing
  bt (SE x) = Just x
