{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.StackExchange.Auth
  ( -- * Authentication related routines
    askPermission, accessToken
    -- * Authentication customization
  , state
  ) where

import Control.Applicative ((<$>), (*>))
import Data.Monoid ((<>), mempty)
import Prelude hiding (id)

import qualified Data.Map as M
import           Data.ByteString.Lazy (ByteString)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)
import qualified Data.Attoparsec.Text.Lazy as P

import Network.StackExchange.Request


askPermission ∷ Int → Text → Request a i r
askPermission c r = host "https://stackexchange.com/oauth" <> id c <> redirectURI r


accessToken ∷ Int → Text → Text → Text → Request a i Text
accessToken c s c' r =
  host "https://stackexchange.com/oauth/access_token" <> id c <> secret s <> code c' <> redirectURI r <>
  parse parseToken <>
  method "POST"
 where
  parseToken ∷ ByteString → Either (ByteString, String) Text
  parseToken bs = case P.eitherResult $ P.parse parser (decodeUtf8 bs) of
    Right t → Right t
    Left e → Left (bs, show e)

  parser ∷ P.Parser Text
  parser = P.string "access_token=" *> (T.pack <$> P.manyTill P.anyChar (P.char '&'))


state ∷ Text → Request a i r
state s = mempty {_query = M.singleton "state" s}


id ∷ Int → Request a i r
id c = mempty {_query = M.singleton "client_id" (toLazyText $ decimal c)}


redirectURI ∷ Text → Request a i r
redirectURI r = mempty {_query = M.singleton "redirect_uri" r}


secret ∷ Text → Request a i r
secret c = mempty {_query = M.singleton "client_secret" c}


code ∷ Text → Request a i r
code c = mempty {_query = M.singleton "code" c}
