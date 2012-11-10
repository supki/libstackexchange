{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | StackExchange authentication helpers
module Network.StackExchange.Auth
  ( -- * Explicit OAuth 2.0 flow
    explicitUserPermission, explicitAccessToken
    -- * Implicit OAuth 2.0 flow
  , implicitUserPermission
  ) where

import Control.Applicative ((<$>), (*>))
import Control.Exception (throw)
import Data.Monoid ((<>), mconcat)

import           Data.Default (def)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Attoparsec.Text.Lazy as P

import Network.StackExchange.Response
import Network.StackExchange.Request


-- | Construct URI at which user should approve app
explicitUserPermission ∷ Int → Text → R a n r
explicitUserPermission c r =
  unwrap (userPermission c r <> host "https://stackexchange.com/oauth") def


-- | Request access_token from StackExchange
explicitAccessToken ∷ Int → Text → Text → Text → Request a n Text
explicitAccessToken c s c' r = mconcat
  [ host "https://stackexchange.com/oauth/access_token"
  , client c
  , secret s
  , code c'
  , redirectURI r
  , parse parseToken
  , method "POST"
  ]
 where
  parseToken bs = case P.eitherResult $ P.parse parser (decodeUtf8 bs) of
    Right t → t
    Left e → throw $ SEException bs ("libstackexchange.accessToken: " ++ show e)

  parser ∷ P.Parser Text
  parser = P.string "access_token=" *> (T.pack <$> P.manyTill P.anyChar (P.char '&'))


-- | Construct URI at which user should approve app
implicitUserPermission ∷ Int → Text → R a n r
implicitUserPermission c r =
  unwrap (userPermission c r <> host "https://stackexchange.com/oauth/dialog") def


userPermission ∷ Int → Text → Request a n r
userPermission c r = client c <> redirectURI r
