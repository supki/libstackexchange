{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | StackExchange authentication helpers
module Network.StackExchange.Auth
  ( -- * Authentication related routines
    askPermission, accessToken
  ) where

import Control.Applicative ((<$>), (*>))
import Control.Exception (throw)
import Data.Monoid ((<>), mconcat)

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Attoparsec.Text.Lazy as P

import Network.StackExchange.Response
import Network.StackExchange.Request


-- | Construct URI at which user should approve app
askPermission ∷ Int → Text → Request a i r
askPermission c r = host "https://stackexchange.com/oauth" <> client c <> redirectURI r


-- | Request access_token from StackExchange
accessToken ∷ Int → Text → Text → Text → Request a i Text
accessToken c s c' r = mconcat
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
