{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.StackExchange.Auth
  ( -- * Authentication related routines
    askPermission, accessToken
    -- * Authentication customization
  , state, Scope(..), scope
  ) where

import Control.Applicative ((<$>), (*>))
import Control.Exception (throw)
import Data.Monoid ((<>), mconcat)
import Prelude hiding (id)

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Attoparsec.Text.Lazy as P

import Network.StackExchange.Response
import Network.StackExchange.Request


askPermission ∷ Int → Text → Request a i r
askPermission c r = host "https://stackexchange.com/oauth" <> id c <> redirectURI r


accessToken ∷ Int → Text → Text → Text → Request a i Text
accessToken c s c' r = mconcat
  [ host "https://stackexchange.com/oauth/access_token"
  , id c
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
