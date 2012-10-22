{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.StackExchange.Auth where

import Data.Monoid ((<>), mempty)

import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)

import Network.StackExchange.Request


askPermission ∷ Int → Text → Request a i r
askPermission c r = host "https://stackexchange.com/oauth" <> clientId c <> redirectURI r


clientId ∷ Int → Request a i r
clientId c = mempty {_query = M.singleton "client_id" (toLazyText $ decimal c)}


redirectURI ∷ Text → Request a i r
redirectURI r = mempty {_query = M.singleton "redirect_uri" r}


state ∷ Text → Request a i r
state s = mempty {_query = M.singleton "state" s}
