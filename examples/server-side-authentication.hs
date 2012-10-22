{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Simple StackExchange server-side authentication example
-- It consists of two steps.
--
-- * Firstly, after you ran the application, you go to http://appaddress:8000/authenticate-me-please
--   It will redirect you to StackExchange website, asking for confirmation. When you confirm
--   apps access, you will be redirected to @rURI@, it should be http://appaddress:8000/save-token
-- * Secondly, you can check what access token did you receive by asking http://appaddress:8000/show-tokens
--
-- That's it.
module Main where

import Control.Monad
import Data.IORef

import Control.Monad.Trans (liftIO)
import Happstack.Server hiding (host, path)

import Network.StackExchange


main ∷ IO ()
main = do
  tokens ← newIORef []
  simpleHTTP nullConf $ msum
    [ dir "authenticate-me-please" $ seeOther (render $ askPermission cID rURI) ""
    , dir "save-token" $ do
        c ← lookText "code"
        liftIO $ askSE (accessToken cID cSecret c rURI) >>= \(Right t) → modifyIORef' tokens (t:)
        ok "Saved."
    , dir "show-tokens" $ liftIO (readIORef tokens) >>= ok . show
    ]
 where
  rURI = ###

  cID = ###

  cSecret = ###
