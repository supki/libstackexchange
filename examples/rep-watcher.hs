#!/usr/bin/env runhaskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative ((<$>))
import Data.Monoid ((<>))

import Control.Lens

import Network.StackExchange


main ∷ IO ()
main = askSE req >>= mapM_ print
 where
  req = (\xs → xs ^.. traverse . int "reputation") <$>
    usersByIds [972985] <> site "stackoverflow" <> key "Lhg6xe5d5BvNK*C0S8jijA(("
