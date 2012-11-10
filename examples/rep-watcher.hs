#!/usr/bin/env runhaskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

import           Control.Lens
import qualified Data.Aeson.Lens as L

import Network.StackExchange


main ∷ IO ()
main = reputation <$> askSE req >>= mapM_ (print . truncate) . catMaybes
 where
  req = usersByIds [972985] <> site "stackoverflow" <> key "Lhg6xe5d5BvNK*C0S8jijA(("
  reputation xs = xs ^.. traverse . from se . to Just . L.key "reputation" . L.asDouble
