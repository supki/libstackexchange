#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Data.Monoid ((<>))

import Control.Lens

import Network.StackExchange


main ∷ IO ()
main =
  askSE (usersByIds [972985] <> site "stackoverflow" <> key "Lhg6xe5d5BvNK*C0S8jijA((") >>= \xs →
    (xs ^.. traverse . to unSE . field "reputation") ^! traverse . act (print ∷ Int → IO ())
