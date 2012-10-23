#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Data.List (intercalate)
import Data.Monoid ((<>))

import Control.Lens

import Network.StackExchange hiding (filter)


main ∷ IO ()
main =
  askSE (badgesOnUsers [972985] <> site "stackoverflow" <> key "Lhg6xe5d5BvNK*C0S8jijA((") >>= \xs →
    (xs ^.. traverse . to unSE . field "rank") ^! act (putStrLn . intercalate " " . sequence (map f ["gold", "silver", "bronze"]))
 where
  f ∷ String → [String] → String
  f t = show . length . filter (== t)
