#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Data.Monoid ((<>))
import System.Exit (exitFailure)

import Control.Lens

import Network.StackExchange


main ∷ IO ()
main = do
  askSE (badgesOnUsers [972985] <> site "stackoverflow" <> key "Lhg6xe5d5BvNK*C0S8jijA((") >>= \case
    Right xs → do
      let ranks = xs ^.. traverse . to unSE . field "rank" ∷ [String]
          badgesCount = \τ → length . Prelude.filter (== τ)
          golds = badgesCount "gold" ranks
          silvers = badgesCount "silver" ranks
          bronzes = badgesCount "bronze" ranks
      putStrLn $ show golds ++ " " ++ show silvers ++ " " ++ show bronzes
    _ → putStrLn "libse: FAIL" >> exitFailure
