#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Lens
import Data.Monoid ((<>))
import System.Exit (exitFailure)

import Network.StackExchange


main ∷ IO ()
main = do
  askSE (badgesOnUsers [972985] <> site "stackoverflow") >>= \case
    Right xs → do
      let ranks = xs ^.. traverse . to unSE . field "rank" ∷ [String]
          badgesCount = \τ → length . Prelude.filter (== τ)
          golds = badgesCount "gold" ranks
          silvers = badgesCount "silver" ranks
          bronzes = badgesCount "bronze" ranks
      putStrLn $ show golds ++ " " ++ show silvers ++ " " ++ show bronzes
    _ → putStrLn "libse: FAIL" >> exitFailure
