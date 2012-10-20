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
  askSE (usersByIds [972985] <> site "stackoverflow") >>= \case
    Right xs → do
      let reps = xs ^.. traverse . to unSE . field "reputation" ∷ [Int]
      reps ^! traverse . act print
    _ → putStrLn "libse: FAIL" >> exitFailure
