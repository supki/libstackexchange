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
  askSE (usersByIds [972985] <> site "stackoverflow" <> key "Lhg6xe5d5BvNK*C0S8jijA((") >>= \case
    Right xs → do
      let reps = xs ^.. traverse . to unSE . field "reputation" ∷ [Int]
      reps ^! traverse . act print
    _ → putStrLn "libse: FAIL" >> exitFailure
