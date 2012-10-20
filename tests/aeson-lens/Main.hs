{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import System.Exit (exitFailure, exitSuccess)

import           Data.ByteString.Lazy (ByteString)
import           Control.Lens
import qualified Data.Aeson as A
import           Network.StackExchange.JSON
import qualified Data.Attoparsec.Lazy as AP
import           Test.HUnit

import TH (string)


main ∷ IO ()
main = do
  let AP.Done _ parsed = AP.parse A.json json
  Counts { errors, failures } ← runTestTT (tests parsed)
  if errors + failures == 0 then exitSuccess else exitFailure


json ∷ ByteString
json = [string|
  { "apples": 4
  , "bananas":
    { "bananas": 7
    }
  , "pinapples": [4, 7]
  , "peaches":
    [ { "peaches": [4, 7] }
    , { "peaches": [4, 7] }
    ]
  }
|]


tests ∷ A.Value → Test
tests = TestList . sequence
  [ testKeyLookup
  , testNestedKeyLookup
  , testFailedKeyLookup
  , testKeysLookup
  , testNestedKeysLookup
  ]
 where
  testKeyLookup parsed = TestCase . assertEqual "key lookup" (Just 4) $
    (parsed ^! field "apples" ∷ Maybe Int)
  testNestedKeyLookup parsed = TestCase . assertEqual "nested key lookup" (Just 7) $
    (parsed ^! field "bananas" . field "bananas" ∷ Maybe Int)
  testFailedKeyLookup parsed = TestCase . assertEqual "failed key lookup" Nothing $
    (parsed ^! field "oranges" ∷ Maybe Int)
  testKeysLookup parsed = TestCase . assertEqual "keys lookup" (Just [4,7]) $
    (parsed ^! field "pinapples" ∷ Maybe [Int])
  testNestedKeysLookup parsed = TestCase . assertEqual "nested keys lookup" (Just [4,7,4,7]) $
    (parsed ^! field "peaches" . fields "peaches" . traverse ∷ Maybe [Int])
