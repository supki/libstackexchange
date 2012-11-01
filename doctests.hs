{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Test.DocTest


main ∷ IO ()
main = doctest ["-isrc", "-XOverloadedStrings", "src/Network/StackExchange/API.hs"]
