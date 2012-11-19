#!/usr/bin/env runhaskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Foldable (foldMap)
import Data.Maybe (fromMaybe)
import Data.Monoid

import           Control.Lens
import qualified Data.Aeson.Lens as L

import Network.StackExchange


main ∷ IO ()
main = ask >>= print . foldMap count


ask ∷ IO [SE Badge]
ask = askSE $ badgesOnUsers [972985] <> site "stackoverflow" <> key "Lhg6xe5d5BvNK*C0S8jijA(("


count ∷ SE Badge → (Sum Int, Sum Int, Sum Int)
count (view (from se) → x) = (\l → set l (x ^. L.key "award_count" . to (Sum . fromMaybe 0)) mempty) $
  case x ^. L.key "rank" . L.asText of
    Just "bronze" → _1
    Just "silver" → _2
    Just "gold"   → _3
    _             → error "badge rank isn't bronze/silver/gold"
