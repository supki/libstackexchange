#!/usr/bin/env runhaskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Data.Foldable (foldMap)
import Data.Monoid

import Control.Lens

import Network.StackExchange


main ∷ IO ()
main = ask >>= print . foldMap count


ask ∷ IO [SE Badge]
ask = askSE $ badgesOnUsers [972985] <> site "stackoverflow" <> key "Lhg6xe5d5BvNK*C0S8jijA(("


count ∷ SE Badge → (Sum Int, Sum Int, Sum Int)
count (SE x) = (\l → set l (x ^. field "award_count" . to Sum) mempty) $
  case x ^. field "rank" ∷ String of
    "bronze" → _1
    "silver" → _2
    "gold"   → _3
    _        → error "badge rank isn't bronze/silver/gold"
