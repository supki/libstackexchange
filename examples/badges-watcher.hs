#!/usr/bin/env runhaskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Data.Monoid

import Control.Lens

import Network.StackExchange hiding (filter)


main ∷ IO ()
main =
  askSE (badgesOnUsers [972985] <> site "stackoverflow" <> key "Lhg6xe5d5BvNK*C0S8jijA((") >>= \xs →
    (xs ^.. traverse . to f) ^! to mconcat . act print
 where
  f ∷ SE Badge → (Sum Int, Sum Int, Sum Int)
  f (SE x) = (\l → set l (x ^. field "award_count" . to Sum) mempty) $
    case x ^. field "rank" ∷ String of
      "bronze" → _1
      "silver" → _2
      "gold"   → _3
      _        → error "badge rank isn't bronze/silver/gold"
