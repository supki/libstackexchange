{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module TH(string) where

import GHC.Exts(IsString(..))
import Language.Haskell.TH.Quote


string ∷ QuasiQuoter
string = QuasiQuoter
  ((\a → [|fromString a|]) . filter (/= '\r'))
  (error "Cannot use q as a pattern")
  (error "Cannot use q as a type")
  (error "Cannot use q as a dec")
