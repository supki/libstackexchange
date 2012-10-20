{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>), mempty)
import System.Exit (exitFailure, exitSuccess)

import           Network.StackExchange.Request
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Test.QuickCheck


instance Show (a → b) where
  show = const "<function>"


instance Eq (a → b) where
  (==) = const . const True


deriving instance Show (Request a i r)


deriving instance Eq (Request a i r)


instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary


instance (Ord k, Arbitrary k, Arbitrary v) ⇒ Arbitrary (Map k v) where
  arbitrary = M.fromList <$> arbitrary


instance Arbitrary (Request a i r) where
  arbitrary =
    Request <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> return Nothing


prop_right_id ∷ Request a i r → Bool
prop_right_id r = r <> mempty == r


prop_left_id ∷ Request a i r → Bool
prop_left_id r = mempty <> r == r


prop_associative ∷ Request a i r → Request a i r → Request a i r → Bool
prop_associative x y z = (x <> y) <> z == x <> (y <> z)


prop_idempotent ∷ Request a i r → Bool
prop_idempotent x = x <> x == x


main ∷ IO ()
main = (,,,) <$>
  (check prop_right_id) <*>
  (check prop_left_id) <*>
  (check prop_associative) <*>
  (check prop_idempotent) >>= \case
  (True,True,True,True) → exitSuccess
  _                     → exitFailure
 where
  check p = success <$> quickCheckWithResult (stdArgs {maxSuccess = 300}) p

  success (Success {}) = True
  success _ = False
