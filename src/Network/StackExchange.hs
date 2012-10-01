{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module Network.StackExchange where

import Control.Applicative ((<$>), empty)
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)

import           Data.Monoid.Lens ((<>~))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.Lazy as AP
import           Data.Text.Lazy (intercalate)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)
import           Network.HTTP.Conduit (simpleHttp)

import Network.StackExchange.URI


data Object =
    AccessTokens
  | AccountMerge
  | Answer
  | Badge
  | BadgeCount
  | Comment
  | Error
  | Event
  | Filter
  | InboxItem
  | Info
  | MigrationInfo
  | Notice
  | Notification
  | Post
  | Priviledge
  | Question
  | QuestionTimeline
  | RelatedSite
  | Reputation
  | ReputationHistory
  | Revision
  | Site
  | Styling
  | SuggestedEdit
  | Tag
  | TagScore
  | TagSynonym
  | TagTop
  | TagWiki
  | User
  | UserNetwork
  | UserShallow
  | UserTimeline
  | WritePermission


newtype SE (a ∷ Object) = SE A.Value deriving Show


newtype SEException = SEException String
  deriving (Show, Typeable)


instance Exception SEException


users'ids'answers ∷ [Int] → IO [SE Answer]
users'ids'answers (intercalate ";" . map (toLazyText . decimal) → ids) = do
  let path = uriPath <>~ ["users", ids, "answers"]
      query = uriQuery <>~ [("order","desc"),("sort","activity"),("site","stackoverflow")]
  AP.parse A.json <$> (simpleHttp . render . query . path $ stackexchange) >>= \case
    AP.Done _ s → case A.parse p s of
      A.Success v → return $ map SE v
      _ → throwSE ".users/{ids}/answers: Incorrect JSON, cannot parse as a list of answers"
    _ → throwSE ".users/{ids}/answers: Malformed JSON, cannot parse"
 where
  p (A.Object o) = o A..: "items"
  p _ = empty


throwSE ∷ String → IO a
throwSE = throwIO . SEException . ("libstackexchange" ++)
