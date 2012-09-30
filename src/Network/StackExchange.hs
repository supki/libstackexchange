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
import Data.List (intercalate)
import Data.Typeable (Typeable)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.Lazy as AP
import qualified Data.ByteString.Lazy as B
import           Network.HTTP.Conduit


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


data SE (a ∷ Object) = SE A.Value deriving Show


newtype SEException = SEException String
  deriving (Show, Typeable)


instance Exception SEException


users'ids'answers ∷ [Int] → IO [SE Answer]
users'ids'answers (intercalate ";" . map show → ids) =
  AP.parse A.json <$> simpleHttp
    (stackexchange ++ "users/" ++ ids ++ "/answers?order=desc&sort=activity&site=stackoverflow") >>= \case
    AP.Done _ s → case A.parse p s of
      A.Success v → return $ map SE v
      _ → throwSE ".users/{ids}/answers: Incorrect JSON, cannot as a list of answers"
    _ → throwSE ".users/{ids}/answers: Malformed JSON, cannot parse"
 where
  p (A.Object o) = o A..: "items"
  p _ = empty


stackexchange ∷ String
stackexchange = "https://api.stackexchange.com/2.1/"


throwSE ∷ String → IO a
throwSE = throwIO . SEException . ("libstackexchange" ++)
