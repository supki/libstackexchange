{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.StackExchange.Types where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

import qualified Data.Aeson.Types as A


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
