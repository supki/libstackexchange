{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.StackExchange.Types where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (Value)


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
  | Privilege
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
  | TagWiki
  | TopTag
  | User
  | UserNetwork
  | UserShallow
  | UserTimeline
  | WritePermission


newtype SE (a ∷ Object) = SE { unSE ∷ Value } deriving (Show, FromJSON)
