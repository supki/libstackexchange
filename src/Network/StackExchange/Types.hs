{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.StackExchange.Types where


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
  | TagTop
  | TagWiki
  | User
  | UserNetwork
  | UserShallow
  | UserTimeline
  | WritePermission


newtype SE (a ∷ Object) = SE { unSE ∷ Value } deriving Show
