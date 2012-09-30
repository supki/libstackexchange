{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.StackExchange where

import qualified Data.ByteString.Lazy as B


data Object =
    AccessTokens
  | AccountMerge
  | Answer
  | Badge
  | Comment
  | Edit
  | Event
  | Error
  | Filter
  | Inbox
  | Info
  | NetworkUser
  | Notification
  | Post
  | Question
  | Priviledge
  | Reputation
  | ReputationHistory
  | Revision
  | Site
  | Tag
  | TagScore
  | TagSynonym
  | Timeline
  | TagTop
  | User


data Response (a ∷ Object) = Response B.ByteString
