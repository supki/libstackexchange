{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- | API methods list
module Network.StackExchange.API
  ( -- * SE Answer
    answers, answersByIds, answersOnUsers
  , answersOnQuestions, meAnswers, topUserAnswersInTags, meTagsTopAnswers
    -- * SE Badge
  , badges, badgesByIds, badgeRecipientsByIds
  , badgesByName, badgeRecipients, badgesByTag
  , badgesOnUsers, meBadges
    -- * SE Comment
  , commentsOnAnswers, comments, commentsByIds, deleteComment, editComment
  , commentsOnPosts, createComment, commentsOnQuestions
  , commentsOnUsers, meComments, commentsByUsersToUser, meCommentsTo
  , mentionsOnUsers, meMentioned
    -- * SE Error
  , errors
    -- * SE Event
  , events
    -- * SE Filter
  , createFilter, readFilter
    -- * SE InboxItems
  , inbox, inboxUnread, userInbox, meInbox, userUnreadInbox, meUnreadInbox
    -- * SE Info
  , info
    -- * SE NetworkUser
  , associatedUsers, meAssociatedUsers
    -- * SE AccountMerge
  , mergeHistory, meMergeHistory
    -- * SE Notification
  , notifications, notificationsUnread, userNotifications, meNotifications
  , userUnreadNotifications, meUnreadNotifications
    -- * SE Post
  , posts, postsByIds
    -- * SE Privilege
  , privileges, privilegesOnUsers, mePriviledges
    -- * SE Question
  , questions, questionsByIds, linkedQuestions, relatedQuestions
  , featuredQuestions, unansweredQuestions, noAnswerQuestions
  , search, advancedSearch, similar, faqsByTags, favoritesOnUsers
  , meFavorites, questionsOnUsers, meQuestions, featuredQuestionsOnUsers
  , meFeaturedQuestions, noAnswerQuestionsOnUsers, meNoAnswerQuestions
  , unacceptedQuestionsOnUsers, meUnacceptedQuestions, unansweredQuestionsOnUsers
  , meUnansweredQuestions, topUserQuestionsInTags, meTagsTopQuestions
    -- * SE QuestionTimeline
  , questionsTimeline
    -- * SE Reputation
  , reputationOnUsers, meReputation
    -- * SE ReputationHistory
  , reputationHistory, reputationHistoryFull
  , meReputationHistory, meReputationHistoryFull
    -- * SE Revision
  , revisionsByIds, revisionsByGuids
    -- * SE Site
  , sites
    -- * SE SuggestedEdit
  , postsOnSuggestedEdits, suggestedEdits, suggestedEditsByIds
  , suggestedEditsOnUsers, meSuggestedEdits
    -- * SE Tag
  , tags, moderatorOnlyTags, requiredTags
  , tagsByName, relatedTags, tagsOnUsers, meTags
    -- * SE TagScore
  , topAnswerersOnTag, topAskersOnTag
    -- * SE TagSynonym
  , tagSynonyms, synonymsByTags
    -- * SE TagWiki
  , wikisByTags
    -- * SE TopTag
  , topAnswerTagsOnUsers, topQuestionTagsOnUsers
  , meTopAnswerTags, meTopQuestionTags
    -- * SE User
  , users, usersByIds, me, moderators, electedModerators
    -- * SE UserTimeline
  , timelineOnUsers, meTimeline
    -- * SE WritePermission
  , writePermissions, meWritePermissions
  ) where

import Data.Monoid ((<>))

import           Control.Exception (throw)
import           Control.Lens ((^!))
import           Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Lazy as AP
import           Data.ByteString.Lazy (ByteString)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)

import Network.StackExchange.Response
import Network.StackExchange.Request

--------------------------
-- Answers
--------------------------

-- | <https://api.stackexchange.com/docs/answers>
answers ∷ Request a __COUNTER__ [SE Answer]
answers = path "answers" <> parse (attoparsec items ".answers: ")


-- | <https://api.stackexchange.com/docs/answers-by-ids>
answersByIds ∷ [Int] → Request a __COUNTER__ [SE Answer]
answersByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("answers/" <> is) <> parse (attoparsec items ".answers/{ids}: ")


-- | <https://api.stackexchange.com/docs/answers-on-users>
answersOnUsers ∷ [Int] → Request a __COUNTER__ [SE Answer]
answersOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/answers") <>
  parse (attoparsec items ".users/{ids}/answers: ")


-- | <https://api.stackexchange.com/docs/answers-on-questions>
answersOnQuestions ∷ [Int] → Request a __COUNTER__ [SE Answer]
answersOnQuestions (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/answers") <>
  parse (attoparsec items ".questions/{ids}/answers: ")


-- | <https://api.stackexchange.com/docs/me-answers>
meAnswers ∷ Request RequireToken __COUNTER__ [SE Answer]
meAnswers =
  path ("me/answers") <> parse (attoparsec items ".me/answers: ")


-- | <https://api.stackexchange.com/docs/top-user-answers-in-tags>
topUserAnswersInTags ∷ Int → [Text] → Request a __COUNTER__ [SE Answer]
topUserAnswersInTags (toLazyText . decimal → i) (T.intercalate ";" → ts) =
  path ("users/" <> i <> "/tags/" <> ts <> "/top-answers") <>
  parse (attoparsec items ".users/{id}/tags/{tags}/top-answers: ")


-- | <https://api.stackexchange.com/docs/me-tags-top-answers>
meTagsTopAnswers ∷ [Text] → Request RequireToken __COUNTER__ [SE Answer]
meTagsTopAnswers (T.intercalate ";" → ts) =
  path ("me/tags/" <> ts <> "/top-answers") <>
  parse (attoparsec items ".me/tags/{tags}/top-answers: ")


--------------------------
-- Badges
--------------------------

-- | <https://api.stackexchange.com/docs/badges>
badges ∷ Request a __COUNTER__ [SE Badge]
badges = path "badges" <> parse (attoparsec items ".badges: ")


-- | <https://api.stackexchange.com/docs/badges-by-ids>
badgesByIds ∷ [Int] → Request a __COUNTER__ [SE Badge]
badgesByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("badges/" <> is) <> parse (attoparsec items ".badges/{ids}: ")


-- | <https://api.stackexchange.com/docs/badge-recipients-by-ids>
badgeRecipientsByIds ∷ [Int] → Request a __COUNTER__ [SE Badge]
badgeRecipientsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("badges" <> is <> "/recipients") <>
  parse (attoparsec items ".badges/{ids}/recipients: ")


-- | <https://api.stackexchange.com/docs/badges-by-name>
badgesByName ∷ Request a __COUNTER__ [SE Badge]
badgesByName =
  path ("badges" <> "/name") <> parse (attoparsec items ".badges/name: ")


-- | <https://api.stackexchange.com/docs/badge-recipients>
badgeRecipients ∷ Request a __COUNTER__ [SE Badge]
badgeRecipients =
  path ("badges" <> "/recipients") <>
  parse (attoparsec items ".badges/recipients: ")


-- | <https://api.stackexchange.com/docs/badges-by-tag>
badgesByTag ∷ Request a __COUNTER__ [SE Badge]
badgesByTag =
  path ("badges" <> "/tags") <> parse (attoparsec items ".badges/tags: ")


-- | <https://api.stackexchange.com/docs/badges-on-users>
badgesOnUsers ∷ [Int] → Request a __COUNTER__ [SE Badge]
badgesOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/badges") <>
  parse (attoparsec items ".users/{ids}/badges: ")


-- | <https://api.stackexchange.com/docs/me-badges>
meBadges ∷ Request RequireToken __COUNTER__ [SE Badge]
meBadges = path "me/badges" <> parse (attoparsec items ".me/badges: ")



--------------------------
-- Comments
--------------------------

-- | <https://api.stackexchange.com/docs/comments-on-answers>
commentsOnAnswers ∷ [Int] → Request a __COUNTER__ [SE Comment]
commentsOnAnswers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("answers/" <> is <> "/comments") <>
  parse (attoparsec items ".answers/{ids}/comments: ")


-- | <https://api.stackexchange.com/docs/comments>
comments ∷ Request a __COUNTER__ [SE Comment]
comments = path "comments" <> parse (attoparsec items ".comments: ")


-- | <https://api.stackexchange.com/docs/delete-comment>
deleteComment ∷ Int → Request RequireToken __COUNTER__ [SE Comment]
deleteComment ((toLazyText . decimal) → i) =
  path ("comments/" <> i <> "/delete") <>
  parse (attoparsec items ".comments/{id}/delete:")


-- | <https://api.stackexchange.com/docs/edit-comment>
editComment ∷ Int → Request RequireToken __COUNTER__ [SE Comment]
editComment ((toLazyText . decimal) → i) =
  path ("comments/" <> i <> "/edit") <>
  parse (attoparsec items ".comments/{id}/edit:")


-- | <https://api.stackexchange.com/docs/comments-by-ids>
commentsByIds ∷ [Int] → Request a __COUNTER__ [SE Comment]
commentsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("comments/" <> is) <> parse (attoparsec items ".comments/{ids}: ")


-- | <https://api.stackexchange.com/docs/comments-on-posts>
commentsOnPosts ∷ [Int] → Request a __COUNTER__ [SE Comment]
commentsOnPosts (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("posts/" <> is <> "/comments") <>
  parse (attoparsec items ".posts/{ids}/comments: ")


-- | <https://api.stackexchange.com/docs/create-comment>
createComment ∷ Int → Request RequireToken __COUNTER__ [SE Comment]
createComment ((toLazyText . decimal) → i) =
  path ("posts/" <> i <> "/comments/add") <>
  parse (attoparsec items ".posts/{id}/comments/add:")


-- | <https://api.stackexchange.com/docs/comments-on-questions>
commentsOnQuestions ∷ [Int] → Request a __COUNTER__ [SE Comment]
commentsOnQuestions (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/comments") <>
  parse (attoparsec items ".questions/{ids}/comments: ")


-- | <https://api.stackexchange.com/docs/comments-on-users>
commentsOnUsers ∷ [Int] → Request a __COUNTER__ [SE Comment]
commentsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/comments") <>
  parse (attoparsec items ".users/{ids}/comments: ")


-- | <https://api.stackexchange.com/docs/me-comments>
meComments ∷ Request RequireToken __COUNTER__ [SE Comment]
meComments = path "me/comments" <> parse (attoparsec items ".me/comments: ")


-- | <https://api.stackexchange.com/docs/comments-by-users-to-user>
commentsByUsersToUser ∷ [Int] → Int → Request a __COUNTER__ [SE Comment]
commentsByUsersToUser (T.intercalate ";" . map (toLazyText . decimal) → is)
                      ((toLazyText . decimal) → toid) =
  path ("users/" <> is <> "/comments/" <> toid) <>
  parse (attoparsec items ".users/{ids}/comments/{toid}: ")


-- | <https://api.stackexchange.com/docs/me-comments-to>
meCommentsTo ∷ Int → Request RequireToken __COUNTER__ [SE Comment]
meCommentsTo ((toLazyText . decimal) → toid) =
  path ("me/comments/" <> toid) <>
  parse (attoparsec items ".me/comments/{toid}:")


-- | <https://api.stackexchange.com/docs/mentions-on-users>
mentionsOnUsers ∷ [Int] → Request a __COUNTER__ [SE Comment]
mentionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/mentioned") <>
  parse (attoparsec items ".users/{ids}/mentioned: ")


-- | <https://api.stackexchange.com/docs/me-mentioned>
meMentioned ∷ Request RequireToken __COUNTER__ [SE Comment]
meMentioned = path "me/mentioned" <> parse (attoparsec items ".me/mentioned: ")


--------------------------
-- Errors
--------------------------

-- | <https://api.stackexchange.com/docs/errors>
errors ∷ Request a __COUNTER__ [SE Error]
errors = path "errors" <> parse (attoparsec items ".errors: ")


--------------------------
-- Events
--------------------------

-- | <https://api.stackexchange.com/docs/events>
events ∷ Request RequireToken __COUNTER__ [SE Event]
events = path "events" <> parse (attoparsec items ".events: ")


--------------------------
-- Filters
--------------------------

-- | <https://api.stackexchange.com/docs/create-filter>
createFilter ∷ [Text] → [Text] → Text → Request a __COUNTER__ (SE Filter)
createFilter (T.intercalate ";" → include) (T.intercalate ";" → exclude) base =
  path "filter/create" <>
  query [("include", include), ("exclude", exclude), ("base", base)] <>
  parse (attoparsec (return . SE) ".filter/create: ")


-- | <https://api.stackexchange.com/docs/read-filter>
readFilter ∷ [Text] → Request a __COUNTER__ [SE Filter]
readFilter (T.intercalate ";" → filters) =
  path "filters" <>
  query [("filters", filters)] <>
  parse (attoparsec items ".filters: ")


--------------------------
-- Inbox Items
--------------------------

-- | <https://api.stackexchange.com/docs/inbox>
inbox ∷ Request RequireToken __COUNTER__ [SE InboxItem]
inbox =
  path ("inbox") <>
  parse (attoparsec items ".inbox: ")


-- | <https://api.stackexchange.com/docs/inbox-unread>
inboxUnread ∷ Request RequireToken __COUNTER__ [SE InboxItem]
inboxUnread =
  path ("inbox/unread") <>
  parse (attoparsec items ".inbox/unread: ")


-- | <https://api.stackexchange.com/docs/user-inbox>
userInbox ∷ Int → Request RequireToken __COUNTER__ [SE InboxItem]
userInbox ((toLazyText . decimal) → i) =
  path ("users/" <> i <> "/inbox") <>
  parse (attoparsec items ".users/{id}/inbox: ")


-- | <https://api.stackexchange.com/docs/me-inbox>
meInbox ∷ Request RequireToken __COUNTER__ [SE InboxItem]
meInbox =
  path ("me/inbox") <>
  parse (attoparsec items ".me/inbox: ")


-- | <https://api.stackexchange.com/docs/user-unread-inbox>
userUnreadInbox ∷ Int → Request RequireToken __COUNTER__ [SE InboxItem]
userUnreadInbox ((toLazyText . decimal) → i) =
  path ("users/" <> i <> "/inbox/unread") <>
  parse (attoparsec items ".users/{id}/inbox/unread: ")


-- | <https://api.stackexchange.com/docs/me-unread-inbox>
meUnreadInbox ∷ Request RequireToken __COUNTER__ [SE InboxItem]
meUnreadInbox =
  path ("me/inbox/unread") <>
  parse (attoparsec items ".me/inbox/unread: ")


--------------------------
-- Info
--------------------------

-- | <https://api.stackexchange.com/docs/info>
info ∷ Request a __COUNTER__ (SE Info)
info = path "info" <> parse (attoparsec (return . SE) ".info: ")


--------------------------
-- Network Users
--------------------------

-- | <https://api.stackexchange.com/docs/associated-users>
associatedUsers ∷ [Int] → Request a __COUNTER__ [SE NetworkUser]
associatedUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/associated") <>
  parse (attoparsec items ".users/{ids}/associated: ")


-- | <https://api.stackexchange.com/docs/me-associated-users>
meAssociatedUsers ∷ Request RequireToken __COUNTER__ [SE NetworkUser]
meAssociatedUsers =
  path ("me/associated") <>
  parse (attoparsec items ".me/associated: ")


--------------------------
-- Merge History
--------------------------

-- | <https://api.stackexchange.com/docs/merge-history>
mergeHistory ∷ [Int] → Request a __COUNTER__ [SE AccountMerge]
mergeHistory (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/merges") <>
  parse (attoparsec items ".users/{ids}/merges: ")


-- | <https://api.stackexchange.com/docs/me-merge-history>
meMergeHistory ∷ Request RequireToken __COUNTER__ [SE AccountMerge]
meMergeHistory =
  path ("me/merges") <>
  parse (attoparsec items ".me/merges: ")


--------------------------
-- Notifications
--------------------------

-- | <https://api.stackexchange.com/docs/notifications>
notifications ∷ Request RequireToken __COUNTER__ [SE Notification]
notifications = path ("notifications") <> parse (attoparsec items ".notifications: ")

-- | <https://api.stackexchange.com/docs/notifications-unread>
notificationsUnread ∷ Request RequireToken __COUNTER__ [SE Notification]
notificationsUnread = path ("notifications/unread") <> parse (attoparsec items ".notifications/unread: ")


-- | <https://api.stackexchange.com/docs/user-notifications>
userNotifications ∷ Int → Request RequireToken __COUNTER__ [SE Notification]
userNotifications ((toLazyText . decimal) → i) =
  path ("users/" <> i <> "/notifications") <>
  parse (attoparsec items ".users/{id}/notifications: ")


-- | <https://api.stackexchange.com/docs/me-notifications>
meNotifications ∷ Request RequireToken __COUNTER__ [SE Notification]
meNotifications =
  path ("me/notifications") <>
  parse (attoparsec items ".me/notifications: ")


-- | <https://api.stackexchange.com/docs/user-unread-notifications>
userUnreadNotifications ∷ Int → Request RequireToken __COUNTER__ [SE Notification]
userUnreadNotifications ((toLazyText . decimal) → i) =
  path ("users/" <> i <> "/notifications/unread") <>
  parse (attoparsec items ".users/{id}/notifications/unread: ")


-- | <https://api.stackexchange.com/docs/me-unread-notifications>
meUnreadNotifications ∷ Request RequireToken __COUNTER__ [SE Notification]
meUnreadNotifications =
  path ("me/notifications") <>
  parse (attoparsec items ".me/notifications/unread: ")


--------------------------
-- Posts
--------------------------

-- | <https://api.stackexchange.com/docs/posts>
posts ∷ Request a __COUNTER__ [SE Post]
posts = path "posts" <> parse (attoparsec items ".posts: ")


-- | <https://api.stackexchange.com/docs/posts-by-ids>
postsByIds ∷ [Int] → Request a __COUNTER__ [SE Post]
postsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("posts/" <> is) <> parse (attoparsec items ".posts/{ids}: ")


--------------------------
-- Privileges
--------------------------

-- | <https://api.stackexchange.com/docs/privileges>
privileges ∷ Request a __COUNTER__ [SE Privilege]
privileges = path "privileges" <> parse (attoparsec items ".privileges: ")


-- | <https://api.stackexchange.com/docs/privileges-on-users>
privilegesOnUsers ∷ Int → Request a __COUNTER__ [SE Privilege]
privilegesOnUsers ((toLazyText . decimal) → i) =
  path ("users/" <> i <> "/privileges") <>
  parse (attoparsec items ".users/{ids}/privileges: ")


-- | <https://api.stackexchange.com/docs/me-privileges>
mePriviledges ∷ Request RequireToken __COUNTER__ [SE Privilege]
mePriviledges = path "me/privileges" <> parse (attoparsec items ".me/privileges: ")


--------------------------
-- Questions
--------------------------

-- | <https://api.stackexchange.com/docs/questions>
questions ∷ Request a __COUNTER__ [SE Question]
questions = path "questions" <> parse (attoparsec items ".questions: ")


-- | <https://api.stackexchange.com/docs/questions-by-ids>
questionsByIds ∷ [Int] → Request a __COUNTER__ [SE Question]
questionsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is) <> parse (attoparsec items ".questions/{ids}: ")


-- | <https://api.stackexchange.com/docs/linked-questions>
linkedQuestions ∷ [Int] → Request a __COUNTER__ [SE Question]
linkedQuestions (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/linked") <>
  parse (attoparsec items ".questions/{ids}/linked: ")


-- | <https://api.stackexchange.com/docs/related-questions>
relatedQuestions ∷ [Int] → Request a __COUNTER__ [SE Question]
relatedQuestions (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/related") <>
  parse (attoparsec items ".questions/{ids}/related: ")


-- | <https://api.stackexchange.com/docs/featured-questions>
featuredQuestions ∷ Request a __COUNTER__ [SE Question]
featuredQuestions =
  path "questions/featured" <> parse (attoparsec items ".questions/featured: ")


-- | <https://api.stackexchange.com/docs/unanswered-questions>
unansweredQuestions ∷ Request a __COUNTER__ [SE Question]
unansweredQuestions =
  path "questions/unanswered" <>
  parse (attoparsec items ".questions/unanswered: ")


-- | <https://api.stackexchange.com/docs/no-answer-questions>
noAnswerQuestions ∷ Request a __COUNTER__ [SE Question]
noAnswerQuestions =
  path "questions/no-answers" <>
  parse (attoparsec items ".questions/no-answers: ")


-- | <https://api.stackexchange.com/docs/search>
search ∷ Text → [Text] → Request a __COUNTER__ [SE Question]
search t (T.intercalate ";" → ts) =
  path "search" <>
  query [("intitle",t),("tagged",ts)] <>
  parse (attoparsec items ".search: ")


-- | <https://api.stackexchange.com/docs/advanced-search>
advancedSearch ∷ Request a __COUNTER__ [SE Question]
advancedSearch =
  path "search/advanced" <> parse (attoparsec items ".search/advanced: ")


-- | <https://api.stackexchange.com/docs/similar>
similar ∷ Text → [Text] → Request a __COUNTER__ [SE Question]
similar t (T.intercalate ";" → ts) =
  path "similar" <>
  query [("title",t),("tagged",ts)] <>
  parse (attoparsec items ".similar: ")


-- | <https://api.stackexchange.com/docs/faqs-by-tags>
faqsByTags ∷ [Text] → Request a __COUNTER__ [SE Question]
faqsByTags (T.intercalate ";" → ts) =
  path ("tags/" <> ts <> "/faq") <>
  parse (attoparsec items ".tags/{tags}/faq: ")


-- | <https://api.stackexchange.com/docs/favorites-on-users>
favoritesOnUsers ∷ [Int] → Request a __COUNTER__ [SE Question]
favoritesOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/favorites") <>
  parse (attoparsec items ".users/{ids}/favorites: ")


-- | <https://api.stackexchange.com/docs/me-favorites>
meFavorites ∷ Request RequireToken __COUNTER__ [SE Question]
meFavorites = path "me/favorites" <> parse (attoparsec items ".me/favorites: ")


-- | <https://api.stackexchange.com/docs/questions-on-users>
questionsOnUsers ∷ [Int] → Request a __COUNTER__ [SE Question]
questionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions") <>
  parse (attoparsec items ".users/{ids}/questions: ")


-- | <https://api.stackexchange.com/docs/me-questions>
meQuestions ∷ Request RequireToken __COUNTER__ [SE Question]
meQuestions = path "me/questions" <> parse (attoparsec items ".me/questions: ")


-- | <https://api.stackexchange.com/docs/featured-questions-on-users>
featuredQuestionsOnUsers ∷ [Int] → Request a __COUNTER__ [SE Question]
featuredQuestionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions/featured") <>
  parse (attoparsec items ".users/{ids}/questions/featured: ")


-- | <https://api.stackexchange.com/docs/me-featured-questions>
meFeaturedQuestions ∷ Request RequireToken __COUNTER__ [SE Question]
meFeaturedQuestions = path ("me/questions/featured") <> parse (attoparsec items ".me/questions/featured: ")


-- | <https://api.stackexchange.com/docs/no-answer-questions-on-users>
noAnswerQuestionsOnUsers ∷ [Int] → Request a __COUNTER__ [SE Question]
noAnswerQuestionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions/no-answers") <>
  parse (attoparsec items ".users/{ids}/questions/no-answers: ")


-- | <https://api.stackexchange.com/docs/me-no-answer-questions>
meNoAnswerQuestions ∷ Request RequireToken __COUNTER__ [SE Question]
meNoAnswerQuestions = path ("me/questions/no-answers") <> parse (attoparsec items ".me/questions/no-answers: ")


-- | <https://api.stackexchange.com/docs/unaccepted-questions-on-users>
unacceptedQuestionsOnUsers ∷ [Int] → Request a __COUNTER__ [SE Question]
unacceptedQuestionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions/unaccepted") <>
  parse (attoparsec items ".users/{ids}/questions/unaccepted: ")


-- | <https://api.stackexchange.com/docs/me-unaccepted-questions>
meUnacceptedQuestions ∷ Request RequireToken __COUNTER__ [SE Question]
meUnacceptedQuestions = path ("me/questions/unaccepted") <> parse (attoparsec items ".me/questions/unaccepted: ")


-- | <https://api.stackexchange.com/docs/unanswered-questions-on-users>
unansweredQuestionsOnUsers ∷ [Int] → Request a __COUNTER__ [SE Question]
unansweredQuestionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions/unanswered") <>
  parse (attoparsec items ".users/{ids}/questions/unanswered: ")


-- | <https://api.stackexchange.com/docs/me-unanswered-questions>
meUnansweredQuestions ∷ Request RequireToken __COUNTER__ [SE Question]
meUnansweredQuestions = path ("me/questions/unanswered") <> parse (attoparsec items ".me/questions/unanswered: ")


-- | <https://api.stackexchange.com/docs/top-user-questions-in-tags>
topUserQuestionsInTags ∷ Int → [Text] → Request a __COUNTER__ [SE Question]
topUserQuestionsInTags ((toLazyText . decimal) → i) (T.intercalate ";" → ts) =
  path ("users/" <> i <> "/tags/" <> ts <> "/top-questions") <>
    parse (attoparsec items ".users/{id}/tags/{tags}/top-questions: ")


-- | <https://api.stackexchange.com/docs/me-tags-top-questions>
meTagsTopQuestions ∷ [Text] → Request RequireToken __COUNTER__ [SE Question]
meTagsTopQuestions (T.intercalate ";" → ts) =
  path ("me/tags/" <> ts <> "/top-questions") <>
  parse (attoparsec items ".me/tags/{tags}/top-questions: ")


--------------------------
-- Question Timelines
--------------------------

-- | <https://api.stackexchange.com/docs/questions-timeline>
questionsTimeline ∷ [Int] → Request a __COUNTER__ [SE QuestionTimeline]
questionsTimeline (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/timeline") <>
  parse (attoparsec items ".questions/{ids}/timeline: ")


--------------------------
-- Reputation
--------------------------

-- | <https://api.stackexchange.com/docs/reputation-on-users>
reputationOnUsers ∷ [Int] → Request a __COUNTER__ [SE Reputation]
reputationOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/reputation") <>
  parse (attoparsec items ".users/{ids}/reputation: ")


-- | <https://api.stackexchange.com/docs/me-reputation>
meReputation ∷ Request RequireToken __COUNTER__ [SE Reputation]
meReputation = path "me/reputation" <> parse (attoparsec items ".me/reputation: ")


--------------------------
-- Reputation History
-------------------------

-- | <https://api.stackexchange.com/docs/reputation-history>
reputationHistory ∷ [Int] → Request a __COUNTER__ [SE ReputationHistory]
reputationHistory (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/reputation-history") <>
  parse (attoparsec items ".users/{ids}/reputation-history: ")


-- | <https://api.stackexchange.com/docs/me-reputation-history>
meReputationHistory ∷ Request RequireToken __COUNTER__ [SE ReputationHistory]
meReputationHistory = path ("me/reputation-history") <> parse (attoparsec items ".me/reputation-history: ")


-- | <https://api.stackexchange.com/docs/full-reputation-history>
reputationHistoryFull ∷ [Int] → Request RequireToken __COUNTER__ [SE ReputationHistory]
reputationHistoryFull (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/reputation-history/full") <>
  parse (attoparsec items ".users/{ids}/reputation-history/full: ")


-- | <https://api.stackexchange.com/docs/me-full-reputation-history>
meReputationHistoryFull ∷ Request RequireToken __COUNTER__ [SE ReputationHistory]
meReputationHistoryFull = path ("me/reputation-history/full") <> parse (attoparsec items ".me/reputation-history/full: ")


--------------------------
-- Revisions
--------------------------

-- | <https://api.stackexchange.com/docs/revisions-by-ids>
revisionsByIds ∷ [Int] → Request a __COUNTER__ [SE Revision]
revisionsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("posts/" <> is <> "/revisions") <>
  parse (attoparsec items ".posts/{ids}/revisions: ")


-- | <https://api.stackexchange.com/docs/revisions-by-guids>
revisionsByGuids ∷ [Text] → Request a __COUNTER__ [SE Revision]
revisionsByGuids (T.intercalate ";" → is) =
  path ("revisions/" <> is) <>
  parse (attoparsec items ".revisions/{ids}: ")


--------------------------
-- Sites
--------------------------

-- | <https://api.stackexchange.com/docs/sites>
sites ∷ Request a __COUNTER__ [SE Site]
sites = path "sites" <> parse (attoparsec items ".sites: ")


--------------------------
-- Suggested Edits
--------------------------

-- | <https://api.stackexchange.com/docs/posts-on-suggested-edits>
postsOnSuggestedEdits ∷ [Int] → Request a __COUNTER__ [SE SuggestedEdit]
postsOnSuggestedEdits (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("posts/" <> is <> "/suggested-edits") <>
  parse (attoparsec items ".posts/{ids}/suggested-edits: ")


-- | <https://api.stackexchange.com/docs/suggested-edits>
suggestedEdits ∷ Request a __COUNTER__ [SE SuggestedEdit]
suggestedEdits =
  path "suggested-edits" <> parse (attoparsec items ".suggested-edits: ")


-- | <https://api.stackexchange.com/docs/suggested-edits-by-ids>
suggestedEditsByIds ∷ [Int] → Request a __COUNTER__ [SE SuggestedEdit]
suggestedEditsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("suggested-edits/" <> is ) <>
  parse (attoparsec items ".suggested-edits/{ids}: ")


-- | <https://api.stackexchange.com/docs/suggested-edits-on-users>
suggestedEditsOnUsers ∷ [Int] → Request a __COUNTER__ [SE SuggestedEdit]
suggestedEditsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/suggested-edits") <>
  parse (attoparsec items ".users/{ids}/suggested-edits: ")


-- | <https://api.stackexchange.com/docs/me-suggested-edits>
meSuggestedEdits ∷ Request RequireToken __COUNTER__ [SE SuggestedEdit]
meSuggestedEdits = path ("me/suggested-edits") <> parse (attoparsec items ".me/suggested-edits: ")


--------------------------
-- Tags
--------------------------

-- | <https://api.stackexchange.com/docs/tags>
tags ∷ Request a __COUNTER__ [SE Tag]
tags = path "tags" <> parse (attoparsec items ".tags: ")


-- | <https://api.stackexchange.com/docs/moderator-only-tags>
moderatorOnlyTags ∷ Request a __COUNTER__ [SE Tag]
moderatorOnlyTags =
  path "tags/moderator-only" <>
  parse (attoparsec items ".tags/moderator-only: ")


-- | <https://api.stackexchange.com/docs/required-tags>
requiredTags ∷ Request a __COUNTER__ [SE Tag]
requiredTags =
  path "tags/required" <> parse (attoparsec items ".tags/required: ")


-- | <https://api.stackexchange.com/docs/tags-by-name>
tagsByName ∷ [Text] → Request a __COUNTER__ [SE Tag]
tagsByName (T.intercalate ";" → ts) =
  path ("tags/" <> ts <> "/info") <>
  parse (attoparsec items ".tags/{tags}/info: ")


-- | <https://api.stackexchange.com/docs/related-tags>
relatedTags ∷ [Text] → Request a __COUNTER__ [SE Tag]
relatedTags (T.intercalate ";" → ts) =
  path ("tags/" <> ts <> "/related") <>
  parse (attoparsec items ".tags/{tags}/related: ")


-- | <https://api.stackexchange.com/docs/tags-on-users>
tagsOnUsers ∷ [Int] → Request a __COUNTER__ [SE Tag]
tagsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/tags") <>
  parse (attoparsec items ".users/{ids}/tags: ")


-- | <https://api.stackexchange.com/docs/me-tags>
meTags ∷ Request RequireToken __COUNTER__ [SE Tag]
meTags = path ("me/tags") <> parse (attoparsec items ".me/tags: ")


--------------------------
-- Tag Scores
--------------------------

-- | <https://api.stackexchange.com/docs/top-answerers-on-tags>
topAnswerersOnTag ∷ Text → Text → Request a __COUNTER__ [SE TagScore]
topAnswerersOnTag t p =
  path ("tags/" <> t <> "/top-answerers/" <> p) <>
  parse (attoparsec items ".tags/{tag}/top-answerers/{period}: ")


-- | <https://api.stackexchange.com/docs/top-askers-on-tags>
topAskersOnTag ∷ Text → Text → Request a __COUNTER__ [SE TagScore]
topAskersOnTag t p =
  path ("tags/" <> t <> "/top-askers/" <> p) <>
  parse (attoparsec items ".tags/{tag}/top-askers/{period}: ")


--------------------------
-- Tag Synonyms
--------------------------

-- | <https://api.stackexchange.com/docs/tag-synonyms>
tagSynonyms ∷ Request a __COUNTER__ [SE TagSynonym]
tagSynonyms =
  path "tags/synonyms" <> parse (attoparsec items ".tags/synonyms: ")


-- | <https://api.stackexchange.com/docs/synonyms-by-tags>
synonymsByTags ∷ [Text] → Request a __COUNTER__ [SE TagSynonym]
synonymsByTags (T.intercalate ";" → ts) =
  path ("tags/" <> ts <> "/synonyms") <>
  parse (attoparsec items ".tags/{tags}/synonyms: ")


--------------------------
-- Tag Wikis
--------------------------

-- | <https://api.stackexchange.com/docs/wikis-by-tags>
wikisByTags ∷ [Text] → Request a __COUNTER__ [SE TagWiki]
wikisByTags (T.intercalate ";" → ts) =
  path ("tags/" <> ts <> "/wikis") <>
  parse (attoparsec items ".tags/{tags}/wikis: ")


--------------------------
-- Top Tags
--------------------------

-- | <https://api.stackexchange.com/docs/top-answer-tags-on-users>
topAnswerTagsOnUsers ∷ Int → Request a __COUNTER__ [SE TopTag]
topAnswerTagsOnUsers (toLazyText . decimal → i) =
  path ("users/" <> i <> "/top-answer-tags") <>
  parse (attoparsec items ".users/{id}/top-answer-tags: ")


-- | <https://api.stackexchange.com/docs/me-top-answer-tags>
meTopAnswerTags ∷ Request RequireToken __COUNTER__ [SE TopTag]
meTopAnswerTags = path "me/top-answer-tags" <> parse (attoparsec items ".me/top-answer-tags: ")


-- | <https://api.stackexchange.com/docs/top-question-tags-on-users>
topQuestionTagsOnUsers ∷ Int → Request a __COUNTER__ [SE TopTag]
topQuestionTagsOnUsers (toLazyText . decimal → i) =
  path ("users/" <> i <> "/top-question-tags") <>
  parse (attoparsec items ".users/{id}/top-question-tags: ")


-- | <https://api.stackexchange.com/docs/me-top-question-tags>
meTopQuestionTags ∷ Request RequireToken __COUNTER__ [SE TopTag]
meTopQuestionTags = path "me/top-question-tags" <> parse (attoparsec items ".me/top-question-tags: ")


--------------------------
-- Users
--------------------------

-- | <https://api.stackexchange.com/docs/users>
users ∷ Request a __COUNTER__ [SE User]
users = path "users" <> parse (attoparsec items ".users: ")


-- | <https://api.stackexchange.com/docs/users-by-ids>
usersByIds ∷ [Int] → Request a __COUNTER__ [SE User]
usersByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is) <> parse (attoparsec items ".users/{ids}: ")


-- | <https://api.stackexchange.com/docs/me>
me ∷ Request RequireToken __COUNTER__ (SE User)
me = path "me" <> parse (head . attoparsec items ".me: ")


-- | <https://api.stackexchange.com/docs/moderators>
moderators ∷ Request a __COUNTER__ [SE User]
moderators =
  path "users/moderators" <> parse (attoparsec items ".users/moderators: ")


-- | <https://api.stackexchange.com/docs/elected-moderators>
electedModerators ∷ Request a __COUNTER__ [SE User]
electedModerators =
  path "users/moderators/elected" <>
  parse (attoparsec items ".users/moderators/elected: ")


--------------------------
-- User Timeline
--------------------------

-- | <https://api.stackexchange.com/docs/timeline-on-users>
timelineOnUsers ∷ [Int] → Request a __COUNTER__ [SE UserTimeline]
timelineOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/timeline") <>
  parse (attoparsec items ".users/{ids}/timeline: ")


-- | <https://api.stackexchange.com/docs/me-timeline>
meTimeline ∷ Request RequireToken __COUNTER__ [SE UserTimeline]
meTimeline = path "me/timeline" <> parse (attoparsec items ".me/timeline: ")


--------------------------
-- Write Permissions
--------------------------

-- | <https://api.stackexchange.com/docs/write-permissions>
writePermissions ∷ Int → Request a __COUNTER__ [SE WritePermission]
writePermissions (toLazyText . decimal → i) =
  path ("users/" <> i <> "/write-permissions") <>
  parse (attoparsec items ".users/{id}/write-permissions: ")


-- | <https://api.stackexchange.com/docs/me-write-permissions>
meWritePermissions ∷ Request RequireToken __COUNTER__ [SE WritePermission]
meWritePermissions = path "me/write-permissions" <> parse (attoparsec items ".me/write-permissions: ")


attoparsec ∷ (Value → Maybe b) → String → ByteString → b
attoparsec f msg request = case AP.eitherResult $ AP.parse A.json request of
  Right s → case f s of
    Just b → b
    Nothing → throw $ SEException request ("libstackexchange" ++ msg ++ "incorrect JSON content")
  Left e → throw $ SEException request ("libstackexchange" ++ msg ++ e)


items ∷ (Functor m, Monad m) ⇒ Value → m [SE a]
items s = (SE s ^! field "items")
