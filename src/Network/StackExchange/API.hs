{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module Network.StackExchange.API where

import Data.Monoid ((<>))

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)

import Network.StackExchange.JSON
import Network.StackExchange.Request
import Network.StackExchange.Types

--------------------------
-- Answers
--------------------------

-- | <https://api.stackexchange.com/docs/answers>
answers ∷ Request a 1 [SE Answer]
answers = path "answers" <> parse (attoparsec items ".answers: ")


-- | <https://api.stackexchange.com/docs/answers-by-ids>
answersByIds ∷ [Int] → Request a 2 [SE Answer]
answersByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("answers/" <> is) <> parse (attoparsec items ".answers/{ids}: ")


-- | <https://api.stackexchange.com/docs/answers-on-users>
answersOnUsers ∷ [Int] → Request a 3 [SE Answer]
answersOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/answers") <>
  parse (attoparsec items ".users/{ids}/answers: ")


-- | <https://api.stackexchange.com/docs/answers-on-questions>
answersOnQuestions ∷ [Int] → Request a 4 [SE Answer]
answersOnQuestions (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/answers") <>
  parse (attoparsec items ".questions/{ids}/answers: ")


-- | <https://api.stackexchange.com/docs/top-user-answers-in-tags>
topUserAnswersInTags ∷ Int → [Text] → Request a 5 [SE Answer]
topUserAnswersInTags (toLazyText . decimal → i) (T.intercalate ";" → ts) =
  path ("users/" <> i <> "/tags/" <> ts <> "/top-answers") <>
  parse (attoparsec items ".users/{id}/tags/{tags}/top-answers: ")


--------------------------
-- Badges
--------------------------

-- | <https://api.stackexchange.com/docs/badges>
badges ∷ Request a 9 [SE Badge]
badges = path "badges" <> parse (attoparsec items ".badges: ")


-- | <https://api.stackexchange.com/docs/badges-by-ids>
badgesByIds ∷ [Int] → Request a 10 [SE Badge]
badgesByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("badges/" <> is) <> parse (attoparsec items ".badges/{ids}: ")


-- | <https://api.stackexchange.com/docs/badge-recipients-by-ids>
badgeRecipientsByIds ∷ [Int] → Request a 11 [SE Badge]
badgeRecipientsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("badges" <> is <> "/recipients") <>
  parse (attoparsec items ".badges/{ids}/recipients: ")


-- | <https://api.stackexchange.com/docs/badges-by-name>
badgesByName ∷ Request a 12 [SE Badge]
badgesByName =
  path ("badges" <> "/name") <> parse (attoparsec items ".badges/name: ")


-- | <https://api.stackexchange.com/docs/badge-recipients>
badgeRecipients ∷ Request a 13 [SE Badge]
badgeRecipients =
  path ("badges" <> "/recipients") <>
  parse (attoparsec items ".badges/recipients: ")


-- | <https://api.stackexchange.com/docs/badges-by-tag>
badgesByTag ∷ Request a 14 [SE Badge]
badgesByTag =
  path ("badges" <> "/tags") <> parse (attoparsec items ".badges/tags: ")


-- | <https://api.stackexchange.com/docs/badges-on-users>
badgesOnUsers ∷ [Int] → Request a 15 [SE Badge]
badgesOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/badges") <>
  parse (attoparsec items ".users/{ids}/badges: ")


--------------------------
-- Comments
--------------------------

-- | <https://api.stackexchange.com/docs/comments-on-answers>
commentsOnAnswers ∷ [Int] → Request a 16 [SE Comment]
commentsOnAnswers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("answers/" <> is <> "/comments") <>
  parse (attoparsec items ".answers/{ids}/comments: ")


-- | <https://api.stackexchange.com/docs/comments>
comments ∷ Request a 17 [SE Comment]
comments = path "comments" <> parse (attoparsec items ".comments: ")


-- | <https://api.stackexchange.com/docs/comments-by-ids>
commentsByIds ∷ [Int] → Request a 18 [SE Comment]
commentsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("comments/" <> is) <> parse (attoparsec items ".comments/{ids}: ")


-- | <https://api.stackexchange.com/docs/comments-on-posts>
commentsOnPosts ∷ [Int] → Request a 19 [SE Comment]
commentsOnPosts (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("posts/" <> is <> "/comments") <>
  parse (attoparsec items ".posts/{ids}/comments: ")


-- | <https://api.stackexchange.com/docs/comments-on-questions>
commentsOnQuestions ∷ [Int] → Request a 20 [SE Comment]
commentsOnQuestions (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/comments") <>
  parse (attoparsec items ".questions/{ids}/comments: ")


-- | <https://api.stackexchange.com/docs/comments-on-users>
commentsOnUsers ∷ [Int] → Request a 21 [SE Comment]
commentsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/comments") <>
  parse (attoparsec items ".users/{ids}/comments: ")


-- | <https://api.stackexchange.com/docs/comments-by-users-to-user>
commentsByUsersToUser ∷ [Int] → Int → Request a 22 [SE Comment]
commentsByUsersToUser (T.intercalate ";" . map (toLazyText . decimal) → is)
                      ((toLazyText . decimal) → toid) =
  path ("users/" <> is <> "/comments/" <> toid) <>
  parse (attoparsec items ".users/{ids}/comments/{toid}: ")


-- | <https://api.stackexchange.com/docs/mentions-on-users>
mentionsOnUsers ∷ [Int] → Request a 23 [SE Comment]
mentionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/mentioned") <>
  parse (attoparsec items ".users/{ids}/mentioned: ")


--------------------------
-- Errors
--------------------------

-- | <https://api.stackexchange.com/docs/errors>
errors ∷ Request a 24 [SE Error]
errors = path "errors" <> parse (attoparsec items ".errors: ")


--------------------------
-- Filters
--------------------------

-- | <https://api.stackexchange.com/docs/create-filter>
createFilter ∷ [Text] → [Text] → Text → Request a 6 (SE Filter)
createFilter (T.intercalate ";" → include) (T.intercalate ";" → exclude) base =
  path "filter/create" <>
  query [("include", include), ("exclude", exclude), ("base", base)] <>
  parse (attoparsec (return . SE) ".filter/create: ")


-- | <https://api.stackexchange.com/docs/read-filter>
readFilter ∷ [Text] → Request a 7 [SE Filter]
readFilter (T.intercalate ";" → filters) =
  path "filters" <>
  query [("filters", filters)] <>
  parse (attoparsec items ".filters: ")


--------------------------
-- Info
--------------------------

-- | <https://api.stackexchange.com/docs/info>
info ∷ Request a 25 (SE Info)
info = path "info" <> parse (attoparsec (return . SE) ".info: ")


--------------------------
-- Network Users
--------------------------

-- | <https://api.stackexchange.com/docs/associated-users>
associatedUsers ∷ [Int] → Request a 26 [SE UserNetwork]
associatedUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/associated") <>
  parse (attoparsec items ".users/{ids}/associated: ")


--------------------------
-- Merge History
--------------------------

-- | <https://api.stackexchange.com/docs/merge-history>
mergeHistory ∷ [Int] → Request a 27 [SE AccountMerge]
mergeHistory (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/merges") <>
  parse (attoparsec items ".users/{ids}/merges: ")


--------------------------
-- Posts
--------------------------

-- | <https://api.stackexchange.com/docs/posts>
posts ∷ Request a 50 [SE Post]
posts = path "posts" <> parse (attoparsec items ".posts: ")


-- | <https://api.stackexchange.com/docs/posts-by-ids>
postsByIds ∷ [Int] → Request a 51 [SE Post]
postsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("posts/" <> is) <> parse (attoparsec items ".posts/{ids}: ")


--------------------------
-- Privileges
--------------------------

-- | <https://api.stackexchange.com/docs/privileges>
privileges ∷ Request a 28 [SE Privilege]
privileges = path "privileges" <> parse (attoparsec items ".privileges: ")


-- | <https://api.stackexchange.com/docs/privileges-on-users>
privilegesOnUsers ∷ Int → Request a 29 [SE Privilege]
privilegesOnUsers ((toLazyText . decimal) → i) =
  path ("users/" <> i <> "/privileges") <>
  parse (attoparsec items ".users/{ids}/privileges: ")


--------------------------
-- Questions
--------------------------

-- | <https://api.stackexchange.com/docs/questions>
questions ∷ Request a 30 [SE Question]
questions = path "questions" <> parse (attoparsec items ".questions: ")


-- | <https://api.stackexchange.com/docs/questions-by-ids>
questionsByIds ∷ [Int] → Request a 31 [SE Question]
questionsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is) <> parse (attoparsec items ".questions/{ids}: ")


-- | <https://api.stackexchange.com/docs/linked-questions>
linkedQuestions ∷ [Int] → Request a 32 [SE Question]
linkedQuestions (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/linked") <>
  parse (attoparsec items ".questions/{ids}/linked: ")


-- | <https://api.stackexchange.com/docs/related-questions>
relatedQuestions ∷ [Int] → Request a 33 [SE Question]
relatedQuestions (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/related") <>
  parse (attoparsec items ".questions/{ids}/related: ")


-- | <https://api.stackexchange.com/docs/featured-questions>
featuredQuestions ∷ Request a 34 [SE Question]
featuredQuestions =
  path "questions/featured" <> parse (attoparsec items ".questions/featured: ")


-- | <https://api.stackexchange.com/docs/unanswered-questions>
unansweredQuestions ∷ Request a 35 [SE Question]
unansweredQuestions =
  path "questions/unanswered" <>
  parse (attoparsec items ".questions/unanswered: ")


-- | <https://api.stackexchange.com/docs/no-answer-questions>
noAnswerQuestions ∷ Request a 36 [SE Question]
noAnswerQuestions =
  path "questions/no-answers" <>
  parse (attoparsec items ".questions/no-answers: ")


-- | <https://api.stackexchange.com/docs/search>
search ∷ Text → [Text] → Request a 37 [SE Question]
search t (T.intercalate ";" → ts) =
  path "search" <>
  query [("intitle",t),("tagged",ts)] <>
  parse (attoparsec items ".search: ")


-- | <https://api.stackexchange.com/docs/advanced-search>
advancedSearch ∷ Request a 38 [SE Question]
advancedSearch =
  path "search/advanced" <> parse (attoparsec items ".search/advanced: ")


-- | <https://api.stackexchange.com/docs/similar>
similar ∷ Text → [Text] → Request a 39 [SE Question]
similar t (T.intercalate ";" → ts) =
  path "similar" <>
  query [("title",t),("tagged",ts)] <>
  parse (attoparsec items ".similar: ")


-- | <https://api.stackexchange.com/docs/faqs-by-tags>
faqsByTags ∷ [Text] → Request a 40 [SE Question]
faqsByTags (T.intercalate ";" → ts) =
  path ("tags/" <> ts <> "/faq") <>
  parse (attoparsec items ".tags/{tags}/faq: ")


-- | <https://api.stackexchange.com/docs/favorites-on-users>
favoritesOnUsers ∷ [Int] → Request a 41 [SE Question]
favoritesOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/favorites") <>
  parse (attoparsec items ".users/{ids}/favorites: ")


-- | <https://api.stackexchange.com/docs/questions-on-users>
questionsOnUsers ∷ [Int] → Request a 42 [SE Question]
questionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions") <>
  parse (attoparsec items ".users/{ids}/questions: ")


-- | <https://api.stackexchange.com/docs/featured-questions-on-users>
featuredQuestionsOnUsers ∷ [Int] → Request a 43 [SE Question]
featuredQuestionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions/featured") <>
  parse (attoparsec items ".users/{ids}/questions/featured: ")


-- | <https://api.stackexchange.com/docs/no-answer-questions-on-users>
noAnswerQuestionsOnUsers ∷ [Int] → Request a 44 [SE Question]
noAnswerQuestionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions/no-answers") <>
  parse (attoparsec items ".users/{ids}/questions/no-answers: ")


-- | <https://api.stackexchange.com/docs/unaccepted-questions-on-users>
unacceptedQuestionsOnUsers ∷ [Int] → Request a 45 [SE Question]
unacceptedQuestionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions/unaccepted") <>
  parse (attoparsec items ".users/{ids}/questions/unaccepted: ")


-- | <https://api.stackexchange.com/docs/unanswered-questions-on-users>
unansweredQuestionsOnUsers ∷ [Int] → Request a 46 [SE Question]
unansweredQuestionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions/unanswered") <>
  parse (attoparsec items ".users/{ids}/questions/unanswered: ")


-- | <https://api.stackexchange.com/docs/top-user-questions-in-tags>
topUserQuestionsInTags ∷ Int → [Text] → Request a 47 [SE Question]
topUserQuestionsInTags ((toLazyText . decimal) → i) (T.intercalate ";" → ts) =
  path ("users/" <> i <> "/tags/" <> ts <> "/top-questions") <>
    parse (attoparsec items ".users/{id}/tags/{tags}/top-questions: ")


--------------------------
-- Question Timelines
--------------------------

-- | <https://api.stackexchange.com/docs/questions-timeline>
questionsTimeline ∷ [Int] → Request a 48 [SE QuestionTimeline]
questionsTimeline (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/timeline") <>
  parse (attoparsec items ".questions/{ids}/timeline: ")


--------------------------
-- Reputation
--------------------------

-- | <https://api.stackexchange.com/docs/reputation-on-users>
reputationOnUsers ∷ [Int] → Request a 49 [SE Reputation]
reputationOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/reputation") <>
  parse (attoparsec items ".users/{ids}/reputation: ")


--------------------------
-- Reputation History
-------------------------

-- | <https://api.stackexchange.com/docs/reputation-history>
reputationHistory ∷ [Int] → Request a 52 [SE Reputation]
reputationHistory (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/reputation-history") <>
  parse (attoparsec items ".users/{ids}/reputation-history: ")


--------------------------
-- Revisions
--------------------------

-- | <https://api.stackexchange.com/docs/revisions-by-ids>
revisionsByIds ∷ [Int] → Request a 53 [SE Revision]
revisionsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("posts/" <> is <> "/revisions") <>
  parse (attoparsec items ".posts/{ids}/revisions: ")


-- | <https://api.stackexchange.com/docs/revisions-by-guids>
revisionsByGuids ∷ [Text] → Request a 54 [SE Revision]
revisionsByGuids (T.intercalate ";" → is) =
  path ("revisions/" <> is) <>
  parse (attoparsec items ".revisions/{ids}: ")


--------------------------
-- Sites
--------------------------

-- | <https://api.stackexchange.com/docs/sites>
sites ∷ Request a 55 [SE Site]
sites = path "sites" <> parse (attoparsec items ".sites: ")


--------------------------
-- Suggested Edits
--------------------------

-- | <https://api.stackexchange.com/docs/posts-on-suggested-edits>
postsOnSuggestedEdits ∷ [Int] → Request a 56 [SE SuggestedEdit]
postsOnSuggestedEdits (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("posts/" <> is <> "/suggested-edits") <>
  parse (attoparsec items ".posts/{ids}/suggested-edits: ")


-- | <https://api.stackexchange.com/docs/suggested-edits>
suggestedEdits ∷ Request a 57 [SE SuggestedEdit]
suggestedEdits =
  path "suggested-edits" <> parse (attoparsec items ".suggested-edits: ")


-- | <https://api.stackexchange.com/docs/suggested-edits-by-ids>
suggestedEditsByIds ∷ [Int] → Request a 58 [SE SuggestedEdit]
suggestedEditsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("suggested-edits/" <> is ) <>
  parse (attoparsec items ".suggested-edits/{ids}: ")


-- | <https://api.stackexchange.com/docs/suggested-edits-on-users>
suggestedEditsOnUsers ∷ [Int] → Request a 59 [SE SuggestedEdit]
suggestedEditsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/suggested-edits") <>
  parse (attoparsec items ".users/{ids}/suggested-edits: ")


--------------------------
-- Tags
--------------------------

-- | <https://api.stackexchange.com/docs/tags>
tags ∷ Request a 60 [SE Tag]
tags = path "tags" <> parse (attoparsec items ".tags: ")


-- | <https://api.stackexchange.com/docs/moderator-only-tags>
moderatorOnlyTags ∷ Request a 61 [SE Tag]
moderatorOnlyTags =
  path "tags/moderator-only" <>
  parse (attoparsec items ".tags/moderator-only: ")


-- | <https://api.stackexchange.com/docs/required-tags>
requiredTags ∷ Request a 62 [SE Tag]
requiredTags =
  path "tags/required" <> parse (attoparsec items ".tags/required: ")


-- | <https://api.stackexchange.com/docs/tags-by-name>
tagsByName ∷ [Text] → Request a 63 [SE Tag]
tagsByName (T.intercalate ";" → ts) =
  path ("tags/" <> ts <> "/info") <>
  parse (attoparsec items ".tags/{tags}/info: ")


-- | <https://api.stackexchange.com/docs/related-tags>
relatedTags ∷ [Text] → Request a 64 [SE Tag]
relatedTags (T.intercalate ";" → ts) =
  path ("tags/" <> ts <> "/related") <>
  parse (attoparsec items ".tags/{tags}/related: ")


-- | <https://api.stackexchange.com/docs/tags-on-users>
tagsOnUsers ∷ [Int] → Request a 65 [SE Tag]
tagsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/tags") <>
  parse (attoparsec items ".users/{ids}/tags: ")


--------------------------
-- Tag Scores
--------------------------
data Period = AllTime | Month

topath ∷ Period → Text
topath Month = "month"
topath _     = "all_time"


-- | <https://api.stackexchange.com/docs/top-answerers-on-tags>
topAnswerersOnTag ∷ Text → Period → Request a 66 [SE TagScore]
topAnswerersOnTag t p =
  path ("tags/" <> t <> "/top-answerers/" <> (topath p)) <>
  parse (attoparsec items ".tags/{tag}/top-answerers/{period}: ")


-- | <https://api.stackexchange.com/docs/top-askers-on-tags>
topAskersOnTag ∷ Text → Period → Request a 67 [SE TagScore]
topAskersOnTag t p =
  path ("tags/" <> t <> "/top-askers/" <> (topath p)) <>
  parse (attoparsec items ".tags/{tag}/top-askers/{period}: ")


--------------------------
-- Tag Synonyms
--------------------------


--------------------------
-- Tag Wikis
--------------------------


--------------------------
-- Top Tags
--------------------------


--------------------------
-- Users
--------------------------

-- | <https://api.stackexchange.com/docs/users-by-ids>
usersByIds ∷ [Int] → Request a 8 [SE User]
usersByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is) <> parse (attoparsec items ".users/{ids}: ")

--------------------------
-- User Timeline
--------------------------


--------------------------
-- Write Permissions
--------------------------
