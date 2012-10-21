{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module Network.StackExchange.API where

import Data.Monoid ((<>))

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (fromString, toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)

import Network.StackExchange.JSON
import Network.StackExchange.Request
import Network.StackExchange.Types


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
  path ("users/" <> is <> "/answers") <> parse (attoparsec items ".users/{ids}/answers: ")


-- | <https://api.stackexchange.com/docs/answers-on-questions>
answersOnQuestions ∷ [Int] → Request a 4 [SE Answer]
answersOnQuestions (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/answers") <> parse (attoparsec items ".questions/{ids}/answers: ")


-- | <https://api.stackexchange.com/docs/top-user-answers-in-tags>
topUserAnswersInTags ∷ Int → [Text] → Request a 5 [SE Answer]
topUserAnswersInTags (toLazyText . decimal → i) (T.intercalate ";" → ts) =
  path ("users/" <> i <> "/tags/" <> ts <> "/top-answers") <>
  parse (attoparsec items ".users/{id}/tags/{tags}/top-answers: ")


-- | <https://api.stackexchange.com/docs/badges>
badges ∷ Request a 9 [SE Answer]
badges = path "badges" <> parse (attoparsec items ".badges: ")


-- | <https://api.stackexchange.com/docs/badges-by-ids>
badgesByIds ∷ [Int] → Request a 10 [SE Answer]
badgesByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("badges/" <> is) <> parse (attoparsec items ".badges/{ids}: ")


-- | <https://api.stackexchange.com/docs/badge-recipients-by-ids>
badgeRecipientsByIds ∷ [Int] → Request a 11 [SE Answer]
badgeRecipientsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("badges" <> is <> "/recipients") <> parse (attoparsec items ".badges/{ids}/recipients: ")


-- | <https://api.stackexchange.com/docs/badges-by-name>
badgesByName ∷ Request a 12 [SE Answer]
badgesByName = path ("badges" <> "/name") <> parse (attoparsec items ".badges/name: ")


-- | <https://api.stackexchange.com/docs/badge-recipients>
badgeRecipients ∷ Request a 13 [SE Answer]
badgeRecipients = path ("badges" <> "/recipients") <> parse (attoparsec items ".badges/recipients: ")


-- | <https://api.stackexchange.com/docs/badges-by-tag>
badgesByTag ∷ Request a 14 [SE Answer]
badgesByTag = path ("badges" <> "/tags") <> parse (attoparsec items ".badges/tags: ")


-- | <https://api.stackexchange.com/docs/badges-on-users>
badgesOnUsers ∷ [Int] → Request a 15 [SE Answer]
badgesOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/badges") <> parse (attoparsec items ".users/{ids}/badges: ")


-- | <https://api.stackexchange.com/docs/comments-on-answers>
commentsOnAnswers ∷ [Int] → Request a 16 [SE Answer]
commentsOnAnswers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("answers/" <> is <> "/comments") <> parse (attoparsec items ".answers/{ids}/comments: ")


-- | <https://api.stackexchange.com/docs/comments>
comments ∷ Request a 17 [SE Answer]
comments = path "comments" <> parse (attoparsec items ".comments: ")


-- | <https://api.stackexchange.com/docs/comments-by-ids>
commentsByIds ∷ [Int] → Request a 18 [SE Answer]
commentsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("comments/" <> is) <> parse (attoparsec items ".comments/{ids}: ")


-- | <https://api.stackexchange.com/docs/comments-on-posts>
commentsOnPosts ∷ [Int] → Request a 19 [SE Answer]
commentsOnPosts (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("posts/" <> is <> "/comments") <> parse (attoparsec items ".posts/{ids}/comments: ")


-- | <https://api.stackexchange.com/docs/comments-on-questions>
commentsOnQuestions ∷ [Int] → Request a 20 [SE Answer]
commentsOnQuestions (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/comments") <> parse (attoparsec items ".questions/{ids}/comments: ")


-- | <https://api.stackexchange.com/docs/comments-on-users>
commentsOnUsers ∷ [Int] → Request a 21 [SE Answer]
commentsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/comments") <> parse (attoparsec items ".users/{ids}/comments: ")


-- | <https://api.stackexchange.com/docs/comments-by-users-to-user>
commentsByUsersToUser ∷ [Int] → Int → Request a 22 [SE Answer]
commentsByUsersToUser (T.intercalate ";" . map (toLazyText . decimal) → is)
                      ((toLazyText . decimal) → toid) =
  path ("users/" <> is <> "/comments/" <> toid) <> parse (attoparsec items ".users/{ids}/comments/{toid}: ")


-- | <https://api.stackexchange.com/docs/mentions-on-users>
mentionsOnUsers ∷ [Int] → Request a 23 [SE Answer]
mentionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/mentioned") <> parse (attoparsec items ".users/{ids}/mentioned: ")


-- | <https://api.stackexchange.com/docs/errors>
errors ∷ Request a 24 [SE Answer]
errors = path "errors" <> parse (attoparsec items ".errors: ")


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


-- | <https://api.stackexchange.com/docs/info>
info ∷ Request a 25 [SE Answer]
info = path "info" <> parse (attoparsec items ".info: ")


-- | <https://api.stackexchange.com/docs/associated-users>
associatedUsers ∷ [Int] → Request a 26 [SE Answer]
associatedUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/associated") <> parse (attoparsec items ".users/{ids}/associated: ")


-- | <https://api.stackexchange.com/docs/merge-history>
mergeHistory ∷ [Int] → Request a 27 [SE Answer]
mergeHistory (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/merges") <> parse (attoparsec items ".users/{ids}/merges: ")


-- | <https://api.stackexchange.com/docs/privileges>
privileges ∷ Request a 28 [SE Answer]
privileges = path "privileges" <> parse (attoparsec items ".privileges: ")


-- | <https://api.stackexchange.com/docs/privileges-on-users>
privilegesOnUsers ∷ Int → Request a 29 [SE Answer]
privilegesOnUsers ((toLazyText . decimal) → i) =
  path ("users/" <> i <> "/privileges") <> parse (attoparsec items ".users/{ids}/privileges: ")


-- | <https://api.stackexchange.com/docs/questions>
questions ∷ Request a 30 [SE Answer]
questions = path "questions" <> parse (attoparsec items ".questions: ")


-- | <https://api.stackexchange.com/docs/questions-by-ids>
questionsByIds ∷ [Int] → Request a 31 [SE Answer]
questionsByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is) <> parse (attoparsec items ".questions/{ids}: ")


-- | <https://api.stackexchange.com/docs/linked-questions>
linkedQuestions ∷ [Int] → Request a 32 [SE Answer]
linkedQuestions (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/linked") <> parse (attoparsec items ".questions/{ids}/linked: ")


-- | <https://api.stackexchange.com/docs/related-questions>
relatedQuestions ∷ [Int] → Request a 33 [SE Answer]
relatedQuestions (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/related") <> parse (attoparsec items ".questions/{ids}/related: ")


-- | <https://api.stackexchange.com/docs/featured-questions>
featuredQuestions ∷ Request a 34 [SE Answer]
featuredQuestions = path "questions/featured" <> parse (attoparsec items ".questions/featured: ")


-- | <https://api.stackexchange.com/docs/unanswered-questions>
unansweredQuestions ∷ Request a 35 [SE Answer]
unansweredQuestions = path "questions/unanswered" <> parse (attoparsec items ".questions/unanswered: ")


-- | <https://api.stackexchange.com/docs/no-answer-questions>
noAnswerQuestions ∷ Request a 36 [SE Answer]
noAnswerQuestions = path "questions/no-answers" <> parse (attoparsec items ".questions/no-answers: ")


-- | <https://api.stackexchange.com/docs/search>
search ∷ Request a 37 [SE Answer]
search = path "search" <> parse (attoparsec items ".search: ")


-- | <https://api.stackexchange.com/docs/advanced-search>
advancedSearch ∷ Request a 38 [SE Answer]
advancedSearch = path "search/advanced" <> parse (attoparsec items ".search/advanced: ")


-- | <https://api.stackexchange.com/docs/similar>
similar ∷ Request a 39 [SE Answer]
similar = path "similar" <> parse (attoparsec items ".similar: ")


-- | <https://api.stackexchange.com/docs/faqs-by-tags>
faqsByTags ∷ [String] → Request a 40 [SE Answer]
faqsByTags (T.intercalate ";" . map (toLazyText . fromString) → tags) =
  path ("tags/" <> tags <> "/faq") <> parse (attoparsec items ".tags/{tags}/faq: ")


-- | <https://api.stackexchange.com/docs/favorites-on-users>
favoritesOnUsers ∷ [Int] → Request a 41 [SE Answer]
favoritesOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/favorites") <> parse (attoparsec items ".users/{ids}/favorites: ")


-- | <https://api.stackexchange.com/docs/questions-on-users>
questionsOnUsers ∷ [Int] → Request a 42 [SE Answer]
questionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions") <> parse (attoparsec items ".users/{ids}/questions: ")


-- | <https://api.stackexchange.com/docs/featured-questions-on-users>
featuredQuestionsOnUsers ∷ [Int] → Request a 43 [SE Answer]
featuredQuestionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions/featured") <> parse (attoparsec items ".users/{ids}/questions/featured: ")


-- | <https://api.stackexchange.com/docs/no-answer-questions-on-users>
noAnswerQuestionsOnUsers ∷ [Int] → Request a 44 [SE Answer]
noAnswerQuestionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions/no-answers") <> parse (attoparsec items ".users/{ids}/questions/no-answers: ")


-- | <https://api.stackexchange.com/docs/unaccepted-questions-on-users>
unacceptedQuestionsOnUsers ∷ [Int] → Request a 45 [SE Answer]
unacceptedQuestionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions/unaccepted") <> parse (attoparsec items ".users/{ids}/questions/unaccepted: ")


-- | <https://api.stackexchange.com/docs/unanswered-questions-on-users>
unansweredQuestionsOnUsers ∷ [Int] → Request a 46 [SE Answer]
unansweredQuestionsOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/questions/unanswered") <> parse (attoparsec items ".users/{ids}/questions/unanswered: ")


-- | <https://api.stackexchange.com/docs/top-user-questions-in-tags>
topUserQuestionsInTags ∷ Int → [String] → Request a 47 [SE Answer]
topUserQuestionsInTags ((toLazyText . decimal) → i)
  (T.intercalate ";" . map (toLazyText . fromString) → tags) =
  path ("users/" <> i <> "/tags/" <> tags <> "/top-questions") <> parse (attoparsec items ".users/{id}/tags/{tags}/top-questions: ")


-- | <https://api.stackexchange.com/docs/questions-timeline>
questionsTimeline ∷ [Int] → Request a 48 [SE Answer]
questionsTimeline (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("questions/" <> is <> "/timeline") <> parse (attoparsec items ".questions/{ids}/timeline: ")


-- | <https://api.stackexchange.com/docs/reputation-on-users>
reputationOnUsers ∷ [Int] → Request a 49 [SE Answer]
reputationOnUsers (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is <> "/reputation") <> parse (attoparsec items ".users/{ids}/reputation: ")


-- | <https://api.stackexchange.com/docs/users-by-ids>
usersByIds ∷ [Int] → Request a 8 [SE User]
usersByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is) <> parse (attoparsec items ".users/{ids}: ")
