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


-- | <https://api.stackexchange.com/docs/users-by-ids>
usersByIds ∷ [Int] → Request a 8 [SE User]
usersByIds (T.intercalate ";" . map (toLazyText . decimal) → is) =
  path ("users/" <> is) <> parse (attoparsec items ".users/{ids}: ")
