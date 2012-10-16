{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- | StackExchange API calls returning answers.
--
-- This module is not intended for importing, but you can import it if you want.
-- All API modules are re-exported by "Network.StackExchange"
--
-- 'Answer' represents an answer to question on some StackExchange site.
--
-- More information is available at <https://api.stackexchange.com/docs/types/answer>
module Network.StackExchange.API.Answer where

import Prelude hiding (id)
import Control.Monad (liftM)

import           Control.Lens
import           Control.Monad.Trans (MonadIO)
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Lazy as AP
import           Data.Monoid.Lens ((<>=))
import           Data.Text.Lazy (Text, intercalate)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)

import Control.Monad.StackExchange (StackExchangeT)
import Network.StackExchange.API (localState, request)
import Network.StackExchange.JSON (field)
import Network.StackExchange.URI
import Network.StackExchange.Types


-- | <https://api.stackexchange.com/docs/answers>
answers ∷ MonadIO m ⇒ StackExchangeT a m [SE Answer]
answers = localState $ do
  uriPath <>= ["answers"]
  uriQuery <>= [("site","stackoverflow")]
  AP.parse A.json `liftM` request >>= \case
    AP.Done _ s → map SE `liftM` (s ^! field "items")
    _           → fail ".users/{ids}/answers: Malformed JSON, cannot parse"


-- | <https://api.stackexchange.com/docs/answers-by-ids>
answersByIds ∷ MonadIO m ⇒ [Int] → StackExchangeT a m [SE Answer]
answersByIds (intercalate ";" . map (toLazyText . decimal) → ids) = localState $ do
  uriPath <>= ["answers", ids]
  uriQuery <>= [("site","stackoverflow")]
  AP.parse A.json `liftM` request >>= \case
    AP.Done _ s → map SE `liftM` (s ^! field "items")
    _           → fail ".users/{ids}/answers: Malformed JSON, cannot parse"


-- | <https://api.stackexchange.com/docs/answers-on-users>
answersOnUsers ∷ MonadIO m ⇒ [Int] → StackExchangeT a m [SE Answer]
answersOnUsers (intercalate ";" . map (toLazyText . decimal) → ids) = localState $ do
  uriPath <>= ["users", ids, "answers"]
  uriQuery <>= [("site","stackoverflow")]
  AP.parse A.json `liftM` request >>= \case
    AP.Done _ s → map SE `liftM` (s ^! field "items")
    _           → fail ".users/{ids}/answers: Malformed JSON, cannot parse"


-- | <https://api.stackexchange.com/docs/answers-on-questions>
answersOnQuestions ∷ MonadIO m ⇒ [Int] → StackExchangeT a m [SE Answer]
answersOnQuestions (intercalate ";" . map (toLazyText . decimal) → ids) = localState $ do
  uriPath <>= ["questions", ids, "answers"]
  uriQuery <>= [("site","stackoverflow")]
  AP.parse A.json `liftM` request >>= \case
    AP.Done _ s → map SE `liftM` (s ^! field "items")
    _           → fail ".users/{ids}/answers: Malformed JSON, cannot parse"


-- | <https://api.stackexchange.com/docs/top-user-answers-in-tags>
topUserAnswersInTags ∷ MonadIO m ⇒ Int → [Text] → StackExchangeT a m [SE Answer]
topUserAnswersInTags (toLazyText . decimal → id) (intercalate ";" → tags) = localState $ do
  uriPath <>= ["users", id, "tags", tags, "top-answers"]
  uriQuery <>= [("site","stackoverflow")]
  AP.parse A.json `liftM` request >>= \case
    AP.Done _ s → map SE `liftM` (s ^! field "items")
    _           → fail ".users/{id}/tags/{tags}/top-answers: Malformed JSON, cannot parse"
