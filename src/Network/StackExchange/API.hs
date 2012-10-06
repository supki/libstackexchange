{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module Network.StackExchange.API
  ( users'ids'answers
  , filter'create
  ) where

import Control.Monad (liftM)

import           Control.Lens
import           Control.Monad.State (MonadState, get, put)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Monoid.Lens ((<>=))
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Lazy as AP
import           Data.ByteString.Lazy (ByteString)
import           Data.Text.Lazy (Text, intercalate)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)
import           Network.HTTP.Conduit (simpleHttp)

import Control.Monad.StackExchange (StackExchangeT)
import Data.Aeson.Lens (field)
import Network.StackExchange.URI
import Network.StackExchange.Types


-- | <https://api.stackexchange.com/docs/answers-on-users>
users'ids'answers ∷ MonadIO m ⇒ [Int] → StackExchangeT m [SE Answer]
users'ids'answers (intercalate ";" . map (toLazyText . decimal) → ids) = localState $ do
  uriPath <>= ["users", ids, "answers"]
  uriQuery <>= [("order","desc"),("sort","activity"),("site","stackoverflow")]
  AP.parse A.json `liftM` request >>= \case
    AP.Done _ s → map SE `liftM` (s ^! field "items")
    _           → fail ".users/{ids}/answers: Malformed JSON, cannot parse"


-- | <https://api.stackexchange.com/docs/create-filter>
filter'create ∷ MonadIO m ⇒ [Text] → [Text] → Text → StackExchangeT m (SE Filter)
filter'create (intercalate ";" → include) (intercalate ";" → exclude) base = localState $ do
  uriPath <>= ["filter", "create"]
  uriQuery <>= [("include",include),("exclude",exclude),("base",base)]
  AP.parse A.json `liftM` request >>= \case
    AP.Done _ s → return $ SE s
    _           → fail ".filter/create: Malformed JSON, cannot parse"


localState ∷ MonadState s m ⇒ m a → m a
localState m = get >>= \s → m >>= \v → put s >> return v
{-# INLINE localState #-}


request ∷ MonadIO m ⇒ StackExchangeT m ByteString
request = get >>= io . simpleHttp . render
{-# INLINE request #-}


io ∷ MonadIO m ⇒ IO a → m a
io = liftIO
{-# INLINE io #-}
