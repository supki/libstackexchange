{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module Network.StackExchange.API
  ( users'ids'answers
  , filter'create
  ) where

import Control.Applicative (empty)
import Control.Exception (throwIO)
import Control.Monad (liftM)

import           Control.Monad.State (MonadState, get, put)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Monoid.Lens ((<>=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.Lazy as AP
import           Data.ByteString.Lazy (ByteString)
import           Data.Text.Lazy (Text, intercalate)
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)
import           Network.HTTP.Conduit (simpleHttp)

import Control.Monad.StackExchange (StackExchangeT)
import Network.StackExchange.URI
import Network.StackExchange.Types


-- | <https://api.stackexchange.com/docs/answers-on-users>
users'ids'answers ∷ MonadIO m ⇒ [Int] → StackExchangeT m [SE Answer]
users'ids'answers (intercalate ";" . map (toLazyText . decimal) → ids) = localState $ do
  uriPath <>= ["users", ids, "answers"]
  uriQuery <>= [("order","desc"),("sort","activity"),("site","stackoverflow")]
  AP.parse A.json `liftM` request >>= \case
    AP.Done _ s → case A.parse p s of
      A.Success v → return $ map SE v
      _ → throwSE ".users/{ids}/answers: Incorrect JSON, cannot parse as a list of answers"
    _ → throwSE ".users/{ids}/answers: Malformed JSON, cannot parse"
 where
  p (A.Object o) = o A..: "items"
  p _ = empty


-- | <https://api.stackexchange.com/docs/create-filter>
filter'create ∷ MonadIO m ⇒ [Text] → [Text] → Text → StackExchangeT m (SE Filter)
filter'create (intercalate ";" → include) (intercalate ";" → exclude) base = localState $ do
  uriPath <>= ["filter", "create"]
  uriQuery <>= [("include",include),("exclude",exclude),("base",base)]
  AP.parse A.json `liftM` request >>= \case
    AP.Done _ s → return $ SE s
    _ → throwSE ".filter/create: Malformed JSON, cannot parse"


localState ∷ MonadState s m ⇒ m a → m a
localState m = get >>= \s → m >>= \v → put s >> return v
{-# INLINE localState #-}


request ∷ MonadIO m ⇒ StackExchangeT m ByteString
request = get >>= io . simpleHttp . render
{-# INLINE request #-}


throwSE ∷ MonadIO m ⇒ String → m a
throwSE = io . throwIO . SEException . ("libstackexchange" ++)
{-# INLINE throwSE #-}


io ∷ MonadIO m ⇒ IO a → m a
io = liftIO
{-# INLINE io #-}
