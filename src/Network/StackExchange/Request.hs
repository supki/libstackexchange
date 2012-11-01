{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- | StackExchange API request manipulation routines
module Network.StackExchange.Request
  ( -- * Type
    Request(..), Auth(..), SE(..), Object(..)
    -- * Constructing requests
  , host, path, method, parse
  , query, token, key, site, filter, state, Scope(..), scope
  , client, redirectURI, secret, code
  ) where

import Data.Monoid (Monoid(..), (<>))
import GHC.TypeLits
import Prelude hiding (filter)

import           Control.Lens hiding (query)
import           Data.ByteString.Lazy (ByteString)
import           Data.Aeson (FromJSON)
import           Data.Aeson.Types (Value)
import           Data.Default (Default(..))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy.Builder.Int (decimal)


-- | Whether to use authentication at all. Currently isn't used
data Auth = RequireToken | Ready


-- | SE response type
data Object =
    AccessToken -- ^ <https://api.stackexchange.com/docs/types/access-token>
  | AccountMerge -- ^ <https://api.stackexchange.com/docs/types/account-merge>
  | Answer -- ^ <https://api.stackexchange.com/docs/types/answer>
  | Badge -- ^  <https://api.stackexchange.com/docs/types/badge>
  | Comment -- ^ <https://api.stackexchange.com/docs/types/comment>
  | Error -- ^ <https://api.stackexchange.com/docs/types/error>
  | Event -- ^ <https://api.stackexchange.com/docs/types/event>
  | Filter -- ^ <https://api.stackexchange.com/docs/types/filter>
  | InboxItem -- ^ <https://api.stackexchange.com/docs/types/inbox-item>
  | Info -- ^ <https://api.stackexchange.com/docs/types/info>
  | NetworkUser -- ^ <https://api.stackexchange.com/docs/types/network-user>
  | Notification -- ^ <https://api.stackexchange.com/docs/types/notification>
  | Post -- ^ <https://api.stackexchange.com/docs/types/post>
  | Privilege -- ^ <https://api.stackexchange.com/docs/types/privilege>
  | Question -- ^ <https://api.stackexchange.com/docs/types/question>
  | QuestionTimeline -- ^ <https://api.stackexchange.com/docs/types/question-timeline>
  | Reputation -- ^ <https://api.stackexchange.com/docs/types/reputation>
  | ReputationHistory -- ^ <https://api.stackexchange.com/docs/types/reputation-history>
  | Revision -- ^ <https://api.stackexchange.com/docs/types/revision>
  | Site -- ^ <https://api.stackexchange.com/docs/types/site>
  | SuggestedEdit -- ^ <https://api.stackexchange.com/docs/types/suggested-edit>
  | Tag -- ^ <https://api.stackexchange.com/docs/types/tag>
  | TagScore -- ^ <https://api.stackexchange.com/docs/types/tag-score>
  | TagSynonym -- ^ <https://api.stackexchange.com/docs/types/tag-synonym>
  | TagWiki -- ^ <https://api.stackexchange.com/docs/types/tag-wiki>
  | TopTag -- ^ <https://api.stackexchange.com/docs/types/top-tag>
  | User -- ^ <https://api.stackexchange.com/docs/types/user>
  | UserTimeline -- ^ <https://api.stackexchange.com/docs/types/user-timeline>
  | WritePermission -- ^ <https://api.stackexchange.com/docs/types/write-permission>


-- | SE response value wrapper
newtype SE (a ∷ Object) = SE { unSE ∷ Value } deriving (Show, FromJSON)


-- | StackExchange API Request data type.
--
-- @a@ is a phantom type showing whether authentication is enabled
--
-- @n@ is a phantom type dissallowing combination of
-- different API calls in one request
--
-- @r@ is a type of parsed API call result
data Request (a ∷ Auth) (n ∷ Symbol) r = Request
  { _host ∷ Text -- ^ API host link
  , _path ∷ Text -- ^ API call link
  , _method ∷ Text -- ^ API call method (GET/POST)
  , _query ∷ Map Text Text -- ^ API call query parameters
  , _parse ∷ Maybe (ByteString → r) -- ^ API call result parsing function
  }


makeLensesFor
  [ ("_host", "__host")
  , ("_path", "__path")
  , ("_method", "__method")
  , ("_query", "__query")
  , ("_parse", "__parse")
  ]
  ''Request


-- | Subject to monoid and idempotent laws, they all are checked in request test suite
instance Monoid (Request a n r) where
  mempty = Request
    { _host = mempty
    , _path = mempty
    , _method = mempty
    , _query = mempty
    , _parse = Nothing
    }
  l `mappend` r = Request
    { _host = _host $ if T.null $ _host r then l else r
    , _path = _path $ if T.null $ _path r then l else r
    , _method = _method $ if T.null $ _method r then l else r
    , _query = _query r <> _query l
    , _parse = _parse $ case _parse r of Just _ → r; Nothing → l
    }


-- | Useful if what's needed is immediate result parse
instance Functor (Request a n) where
  fmap f = over __parse (fmap (f .))
  {-# INLINE fmap #-}


-- | Default StackExchange API request, defines only host link
instance Default (Request a n r) where
  def = mempty % __host .~ "https://api.stackexchange.com/2.1" % __method .~ "GET"
  {-# INLINE def #-}


-- | Request defining only API call host
--
-- Primarily used in Auth, not intended for usage by library user
host ∷ Text → Request a n r
host p = mempty % __host .~ p
{-# INLINE host #-}


-- | Request defining only API call path
--
-- Primarily used in API call wrappers, not intended for usage by library user
path ∷ Text → Request a n r
path p = mempty % __path .~ p
{-# INLINE path #-}


-- | Request defining only call method
--
-- Primarily used in API call wrappers, not intended for usage by library user
method ∷ Text → Request a n r
method m = mempty % __method .~ m
{-# INLINE method #-}


-- | Request defining only API call result parsing function
--
-- Primarily used in API call wrappers, not intended for usage by library user
parse ∷ (ByteString → r) → Request a n r
parse f = mempty % __parse ?~ f
{-# INLINE parse #-}


-- | Request defining only API call query parameters
--
-- Rather low level interface. For more specific usage 'site',
-- 'filter', etc calls may be more convenient
--
--
-- Takes a list of (key, value) parameters such as @[("order", "asc"), ("sort", "rank")]@
query ∷ [(Text, Text)] → Request a n r
query q = mempty % __query .~ M.fromList q
{-# INLINE query #-}


-- | Convert token requiring Request into ready one
token ∷ Text → Request RequireToken n r → Request Ready n r
token t = over __query (M.insert "access_token" t)
{-# INLINE token #-}


-- | Request defining only App key
key ∷ Text → Request a n r
key s = mempty % __query .~ M.singleton "key" s
{-# INLINE key #-}


-- | Request defining only API call site query parameter
site ∷ Text → Request a n r
site s = mempty % __query .~ M.singleton "site" s
{-# INLINE site #-}


-- | Request defining only API call filter query parameter
filter ∷ Text → Request a n r
filter f = mempty % __query .~ M.singleton "filter" f
{-# INLINE filter #-}


-- | Request defining only API call state query parameter
state ∷ Text → Request a n r
state s = mempty % __query .~ M.singleton "state" s
{-# INLINE state #-}


-- | Scope defines permission granted for application by user
data Scope = ReadInbox | NoExpiry | WriteAccess | PrivateInfo


-- | Request defining only API call scope query parameter
scope ∷ [Scope] → Request a n r
scope ss = mempty % __query .~ (M.singleton "scope" $ scopie ss)
 where
  scopie xs = T.intercalate "," . flip map xs $ \case
    ReadInbox   → "read_inbox"
    NoExpiry    → "no_expiry"
    WriteAccess → "write_access"
    PrivateInfo → "private_info"


-- | Request defining only Authentication API call application id
--
-- Primarily used in Authentication API call wrappers, not intended for usage by library user
client ∷ Int → Request a n r
client (toLazyText . decimal → c) = mempty % __query .~ M.singleton "client_id" c
{-# INLINE client #-}


-- | Request defining only Authentication API call redirect url
--
-- Primarily used in Authentication API call wrappers, not intended for usage by library user
redirectURI ∷ Text → Request a n r
redirectURI r = mempty % __query .~ M.singleton "redirect_uri" r
{-# INLINE redirectURI #-}


-- | Request defining only Authentication API call application secret
--
-- Primarily used in Authentication API call wrappers, not intended for usage by library user
secret ∷ Text → Request a n r
secret c = mempty % __query .~ M.singleton "client_secret" c
{-# INLINE secret #-}


-- | Request defining only Authentication API call code
--
-- Primarily used in Authentication API call wrappers, not intended for usage by library user
code ∷ Text → Request a n r
code c = mempty % __query .~ M.singleton "code" c
{-# INLINE code #-}
