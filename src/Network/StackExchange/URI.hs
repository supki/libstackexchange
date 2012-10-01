{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | StackExchange API server URI manipulation routines.
module Network.StackExchange.URI
  ( -- * Types
    URI
    -- * default URI
  , stackexchange
    -- * URI lenses
  , uriHost
  , uriPath
  , uriQuery
    -- ~ URI rendering
  , render
  ) where

import Data.Foldable (foldMap)
import Data.Monoid ((<>), mconcat)

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Control.Lens


-- | Main URI data type
data URI = URI
  { _uriHost ∷ Text -- ^ StackExchange API host name
  , _uriPath ∷ [Text] -- ^ StackExchange API method as URI path
  , _uriQuery ∷ [(Text, Text)] -- ^ StackExchange API method parameters
  }


makeLenses ''URI


{- Waiting for Edward Kmett to give us an option to generate lenses without type signatures
 -
 --- | StackExchange API host name lens
 -uriHost ∷ Lens URI URI String String
 -
 -
 --- | StackExchange API method as URI path lens
 -uriPath ∷ Lens URI URI [String] [String]
 -
 -
 --- | StackExchange API method parameters lens
 -uriQuery ∷ Lens URI URI [(String, String)] [(String, String)]
 -}


stackexchange ∷ URI
stackexchange = URI
  { _uriHost = "https://api.stackexchange.com"
  , _uriPath = ["2.1"]
  , _uriQuery = []
  }


-- | To use URI in http-conduit we need to convert it to a string
render ∷ URI → String
render s = T.unpack . mconcat $ [s^.uriHost, "/", s^.uriPath.to pathie, "?", s^.uriQuery.to argie]
 where
  pathie = foldMap ("/" <>)
  argie = foldMap (\(a, b) → mconcat [a, "=", b, "&"])
