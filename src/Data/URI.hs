{-# LANGUAGE
    Strict
  #-}

module Data.URI where

import Data.URI.Auth (URIAuth)

import Prelude hiding (Maybe)
import Data.Strict.Maybe (Maybe (..))
import Data.Strict.Tuple (Pair (..))
import Data.Text (Text)
import Data.Vector (Vector)


data URI = URI
  { uriScheme    :: !(Maybe Text) -- ^ the scheme without the colon - @https://hackage.haskell.org/@ has a scheme of @https@
  , uriSlashes   :: !Bool -- ^ should slashes be present? - @https://hackage.haskell.org/@ is @True@
  , uriAuthority :: !URIAuth
  , uriPath      :: !(Vector Text) -- ^ slash-separated list - @https://hackage.haskell.org/foo@ is @["foo"]@
  , uriQuery     :: !(Vector (Pair Text (Maybe Text))) -- ^ list of key-value pairs - @https://hackage.haskell.org/?foo=bar&baz&qux=@ is
                                                       -- @[("foo", Just "bar"), ("baz", Nothing), ("qux", Just "")]@
  , uriFragment  :: !(Maybe Text) -- ^ uri suffix - @https://hackage.haskell.org/#some-header@ is @Just "some-header"@
  }
