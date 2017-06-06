{-# LANGUAGE
    Strict
  #-}

module Data.URI.Auth where

import Data.URI.Auth.Host (URIAuthHost)

import Prelude hiding (Maybe)
import Data.Strict.Maybe (Maybe (..))
import Data.Text (Text)
import Data.Word (Word16)



data URIAuth = URIAuth
  { uriAuthUser :: !(Maybe Text) -- ^ a designated user - @ssh://git\@github.com@ is @git@
  , uriAuthHost :: !URIAuthHost
  , uriAuthPort :: !(Maybe Word16) -- ^ the port, if it exists - @foobar.com:3000@ is @3000@ as a 16-bit unsigned int.
  }
