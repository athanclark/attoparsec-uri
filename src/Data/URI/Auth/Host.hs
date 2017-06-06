{-# LANGUAGE
    Strict
  #-}

module Data.URI.Auth.Host where

import Prelude hiding (Either)

import Data.IP (IPv4, IPv6)
import Data.Strict (Either (..))
import Data.Vector (Vector)
import Data.Text (Text)



data URIAuthHost
  = IP !(Either IPv4 IPv6)
  | -- | @Host ["foo","bar"] "com"@ represents @foo.bar.com@
    Host
      { uriAuthHostName   :: !(Vector Text)
      , uriAuthHostSuffix :: !Text
      }
