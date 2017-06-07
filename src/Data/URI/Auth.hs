{-# LANGUAGE
    Strict
  , RecordWildCards
  , OverloadedStrings
  #-}

module Data.URI.Auth where

import Data.URI.Auth.Host (URIAuthHost, parseURIAuthHost)

import Prelude hiding (Maybe (..))
import Data.Strict.Maybe (Maybe (..), fromMaybe)
import Data.Text (Text)
import Data.Word (Word16)
import qualified Data.Text as T
import Data.Attoparsec.Text (Parser, many1, char, notChar, decimal, peekChar)
import Data.Monoid ((<>))
import Control.Applicative ((<|>))
import qualified GHC.Base



data URIAuth = URIAuth
  { uriAuthUser :: !(Maybe Text) -- ^ a designated user - @ssh://git\@github.com@ is @git@
  , uriAuthHost :: !URIAuthHost
  , uriAuthPort :: !(Maybe Word16) -- ^ the port, if it exists - @foobar.com:3000@ is @3000@ as a 16-bit unsigned int.
  }

instance Show URIAuth where
  show URIAuth{..} =
       fromMaybe "" ((\u -> T.unpack $ u <> "@") <$> uriAuthUser)
    ++ show uriAuthHost
    ++ fromMaybe "" ((\p -> ":" ++ show p) <$> uriAuthPort)


parseURIAuth :: Parser URIAuth
parseURIAuth =
  URIAuth <$> ((Just <$> parseUser) <|> pure Nothing)
          <*> parseURIAuthHost
          <*> ((Just <$> parsePort) <|> pure Nothing)
  where
    parseUser = do
      u <- many1 $ notChar '@'
      mC <- peekChar
      case mC of
        GHC.Base.Nothing -> fail "no user @ thing"
        _ -> do
          _ <- char '@'
          pure $ T.pack u
    parsePort = do
      _ <- char ':'
      decimal
