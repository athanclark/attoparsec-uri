{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , DeriveGeneric
  , DeriveDataTypeable
  , StandaloneDeriving
  #-}

module Data.URI.Auth where

import Data.URI.Auth.Host (URIAuthHost, parseURIAuthHost)

import Prelude hiding (Maybe (..))
import qualified Prelude as P
import Data.Strict.Maybe (Maybe (..), fromMaybe)
import Data.Text (Text)
import Data.Word (Word16)
import qualified Data.Text as T
import Data.Attoparsec.Text (Parser, char, decimal, peekChar, takeWhile1)
import Data.Monoid ((<>))
import Control.Monad (void)
import Control.Applicative (optional)

import Data.Data (Typeable)
import GHC.Generics (Generic)


data URIAuth = URIAuth
  { uriAuthUser :: !(Maybe Text) -- ^ a designated user - @ssh://git\@github.com@ is @git@
  , uriAuthHost :: !URIAuthHost
  , uriAuthPort :: !(Maybe Word16) -- ^ the port, if it exists - @foobar.com:3000@ is @3000@ as a 16-bit unsigned int.
  } deriving (Eq, Typeable, Generic)

instance Show URIAuth where
  show URIAuth{..} =
       fromMaybe "" ((\u -> T.unpack $ u <> "@") <$> uriAuthUser)
    ++ show uriAuthHost
    ++ fromMaybe "" ((\p -> ":" ++ show p) <$> uriAuthPort)


parseURIAuth :: Parser URIAuth
parseURIAuth =
  URIAuth <$> (toStrictMaybe <$> optional parseUser)
          <*> parseURIAuthHost
          <*> (toStrictMaybe <$> optional parsePort)
  where
    parseUser = do
      u <- takeWhile1 (\c -> all (c /=) ['@','.',':','/','?','&','='])
      mC <- peekChar
      case mC of
        P.Nothing -> fail "no user @ thing"
        _ -> do
          void $ char '@'
          pure u
    parsePort = do
      void $ char ':'
      decimal

    toStrictMaybe P.Nothing = Nothing
    toStrictMaybe (P.Just x) = Just x
