{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , DeriveGeneric
  , DeriveDataTypeable
  #-}

module Data.URI.Auth where

import Data.URI.Auth.Host (URIAuthHost, parseURIAuthHost, printURIAuthHost)

import Prelude hiding (Maybe (..), maybe)
import qualified Prelude as P
import Data.Strict.Maybe (Maybe (..), maybe)
import Data.Text (Text)
import Data.Word (Word16)
import qualified Data.Text as T
import Data.Attoparsec.Text (Parser, char, decimal, takeWhile1, (<?>))
import Data.Monoid ((<>))
import Control.Monad (void)
import Control.Applicative (optional)

import Data.Data (Typeable)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof, listOf1, elements)


data URIAuth = URIAuth
  { uriAuthUser :: !(Maybe Text) -- ^ a designated user - @ssh://git\@github.com@ is @git@
  , uriAuthHost :: !URIAuthHost
  , uriAuthPort :: !(Maybe Word16) -- ^ the port, if it exists - @foobar.com:3000@ is @3000@ as a 16-bit unsigned int.
  } deriving (Show, Eq, Typeable, Generic)

instance Arbitrary URIAuth where
  arbitrary = URIAuth <$> arbitraryUser <*> arbitrary <*> arbitraryPort
    where
      arbitraryUser = oneof [pure Nothing, Just <$> arbitraryNonEmptyText]
      arbitraryPort = oneof [pure Nothing, Just <$> arbitrary]
      arbitraryNonEmptyText = T.pack <$> listOf1 (elements ['a' .. 'z'])


printURIAuth :: URIAuth -> Text
printURIAuth URIAuth{..} =
     maybe "" (<> "@") uriAuthUser
  <> printURIAuthHost uriAuthHost
  <> maybe "" (\p -> ":" <> T.pack (show p)) uriAuthPort


parseURIAuth :: Parser URIAuth
parseURIAuth =
  URIAuth <$> (toStrictMaybe <$> optional parseUser)
          <*> parseURIAuthHost
          <*> (toStrictMaybe <$> optional parsePort)
  where
    parseUser = do
      u <- takeWhile1 (\c -> c `notElem` ['@','.',':','/','?','&','=']) <?> "user value"
      void (char '@') <?> "user @"
      pure u
    parsePort = do
      void (char ':') <?> "port delimiter"
      decimal

    toStrictMaybe P.Nothing = Nothing
    toStrictMaybe (P.Just x) = Just x
