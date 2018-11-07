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
import Data.Attoparsec.Text ( Parser, char, decimal, takeWhile1, (<?>)
                            , satisfy, peekChar')
import Data.Monoid ((<>))
import Control.Monad (void)
import Control.Applicative (optional)

import Data.Data (Typeable)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof, listOf1, elements)


data URIAuth = URIAuth
  { uriAuthUser     :: !(Maybe Text) -- ^ a designated user - @ssh://git\@github.com@ is @git@
  , uriAuthPassword :: !(Maybe Text) -- ^ a designated password (this field is depricated in RFC 3986, passwords with an at-character will not parse) - @https://user:password\@github.com@ is @password@
  , uriAuthHost     :: !URIAuthHost
  , uriAuthPort     :: !(Maybe Word16) -- ^ the port, if it exists - @foobar.com:3000@ is @3000@ as a 16-bit unsigned int.
  } deriving (Show, Eq, Typeable, Generic)

instance Arbitrary URIAuth where
  arbitrary = URIAuth <$> arbitraryUser <*> arbitraryPassword <*> arbitrary <*> arbitraryPort
    where
      arbitraryUser = oneof [pure Nothing, Just <$> arbitraryNonEmptyText]
      arbitraryPassword = oneof [pure Nothing, Just <$> arbitraryNonEmptyText]
      arbitraryPort = oneof [pure Nothing, Just <$> arbitrary]
      arbitraryNonEmptyText = T.pack <$> listOf1 (elements ['a' .. 'z'])


-- | Prints the URI auth but omits the password even if present.
printURIAuth :: URIAuth -> Text
printURIAuth URIAuth{..} =
     maybe "" (<> "@") uriAuthUser
  <> printURIAuthHost uriAuthHost
  <> maybe "" (\p -> ":" <> T.pack (show p)) uriAuthPort


parseURIAuth :: Parser URIAuth
parseURIAuth =
  do  u_sep <- optional parseUser
      p <- if fmap snd u_sep == P.Just ':'
             then toStrictMaybe <$> optional parsePassword
             else pure Nothing
      URIAuth (toStrictMaybe (fst <$> u_sep)) p
              <$> parseURIAuthHost
              <*> (toStrictMaybe <$> optional parsePort)
  where
    parseUser = do
      u <- takeWhile1 (\c -> c `notElem` ['@','.',':','/','?','&','=']) <?> "user value"
      c <- peekChar' <?> "user '@' or ':'"
      void (satisfy (`elem` ['@',':']) <?> "user '@' or ':'")
      pure (u,c)
    parsePassword= do
      p <- takeWhile1 (\c -> c `notElem` ['@']) <?> "password value"
      void (char '@') <?> "user @"
      pure p
    parsePort = do
      void (char ':') <?> "port delimiter"
      decimal

    toStrictMaybe P.Nothing = Nothing
    toStrictMaybe (P.Just x) = Just x
