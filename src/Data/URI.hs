{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , DeriveDataTypeable
  #-}

module Data.URI where

import Data.URI.Auth (URIAuth, parseURIAuth, printURIAuth)

import Prelude hiding (Maybe (..), takeWhile, maybe)
import qualified Prelude as P
import Data.Strict.Maybe (Maybe (..), maybe)
import Data.Strict.Tuple (Pair (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Monoid ((<>))
import Data.Attoparsec.Text (Parser, char, string, sepBy, takeWhile, takeWhile1, (<?>))
import Data.Char (isControl, isSpace)
import Control.Monad (void, when)
import Control.Applicative ((<|>), optional)

import Data.Data (Typeable)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof, listOf, listOf1, elements)
import Test.QuickCheck.Instances ()


data URI = URI
  { uriScheme    :: !(Maybe Text) -- ^ the scheme without the colon - @https://hackage.haskell.org/@ has a scheme of @https@
  , uriSlashes   :: !Bool -- ^ are the slashes present? - @https://hackage.haskell.org/@ is @True@
  , uriAuthority :: !URIAuth
  , uriPath      :: !(Maybe (Vector Text)) -- ^ slash-separated list - @https://hackage.haskell.org/foo@ is @["foo"]@
  , uriQuery     :: !(Vector (Pair Text (Maybe Text))) -- ^ list of key-value pairs - @https://hackage.haskell.org/?foo=bar&baz&qux=@ is
                                                       -- @[("foo", Just "bar"), ("baz", Nothing), ("qux", Just "")]@
  , uriFragment  :: !(Maybe Text) -- ^ uri suffix - @https://hackage.haskell.org/#some-header@ is @Just "some-header"@
  } deriving (Show, Eq, Typeable, Generic)

instance Arbitrary URI where
  arbitrary = URI <$> arbitraryScheme
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitraryPath
                  <*> arbitraryQuery
                  <*> arbitraryScheme
    where
      arbitraryScheme = oneof [pure Nothing, Just <$> arbitraryNonEmptyText]
      arbitraryNonEmptyText = T.pack <$> listOf1 (elements ['a' .. 'z'])
      arbitraryPath =
        oneof [pure Nothing, Just . V.fromList <$> listOf1 arbitraryNonEmptyText]
      arbitraryQuery =
        V.fromList <$> listOf go
        where
          go = do
            a <- arbitraryNonEmptyText
            mb <- oneof [pure Nothing, Just <$> arbitraryNonEmptyText]
            pure (a :!: mb)



printURI :: URI -> Text
printURI URI{..} =
     maybe "" (<> ":") uriScheme
  <> (if uriSlashes then "//" else "")
  <> printURIAuth uriAuthority
  <> ( case uriPath of
         Just xs -> "/" <> T.intercalate "/" (V.toList xs)
         Nothing -> ""
     )
  <> ( if null uriQuery
          then ""
          else "?"
            <> T.intercalate "&"
                ( V.toList $
                  (\(k :!: mV) ->
                    let v' = case mV of
                              Nothing -> ""
                              Just v  -> "=" <> v
                    in  k <> v'
                  ) <$> uriQuery
                )
      )
  <> case uriFragment of
        Nothing -> ""
        Just f -> "#" <> f



parseURI :: Parser URI
parseURI =
  URI <$> (toStrictMaybe <$> optional parseScheme)
      <*> parseSlashes
      <*> parseURIAuth
      <*> parsePath
      <*> parseQuery
      <*> (toStrictMaybe <$> optional parseFragment)
  where
    parseScheme :: Parser Text
    parseScheme = do
      sch <- takeWhile1 (\c -> c `notElem` [':','/','@','.','[','*']) <?> "scheme value"
      when (sch == "localhost") (fail "can't be localhost")
      void (char ':') <?> "scheme colon"
      pure sch
    parseSlashes :: Parser Bool
    parseSlashes = do
      mS <- optional (string "//") <?> "slashes"
      case mS of
        P.Nothing -> pure False
        P.Just _  -> pure True
    parsePath :: Parser (Maybe (Vector Text))
    parsePath =
      let withRoot = do
            void (char '/') <?> "root"
            (Just . V.fromList <$> parseChunkWithout ['/', '?', '=', '&', '#'] `sepBy` char '/') <?> "path"
          withoutRoot = pure Nothing <?> "empty path"
      in  withRoot <|> withoutRoot
    parseQuery :: Parser (Vector (Pair Text (Maybe Text)))
    parseQuery =
      ( do  void (char '?') <?> "uri query init"
            let parse1 = do
                  k <- parseChunkWithout ['=','&','#'] <?> "uri query key"
                  mV <- ( Just <$> do void (char '=') <?> "uri query sep"
                                      parseChunkWithout ['&','#'] <?> "uri query val"
                        ) <|> pure Nothing
                  pure (k :!: mV)
            qs <- parse1 `sepBy` char '&' <?> "query params"
            pure (V.fromList qs)
      ) <|> pure V.empty
    parseFragment :: Parser Text
    parseFragment = do
      void (char '#') <?> "fragment init"
      parseChunkWithout [] <?> "fragment value"
    parseChunkWithout :: [Char] -> Parser Text
    parseChunkWithout xs =
      takeWhile (\c -> not (isControl c || isSpace c) && c `notElem` xs)

    toStrictMaybe P.Nothing = Nothing
    toStrictMaybe (P.Just x) = Just x
