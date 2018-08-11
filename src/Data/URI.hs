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
import Data.Attoparsec.Text (Parser, char, string, sepBy, takeWhile, takeWhile1)
import Data.Char (isControl, isSpace)
import Control.Monad (void)
import Control.Applicative ((<|>), optional)

import Data.Data (Typeable)
import GHC.Generics (Generic)


data URI = URI
  { uriScheme    :: !(Maybe Text) -- ^ the scheme without the colon - @https://hackage.haskell.org/@ has a scheme of @https@
  , uriSlashes   :: !Bool -- ^ are the slashes present? - @https://hackage.haskell.org/@ is @True@
  , uriAuthority :: !URIAuth
  , uriPath      :: !(Maybe (Vector Text)) -- ^ slash-separated list - @https://hackage.haskell.org/foo@ is @["foo"]@
  , uriQuery     :: !(Vector (Pair Text (Maybe Text))) -- ^ list of key-value pairs - @https://hackage.haskell.org/?foo=bar&baz&qux=@ is
                                                       -- @[("foo", Just "bar"), ("baz", Nothing), ("qux", Just "")]@
  , uriFragment  :: !(Maybe Text) -- ^ uri suffix - @https://hackage.haskell.org/#some-header@ is @Just "some-header"@
  } deriving (Eq, Typeable, Generic)


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
      sch <- takeWhile1 (\c -> c `notElem` [':','/','@','.'])
      _ <- char ':'
      pure sch
    parseSlashes :: Parser Bool
    parseSlashes = do
      mS <- optional (string "//")
      case mS of
        P.Nothing -> pure False
        P.Just _  -> pure True
    parsePath :: Parser (Maybe (Vector Text))
    parsePath =
      let withRoot = do
            void (char '/')
            Just . V.fromList <$> parseChunkWithout ['/', '?', '=', '&', '#'] `sepBy` char '/'
          withoutRoot = pure Nothing
      in  withRoot <|> withoutRoot
    parseQuery :: Parser (Vector (Pair Text (Maybe Text)))
    parseQuery =
      ( do  void (char '?')
            let parse1 = do
                  k <- parseChunkWithout ['=','&','#']
                  mV <- ( Just <$> do void $ char '='
                                      parseChunkWithout ['&','#']
                        ) <|> pure Nothing
                  pure (k :!: mV)
            qs <- parse1 `sepBy` char '&'
            pure $ V.fromList qs
      ) <|> pure V.empty
    parseFragment :: Parser Text
    parseFragment = do
      void $ char '#'
      parseChunkWithout []
    parseChunkWithout :: [Char] -> Parser Text
    parseChunkWithout xs =
      takeWhile (\c -> not (isControl c || isSpace c) && c `notElem` xs)

    toStrictMaybe P.Nothing = Nothing
    toStrictMaybe (P.Just x) = Just x
