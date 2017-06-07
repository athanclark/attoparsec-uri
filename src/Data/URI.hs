{-# LANGUAGE
    Strict
  , OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , DeriveDataTypeable
  , StandaloneDeriving
  #-}

module Data.URI where

import Data.URI.Auth (URIAuth, parseURIAuth)

import Prelude hiding (Maybe (..))
import Data.Strict.Maybe (Maybe (..), fromMaybe)
import Data.Strict.Tuple (Pair (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Attoparsec.Text (Parser, many1, notChar, char, string, sepBy, satisfy, anyChar)
import Data.List (intercalate)
import Control.Applicative ((<|>), many)

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)


deriving instance (Data a, Data b) => Data (Pair a b)

data URI = URI
  { uriScheme    :: !(Maybe Text) -- ^ the scheme without the colon - @https://hackage.haskell.org/@ has a scheme of @https@
  , uriSlashes   :: !Bool -- ^ are the slashes present? - @https://hackage.haskell.org/@ is @True@
  , uriAuthority :: !URIAuth
  , uriPath      :: !(Vector Text) -- ^ slash-separated list - @https://hackage.haskell.org/foo@ is @["foo"]@
  , uriQuery     :: !(Vector (Pair Text (Maybe Text))) -- ^ list of key-value pairs - @https://hackage.haskell.org/?foo=bar&baz&qux=@ is
                                                       -- @[("foo", Just "bar"), ("baz", Nothing), ("qux", Just "")]@
  , uriFragment  :: !(Maybe Text) -- ^ uri suffix - @https://hackage.haskell.org/#some-header@ is @Just "some-header"@
  } deriving (Eq, Data, Typeable, Generic)


instance Show URI where
  show URI{..} =
       fromMaybe "" ((\s -> T.unpack s ++ ":") <$> uriScheme)
    ++ (if uriSlashes then "//" else "")
    ++ show uriAuthority
    ++ "/" ++ intercalate "/" (V.toList $ T.unpack <$> uriPath)
    ++ ( if null uriQuery
           then ""
           else "?" ++ intercalate "&" (V.toList $ (\(k :!: mV) -> T.unpack k ++ ( case mV of
                                                                                     Nothing -> ""
                                                                                     Just v  -> "=" ++ T.unpack v)) <$> uriQuery)
       )
    ++ case uriFragment of
         Nothing -> ""
         Just f -> "#" ++ T.unpack f



parseURI :: Parser URI
parseURI =
  URI <$> ((Just <$> parseScheme) <|> pure Nothing)
      <*> parseSlashes
      <*> parseURIAuth
      <*> parsePath
      <*> parseQuery
      <*> ((Just <$> parseFragment) <|> pure Nothing)
  where
    parseScheme = do
      sch <- many1 (satisfy $ \c -> all (c /=) [':','/','@','.'])
      _ <- char ':'
      pure (T.pack sch)
    parseSlashes = do
      mS <- (Just <$> string "//") <|> pure Nothing
      case mS of
        Nothing -> pure False
        Just _  -> pure True
    parsePath =
      ( do  _ <- char '/'
            V.fromList <$> (T.pack <$> many (satisfy $ \c -> all (c /=) ['/', '?', '=', '&', '#'])) `sepBy` (char '/')
      ) <|> pure V.empty
    parseQuery :: Parser (Vector (Pair Text (Maybe Text)))
    parseQuery =
      ( do  _ <- char '?'
            let parse1 = do
                  k <- many (satisfy (\c -> all (c /=) ['=', '&', '#']))
                  mV <- ( Just <$> do _ <- char '='
                                      v <- many (satisfy $ \c -> c /= '&' && c /= '#')
                                      pure (T.pack v)
                        ) <|> ( pure Nothing
                              )
                  pure (T.pack k :!: mV)
            qs <- parse1 `sepBy` (char '&')
            pure $ V.fromList qs
      ) <|> pure V.empty
    parseFragment = do
      _ <- char '#'
      T.pack <$> many anyChar
