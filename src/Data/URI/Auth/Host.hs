{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , DeriveDataTypeable
  , OverloadedStrings
  #-}

module Data.URI.Auth.Host where

import Prelude hiding (Either (..))

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Attoparsec.Text (Parser, char, sepBy1, takeWhile1)
import Data.Attoparsec.IP (ipv4, ipv6)
import Data.List (intercalate)
import Control.Applicative ((<|>))
import Net.Types (IPv4, IPv6)
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6

import Data.Data (Typeable)
import GHC.Generics (Generic)




data URIAuthHost
  = IPv4 !IPv4
  | IPv6 !IPv6
  | Localhost
  | -- | @Host ["foo","bar"] "com"@ represents @foo.bar.com@
    Host
      { uriAuthHostName   :: !(Vector Text)
      , uriAuthHostSuffix :: !Text
      } deriving (Eq, Typeable, Generic)

instance Show URIAuthHost where
  show (IPv4 l4) = unpack (IPv4.encode l4)
  show (IPv6 r6) = unpack (IPv6.encode r6)
  show Localhost = "localhost"
  show (Host ns c) = intercalate "." $ V.toList $ T.unpack <$> ns `V.snoc` c


parseURIAuthHost :: Parser URIAuthHost
parseURIAuthHost =
      (IPv4 <$> ipv4)
  <|> (IPv6 <$> ipv6)
  <|> parseHost
  where
    parseHost :: Parser URIAuthHost
    parseHost = do
      xss@(x:xs) <- takeWhile1 (\c -> all (c /=) ['.',':','/','?']) `sepBy1` char '.'
      if null xs
        then if x == "localhost"
             then pure Localhost
             else fail $ "Only one term parsed: " ++ show xss
        else let xss' :: Vector Text
                 xss' = V.fromList xss
                 unsnoc :: Vector a -> (Vector a, a)
                 unsnoc x =
                   let (fs,l) = V.splitAt (V.length x - 1) x
                   in  (fs, l V.! 0)
                 (ns,c) = unsnoc xss'
             in  pure (Host ns c)
