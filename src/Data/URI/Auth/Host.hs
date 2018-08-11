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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text (Parser, char, sepBy1, takeWhile1)
import Data.Attoparsec.IP (ipv4, ipv6)
import Control.Applicative ((<|>))
import Net.Types (IPv4, IPv6)
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6

import Data.Data (Typeable)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof, resize, listOf1)
import Test.QuickCheck.Instances ()




data URIAuthHost
  = Glob
  | IPv4 !IPv4
  | IPv6 !IPv6
  | Localhost
  | -- | @Host ["foo","bar"] "com"@ represents @foo.bar.com@
    Host
      { uriAuthHostName   :: !(Vector Text)
      , uriAuthHostSuffix :: !Text
      } deriving (Show, Eq, Typeable, Generic)

instance Arbitrary URIAuthHost where
  arbitrary = oneof
    [ pure Glob
    , IPv4 <$> arbitraryIPv4
    , IPv6 <$> arbitraryIPv6
    , pure Localhost
    , Host <$> arbitraryNonEmptyVector arbitraryNonEmptyText
           <*> arbitraryNonEmptyText
    ]
    where
      arbitraryNonEmptyText =
        resize 2 arbitrary
      arbitraryNonEmptyVector x = V.fromList <$> listOf1 x
      arbitraryIPv4 =
        IPv4.ipv4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      arbitraryIPv6 =
        IPv6.ipv6 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                  <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

printURIAuthHost :: URIAuthHost -> Text
printURIAuthHost x = case x of
  Glob -> "*"
  IPv4 l4 -> IPv4.encode l4
  IPv6 r6 -> IPv6.encode r6
  Localhost -> "localhost"
  Host ns c -> T.intercalate "." (V.toList (ns `V.snoc` c))


parseURIAuthHost :: Parser URIAuthHost
parseURIAuthHost =
      (IPv4 <$> ipv4)
  <|> (IPv6 <$> ipv6)
  <|> parseHost
  where
    parseHost :: Parser URIAuthHost
    parseHost = do
      xss@(x:xs) <- takeWhile1 (\c -> c `notElem` ['.',':','/','?']) `sepBy1` char '.'
      if null xs
        then case () of
               _ | x == "localhost" -> pure Localhost
                 | x == "*" -> pure Glob
                 | otherwise -> fail ("Only one term parsed: " ++ show xss)
        else let xss' :: Vector Text
                 xss' = V.fromList xss
                 unsnoc :: Vector a -> (Vector a, a)
                 unsnoc x' =
                   let (fs,l) = V.splitAt (V.length x' - 1) x'
                   in  (fs, l V.! 0)
                 (ns,c) = unsnoc xss'
             in  pure (Host ns c)
