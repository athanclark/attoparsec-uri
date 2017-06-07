{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , DeriveDataTypeable
  #-}

module Data.URI.Auth.Host where

import Prelude hiding (Either (..))

import Data.Strict (Either (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.NTuple (NTuple, _1, _2, _3, _4, _5, _6, _7, _8)
import qualified Data.NTuple as NTuple
import Data.Word (Word8, Word16)
import Data.Attoparsec.Text (Parser, peekChar', decimal, hexadecimal, char, notChar, sepBy1, many1, satisfy)
import Data.Char (isDigit, isHexDigit)
import Data.Bits ((.&.), shiftR)
import Data.List (intercalate)
import Control.Applicative ((<|>))
import Text.Read (readMaybe)
import Text.Bytedump (hexString)

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)




type IPv4 = NTuple 4 Word8

parseIPv4 :: Parser IPv4
parseIPv4 = do
  a <- parseOctet
  _ <- char '.'
  b <- parseOctet
  _ <- char '.'
  c <- parseOctet
  _ <- char '.'
  d <- parseOctet
  pure $ NTuple.incl _4 d
       . NTuple.incl _3 c
       . NTuple.incl _2 b
       . NTuple.incl _1 a
       $ NTuple.empty
  where
    parseOctet :: Parser Word8
    parseOctet = decimal

showIPv4 :: IPv4 -> String
showIPv4 xs =
  intercalate "." $ V.toList $ show <$> NTuple.toVector xs


type IPv6 = NTuple 8 Word16

parseIPv6 :: Parser IPv6
parseIPv6 = do
  a <- parseDiOctet
  _ <- char ':'
  b <- parseDiOctet
  _ <- char ':'
  c <- parseDiOctet
  _ <- char ':'
  d <- parseDiOctet
  _ <- char ':'
  let soFar = NTuple.incl _4 d
            . NTuple.incl _3 c
            . NTuple.incl _2 b
            . NTuple.incl _1 a
            $ NTuple.empty
  q <- peekChar'
  if q == ':'
    then pure $ NTuple.incl _8 0
              . NTuple.incl _7 0
              . NTuple.incl _6 0
              . NTuple.incl _5 0
              $ soFar
    else do
      e <- parseDiOctet
      _ <- char ':'
      f <- parseDiOctet
      _ <- char ':'
      g <- parseDiOctet
      _ <- char ':'
      h <- parseDiOctet
      pure $ NTuple.incl _8 h
           . NTuple.incl _7 g
           . NTuple.incl _6 f
           . NTuple.incl _5 e
           $ soFar
  where
    parseDiOctet :: Parser Word16
    parseDiOctet = hexadecimal


showIPv6 :: IPv6 -> String
showIPv6 xs =
  let xs'@(a:b:c:d:qs) = V.toList $ NTuple.toVector xs
  in  if all (== 0) qs
        then intercalate ":" (showWord16 <$> [a,b,c,d]) ++ "::"
        else intercalate ":" (showWord16 <$> xs')
  where
    showWord16 :: Word16 -> String
    showWord16 x =
      let (l,r) = breakWord16 x
      in  hexString l ++ hexString r
      where
        breakWord16 :: Word16 -> (Word8, Word8)
        breakWord16 x = ( fromIntegral $ (x .&. 0xFF00) `shiftR` 8
                        , fromIntegral $ x .&. 0xFF
                        )



data URIAuthHost
  = IPv4 !IPv4
  | IPv6 !IPv6
  | Localhost
  | -- | @Host ["foo","bar"] "com"@ represents @foo.bar.com@
    Host
      { uriAuthHostName   :: !(Vector Text)
      , uriAuthHostSuffix :: !Text
      } deriving (Eq, Data, Typeable, Generic)

instance Show URIAuthHost where
  show (IPv4 l4) = showIPv4 l4
  show (IPv6 r6) = showIPv6 r6
  show Localhost = "localhost"
  show (Host ns c) = intercalate "." $ V.toList $ T.unpack <$> ns `V.snoc` c


parseURIAuthHost :: Parser URIAuthHost
parseURIAuthHost =
      (IPv4 <$> parseIPv4)
  <|> (IPv6 <$> parseIPv6)
  <|> parseHost
  where
    parseHost :: Parser URIAuthHost
    parseHost = do
      xss@(x:xs) <- many1 (satisfy $ \c -> all (c /=) ['.',':','/','?']) `sepBy1` char '.'
      if null xs
        then if x == "localhost"
             then pure Localhost
             else fail "Only one term parsed"
        else let xss' :: Vector Text
                 xss' = T.pack <$> V.fromList xss
                 unsnoc :: Vector a -> (Vector a, a)
                 unsnoc x =
                   let (fs,l) = V.splitAt (V.length x - 1) x
                   in  (fs, l V.! 0)
                 (ns,c) = unsnoc xss'
             in  pure (uncurry Host $ unsnoc xss')
