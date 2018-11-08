import Data.URI.Auth.Host (printURIAuthHost, parseURIAuthHost)
import Data.URI.Auth (printURIAuth, parseURIAuth)
import Data.URI (printURI, parseURI)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text (Parser, parseOnly)
import Test.Tasty (testGroup, defaultMain)
import qualified Test.Tasty.QuickCheck as Q
import Test.QuickCheck.Property (Result, succeeded, failed)
import System.IO.Unsafe (unsafePerformIO)



main :: IO ()
main = defaultMain $ testGroup "URI tests"
  [ Q.testProperty "URIAuthHost" (parsePrintIso printURIAuthHost parseURIAuthHost)
  , Q.testProperty "URIAuth" (parsePrintIso printURIAuth parseURIAuth)
  , Q.testProperty "URI" (parsePrintIso printURI parseURI)
  ]


parsePrintIso :: Eq a => Show a => (a -> Text) -> Parser a -> a -> Result
parsePrintIso print' parser x = case parseOnly parser (print' x) of
  Left e ->
    let go = unsafePerformIO $ do
          putStr "Original value: "
          print x
          putStr "Printed Text: "
          putStrLn $ T.unpack $ print' x
          putStr "Parse Error: "
          print e
    in  seq go failed
  Right y
    | y == x -> succeeded
    | otherwise ->
      let go = unsafePerformIO $ do
            putStr "Original value: "
            print x
            putStr "Printed Text: "
            putStrLn $ T.unpack $ print' x
            putStr "Parsed Value: "
            print y
      in  seq go failed
