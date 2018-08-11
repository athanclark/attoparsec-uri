import Data.URI.Auth.Host (printURIAuthHost, parseURIAuthHost)
import Data.URI.Auth (printURIAuth, parseURIAuth)
import Data.URI (printURI, parseURI)

import Data.Text (Text)
import Data.Attoparsec.Text (Parser, parseOnly)
import Test.Tasty (testGroup, defaultMain)
import qualified Test.Tasty.QuickCheck as Q
import Test.QuickCheck.Property (Result, succeeded, failed)



main :: IO ()
main = defaultMain $ testGroup "URI tests"
  [ Q.testProperty "URIAuthHost" (parsePrintIso printURIAuthHost parseURIAuthHost)
  , Q.testProperty "URIAuth" (parsePrintIso printURIAuth parseURIAuth)
  , Q.testProperty "URI" (parsePrintIso printURI parseURI)
  ]


parsePrintIso :: Eq a => (a -> Text) -> Parser a -> a -> Result
parsePrintIso print' parser x = case parseOnly parser (print' x) of
  Left _ -> failed
  Right y
    | y == x -> succeeded
    | otherwise -> failed
