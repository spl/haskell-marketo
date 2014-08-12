module Web.Marketo.SOAP.Internal where

--------------------------------------------------------------------------------

import Web.Marketo.SOAP.Common
import Network.SOAP.Parsing.Stream
import Text.XML.Writer
-- import Data.XML.Types
import qualified Crypto.Hash.SHA1 as SHA1
import Crypto.MAC.HMAC (hmac)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Time.Format as Time
import System.Locale (defaultTimeLocale)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base16 as B16

--------------------------------------------------------------------------------

-- | Format time as ISO 8601
time :: UTCTime -> String
time = Time.formatTime defaultTimeLocale "%FT%T%QZ"
-- time = BB.stringUtf8 . Time.formatTime defaultTimeLocale "%FT%T%Q%z"

toStrictByteString :: Builder -> ByteString
toStrictByteString = BL.toStrict . BB.toLazyByteString

data RequestHeader = RequestHeader
  { reqHdrUserId    :: !ByteString
  , reqHdrSignature :: !ByteString
  , reqHdrTimestamp :: !String
  }

mkRequestHeader :: ByteString -> ByteString -> IO RequestHeader
mkRequestHeader mktowsUserId encryptionKey = do
  timestamp <- Time.formatTime defaultTimeLocale "%FT%T%QZ" <$> getCurrentTime
  let message = toStrictByteString $ BB.stringUtf8 timestamp <> BB.byteString mktowsUserId
  return $ RequestHeader
    { reqHdrUserId = mktowsUserId
    , reqHdrSignature = B16.encode $ hmac SHA1.hash 64 encryptionKey message
    , reqHdrTimestamp = timestamp
    }

instance ToXML RequestHeader where
  toXML RequestHeader {..} = 
        element "{http://www.marketo.com/mktows/}AuthenticationHeader" $ do
          element "mktowsUserId" $ T.decodeUtf8 reqHdrUserId
          element "requestSignature" $ T.decodeUtf8 reqHdrSignature
          element "requestTimestamp" $ T.pack reqHdrTimestamp

main :: IO ()
main = do
  t <- initTransportWith tlsManagerSettings
    "https://793-GRT-658.mktoapi.com/soap/mktows/2_5" traceRequest traceBody
  header <- mkRequestHeader "doxiqdev1_7327231453CFD901ACCEA8"
                            "29101128355558475533CCFFDD990023AACCDEA50646"
  let body =
        element "{http://www.marketo.com/mktows/}paramsGetLead" $ do
          element "leadKey" $ do
            element "keyType" ("IDNUM" :: Text)
            element "keyValue" (8 :: Int)
  let parser = StreamParser (readTag "successGetLead" :: Parser Text)
  response <- invokeWS t "" header body parser
  print response
  return ()

