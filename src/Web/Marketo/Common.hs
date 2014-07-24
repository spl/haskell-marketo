module Web.Marketo.Common
  ( module Web.Marketo.Common
  , module Prelude
  , module Control.Applicative
  , module Control.Arrow
  , module Control.Exception
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Data.Aeson
  , module Data.Aeson.TH
  , module Data.Aeson.Types
  , module Data.ByteString
  , module Data.Char
  , module Data.Either
  , module Data.Foldable
  , module Data.Function
  , module Data.Maybe
  , module Data.Monoid
  , module Data.String
  , module Data.Text
  , module Data.Time.Clock
  , module Data.Time.Clock.POSIX
  , module Network.HTTP.Conduit
  , module Network.HTTP.Types
  ) where

--------------------------------------------------------------------------------

import Prelude hiding (mapM, sequence)
import Control.Applicative
import Control.Arrow
import Control.Exception hiding (throwIO)
import qualified Control.Exception
import Control.Monad hiding (forM, mapM, sequence)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.Foldable (foldMap)
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Language.Haskell.TH (Name, Q, Dec)
import Network.HTTP.Conduit hiding (parseUrl)
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types
import Network.Mime (MimeType)

#if MIN_VERSION_bytestring(0,10,2)
import Data.ByteString.Builder (toLazyByteString, intDec)
#else
import Data.ByteString.Lazy.Builder (toLazyByteString)
import Data.ByteString.Lazy.Builder.ASCII (intDec)
#endif

--------------------------------------------------------------------------------

(.$) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .$ g = \x y -> f (g x y)
infixr 9 .$

headOnly :: (a -> a) -> [a] -> [a]
headOnly _ []     = []
headOnly f (x:xs) = f x : xs

-- | Rewrite camelCase to lowercase with dashes
camelToDashed :: String -> String
camelToDashed []     = []
camelToDashed (c:cs) | isUpper c = '-' : toLower c : camelToDashed cs
                     | otherwise = c : camelToDashed cs

showBS:: ByteString -> String
showBS = TS.unpack . TS.decodeUtf8

intToBS :: Int -> ByteString
intToBS = BL.toStrict . toLazyByteString . intDec

intFromBS :: ByteString -> Maybe Int
intFromBS = readMay . showBS

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> Just x
  _   -> Nothing

--------------------------------------------------------------------------------

expireTime :: Int -> UTCTime -> UTCTime
expireTime sec tm = fromIntegral sec `addUTCTime` tm

--------------------------------------------------------------------------------

throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . Control.Exception.throwIO

--------------------------------------------------------------------------------

eitherToJSON :: (ToJSON a, ToJSON b) => Either a b -> Value
eitherToJSON = either toJSON toJSON

pairIf :: ToJSON a =>(a -> Bool) ->  Text -> a -> [Pair]
pairIf cond name value | cond value = [(name .= value)]
                       | otherwise  = []

-- | Parse alternatives: first try 'Right', then 'Left'
(.:^) :: (FromJSON a, FromJSON b) => Object -> Text -> Parser (Either a b)
obj .:^ key = Right <$> obj .: key <|> Left <$> obj .: key

-- | Parse an optional list: if the field is not there, the list is empty.
(.:*) :: FromJSON a => Object -> Text -> Parser [a]
obj .:* key = obj .: key <|> return []

--------------------------------------------------------------------------------

-- | Parse a list of /-intercalated items starting with the domain
parseUrl :: MonadIO m => [ByteString] -> m Request
parseUrl = liftIO . C.parseUrl . showBS . BS.intercalate "/" . (:) "https:/"

urlEncodeText :: Bool -> Text -> Text
urlEncodeText isQuery = TS.decodeUtf8 . urlEncode isQuery . TS.encodeUtf8

--------------------------------------------------------------------------------

setMethod :: Monad m => StdMethod -> Request -> m Request
setMethod m req = return $ req { method = renderStdMethod m }

--------------------------------------------------------------------------------

setQuery :: Monad m => Query -> Request -> m Request
setQuery q req = return $ req { queryString = renderQuery True q }

addQuery :: Monad m => Query -> Request -> m Request
addQuery q req = return $
  req { queryString = renderQuery True $ parseQuery (queryString req) ++ q }

addQueryItem :: Monad m => QueryItem -> Request -> m Request
addQueryItem qi req = return $
  req { queryString = renderQuery True $ qi : parseQuery (queryString req) }

lookupQ :: ByteString -> Query -> Maybe ByteString
lookupQ k = join . lookup k

--------------------------------------------------------------------------------

addHeader :: Monad m => Header -> Request -> m Request
addHeader hdr req = return $ req { requestHeaders = hdr : requestHeaders req }

findHeader :: Monad m => HeaderName -> Response b -> m ByteString
findHeader hdr rsp = case lookup hdr $ responseHeaders rsp of
  Nothing -> fail $  "findHeader: Can't find " ++ show hdr
                  ++ " in: " ++ show (responseHeaders rsp)
  Just val -> return val

--------------------------------------------------------------------------------

setContentType :: Monad m => ByteString -> Request -> m Request
setContentType contentType = addHeader (hContentType, contentType)

-- | Removes everything after the semicolon, if present.
--
-- Note: Taken from Yesod.Content in yesod-core.
simpleContentType :: ByteString -> ByteString
simpleContentType = fst . BS.breakByte 59 -- 59 == ;

mimeTypeContent :: Monad m => Response BL.ByteString -> m (MimeType, BL.ByteString)
mimeTypeContent rsp =
  (,) `liftM` liftM simpleContentType (findHeader hContentType rsp)
      `ap`    return (responseBody rsp)

--------------------------------------------------------------------------------

setUrlEncodedBody :: Monad m => [(ByteString, ByteString)] -> Request -> m Request
setUrlEncodedBody body req = return $ urlEncodedBody body req

setBody :: Monad m => StdMethod -> ByteString -> BL.ByteString -> Request -> m Request
setBody method contentType body req =
  return req { requestBody = RequestBodyLBS body } >>=
  setContentType contentType >>=
  setMethod method

--------------------------------------------------------------------------------

contentTypeJSON :: ByteString
contentTypeJSON = "application/json"

acceptJSON :: Monad m => Request -> m Request
acceptJSON = addHeader (hAccept, contentTypeJSON)

setJSONBody :: (Monad m, ToJSON a) => StdMethod -> a -> Request -> m Request
setJSONBody method obj = setBody method contentTypeJSON $ encode obj

fromJSONResponse :: (Monad m, FromJSON a) => String -> Response BL.ByteString -> m a
fromJSONResponse msg rsp = do
  (contentType, body) <- mimeTypeContent rsp
  if contentType == contentTypeJSON then
    maybe (fail $ msg ++ ": Can't decode JSON from response: " ++ show rsp)
          return
          (decode' body)
  else
    fail $ msg ++ ": unknown content type: " ++ show contentType

--------------------------------------------------------------------------------

deriveJSON_ :: Name -> Options -> Q [Dec]
deriveJSON_ = flip deriveJSON

deriveToJSON_ :: Name -> Options -> Q [Dec]
deriveToJSON_ = flip deriveToJSON

deriveFromJSON_ :: Name -> Options -> Q [Dec]
deriveFromJSON_ = flip deriveFromJSON

defaultEnumOptions :: Int -> Options
defaultEnumOptions n = defaultOptions
  { allNullaryToStringTag = True
  , constructorTagModifier = map toLower . drop n
  }

defaultRecordOptions :: Int -> Options
defaultRecordOptions n = defaultOptions
  { fieldLabelModifier = headOnly toLower . drop n
  }

dashedRecordOptions :: Int -> Options
dashedRecordOptions n = defaultOptions
  { fieldLabelModifier = camelToDashed . headOnly toLower . drop n
  }
