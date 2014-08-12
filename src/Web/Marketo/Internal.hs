module Web.Marketo.Internal where

--------------------------------------------------------------------------------

import Web.Marketo.Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import qualified Data.Text.Encoding as TS
import qualified Network.HTTP.Conduit as C

--------------------------------------------------------------------------------

-- | This function contains the general flow of a function requesting something
-- from HubSpot and processing the response. It handles setting the acccess
-- token in the query, setting the appropriate Accept type, and checking the
-- HTTP status code in the response.
apiRequest
  :: MonadIO m
  => [ByteString]                     -- ^ Path segments, intercalated with '/'
  -> (Request -> m Request)           -- ^ Modify request
  -> (Response BL.ByteString -> m a)  -- ^ Process response
  -> AppId                            -- ^ Client app ID
  -> ApiAccess                        -- ^ For the REST API domain
  -> Auth                             -- ^ For the access token
  -> Manager
  -> m a
apiRequest pathSegments modifyRequest processResponse appId ApiAccess {..} Auth {..} mgr =
  parseUrl appId pathSegments
  >>= acceptJSON
  >>= addHeader (hAuthorization, "Bearer " <> authAccessToken)
  >>= modifyRequest
{-
  -- Debugging code for request body
  >>= (modifyRequest >=> (\r -> do
    liftIO (putStrLn $ show r)
    let RequestBodyLBS body = requestBody r
    liftIO (putStrLn $ "Request body: " ++ show body)
    return r))
-}
  >>= flip httpLbs mgr
  >>= processResponse

--------------------------------------------------------------------------------

-- | Email address
type Email = Text

--------------------------------------------------------------------------------

-- | App ID
--
-- This is the identifier for a Marketo client application. It is called the
-- Munchkin account ID in some places, but it is also used as the subdomain for
-- the REST API endpoint (e.g. the "100-AEK-913" in
-- "https://100-AEK-913.mktorest.com/rest").
--
-- Note: Use the 'IsString' instance (e.g. with @OverloadedStrings@) for easy
-- construction.
newtype AppId = AppId { fromAppId :: ByteString }
  deriving IsString

instance Show AppId where
  show = showBS . fromAppId

instance ToJSON AppId where
  toJSON = String . TS.decodeUtf8 . fromAppId

instance FromJSON AppId where
  parseJSON = fmap (AppId . TS.encodeUtf8) . parseJSON

-- | Parse a list of /-intercalated items starting with the domain
parseUrl :: MonadIO m => AppId -> [ByteString] -> m Request
parseUrl appId segments = liftIO $ C.parseUrl $ showBS $ mconcat
  ["https://", fromAppId appId, ".mktorest.com/", BS.intercalate "/" segments]

--------------------------------------------------------------------------------

-- | API access information provided by a Marketo administrator who enables this
-- code to access the Marketo API for their data.
data ApiAccess = ApiAccess
  { apiClientId     :: !ByteString  -- ^ OAuth client ID
  , apiClientSecret :: !ByteString  -- ^ OAuth client secret
  }
  deriving Show

instance FromJSON ApiAccess where
  parseJSON = withObject "ApiAccess" $ \o ->
    ApiAccess <$> o .:$ "client_id"
              <*> o .:$ "client_secret"

instance ToJSON ApiAccess where
  toJSON ApiAccess {..} = object
    [ "client_id"     .=$ apiClientId
    , "client_secret" .=$ apiClientSecret
    ]

--------------------------------------------------------------------------------

-- | Authentication information
data Auth = Auth
  { authAccessToken  :: !ByteString   -- ^ OAuth access token
  , authExpiration   :: !UTCTime      -- ^ Expiration time of the access token
  }
  deriving Show

instance FromJSON Auth where
  parseJSON = withObject "Auth" $ \o ->
    Auth <$> o .:$ "access_token"
         <*> o .:  "expiration"

instance ToJSON Auth where
  toJSON Auth {..} = object
    [ "access_token" .=$ authAccessToken
    , "expiration"   .=  authExpiration
    ]

--------------------------------------------------------------------------------

-- | Metadata wrapper for a response from the Marketo API
data ApiResponse a = ApiResponse
  { rspRequestId  :: !Text
  , rspSuccess    :: !Bool
  , rspResult     :: !a
  }
  deriving (Show, Functor)

instance FromJSON a => FromJSON (ApiResponse a) where
  parseJSON = withObject "ApiResponse" $ \o -> do
    ApiResponse <$> o .: "requestId"
                <*> o .: "success"
                <*> o .: "result"

--------------------------------------------------------------------------------

-- | Error message response
data ErrorMessage = ErrorMessage
  { errorCode     :: !Text
  , errorMessage  :: !Text
  }
  deriving Show

-- | Status message response
data NoResult = NoResult
  { resultStatus   :: !Text
  , resultReasons  :: ![ErrorMessage]
  }
  deriving Show

--------------------------------------------------------------------------------

-- | Lead ID
--
-- Note: Use the 'Num' instance for easy construction.
newtype LeadId = LeadId { fromLeadId :: Int }
  deriving (Eq, Num)

instance Read LeadId where
  readsPrec n = map (first LeadId) . readsPrec n

instance Show LeadId where
  show = show . fromLeadId

instance ToJSON LeadId where
  toJSON = toJSON . fromLeadId

instance FromJSON LeadId where
  parseJSON = fmap LeadId . parseJSON

--------------------------------------------------------------------------------

-- | A lead
--
-- Note: We currently only decode part of the lead object. For all fields, see
-- 'leadObject'.
data Lead = Lead
  { leadId         :: !LeadId
  , leadEmail      :: !(Maybe Email)  -- ^ This field is supposed to be required
                    -- by Marketo, but it is possible to have a lead without it.
  , leadObject     :: !Object  -- ^ The entire object
  }
  deriving Show

instance FromJSON Lead where
  parseJSON = withObject "Lead" $ \o ->
    Lead <$> o .: "id"
         <*> o .: "email"
         <*> return o

--------------------------------------------------------------------------------

-- | Lead field key/value pair for creates or updates
data LeadField = LeadField
  { leadFieldName    :: !Text
  , leadFieldValue   :: !Value
  }
  deriving Show

-- | A lead update contains all the fields to be updated for a single lead
newtype LeadUpdate = LeadUpdate { leadUpdateFields :: [LeadField] }
  deriving Show

instance ToJSON LeadUpdate where
  toJSON = Object . H.fromList .
    map (\LeadField {..} -> (leadFieldName, leadFieldValue)) . leadUpdateFields

--------------------------------------------------------------------------------

-- | The action to apply for creating or updating leads
data LeadAction
  = CreateLead
  | UpdateLead
  | CreateOrUpdateLead
  | CreateDuplicateLead
  deriving (Eq, Show)

instance ToJSON LeadAction where
  toJSON = String . go
    where
      go CreateLead          = "createOnly"
      go UpdateLead          = "updateOnly"
      go CreateOrUpdateLead  = "createOrUpdate"
      go CreateDuplicateLead = "createDuplicate"

--------------------------------------------------------------------------------
-- Template Haskell declarations go at the end.

deriveFromJSON_ ''ErrorMessage (defaultRecordOptions 5)
deriveFromJSON_ ''NoResult     (defaultRecordOptions 6)
