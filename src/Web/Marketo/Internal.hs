module Web.Marketo.Internal where

--------------------------------------------------------------------------------

import Web.Marketo.Common
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H

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
  -> Auth                             -- ^ For the REST API domain and access token
  -> Manager
  -> m a
apiRequest pathSegments modifyRequest processResponse Auth {..} mgr =
  parseUrl (apiDomain authApi : pathSegments)
  >>= acceptJSON
  >>= addHeader (hAuthorization, "Bearer " <> authAccessToken)
  >>= modifyRequest
  >>= flip httpLbs mgr
  >>= processResponse

--------------------------------------------------------------------------------

-- | Email address
type Email = Text

--------------------------------------------------------------------------------

-- | API access information
data ApiAccess = ApiAccess
  { apiClientId     :: !ByteString  -- ^ OAuth client ID
  , apiClientSecret :: !ByteString  -- ^ OAuth client secret
  , apiDomain       :: !ByteString  -- ^ Domain for REST API (i.e. the URL minus protocol and path)
  }
  deriving Show

-- | Authentication information
data Auth = Auth
  { authApi          :: !ApiAccess    -- ^ Information needed to access the API
  , authAccessToken  :: !ByteString   -- ^ OAuth access token
  , authExpiration   :: !UTCTime      -- ^ Expiration time of the access token
  }
  deriving Show

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
  , leadEmail      :: !Email
  , leadObject     :: !Object  -- ^ The entire object
  }
  deriving Show

instance ToJSON Lead where
  toJSON Lead {..} =
    -- The leadObject may be empty. We assume the leadId and leadEmail are
    -- preferred over whatever is in the leadObject.
    Object $ leadObject <> H.fromList ["id" .= leadId, "email" .= leadEmail]

instance FromJSON Lead where
  parseJSON = withObject "Lead" $ \o ->
    Lead <$> o .: "id"
         <*> o .: "email"
         <*> return o

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
