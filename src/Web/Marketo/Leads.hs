module Web.Marketo.Leads
  ( getLead
  , createOrUpdateLead
  ) where

--------------------------------------------------------------------------------

import Web.Marketo.Common
import Web.Marketo.Internal

--------------------------------------------------------------------------------

-- | Get a lead by ID
--
-- Note: If the lead exists, the result should be a singleton list. But if the
-- lead is not found, the result may be an empty list.
--
-- http://developers.marketo.com/documentation/rest/get-lead-by-id/
getLead
  :: MonadIO m
  => LeadId
  -> Auth
  -> Manager
  -> m (ApiResponse [Lead])
getLead leadId = apiRequest
  ["rest", "v1", "lead", intToBS (fromLeadId leadId) <> ".json"]
  return
  (fromJSONResponse "getLead")

-- | Create or update leads
--
-- Note: Unless you use @Just "id"@ in the lookup field, the leadId fields of the
-- leads parameter are ignored.
--
-- http://developers.marketo.com/documentation/rest/get-lead-by-id/
createOrUpdateLead
  :: MonadIO m
  => LeadAction   -- ^ Action such as 'CreateLead' or 'UpdateLead'
  -> [Lead]       -- ^ List of leads to create/update
  -> Maybe Text   -- ^ Lookup field for updates (e.g. Use "id" to match the leadId)
  -> Auth
  -> Manager
  -> m (ApiResponse [Either NoResult LeadId])
createOrUpdateLead action leads mLookupField = apiRequest
  ["rest", "v1", "leads.json"]
  (setJSONBody POST $ COULRequest action leads mLookupField)
  (fromJSONResponse "createOrUpdateLead" >=> fmap (map fromCOULResponse) >>> return)

--------------------------------------------------------------------------------

-- | Create or update lead request
data COULRequest = COULRequest
  { _coulrAction       :: !LeadAction
  , _coulrInput        :: ![Lead]
  , _coulrlookupField  :: !(Maybe Text)
  }

newtype COULResponse = COULResponse
  { fromCOULResponse :: Either NoResult LeadId
  }

instance FromJSON COULResponse where
  parseJSON = withObject "COULResponse" $ \o ->
    COULResponse <$> (Right <$> o .: "id" <|> Left <$> parseJSON (Object o))

--------------------------------------------------------------------------------
-- Template Haskell declarations go at the end.

deriveToJSON_ ''COULRequest (defaultRecordOptions 6)
