module Web.Marketo.Auth
  ( getAuth
  , refreshAuth
  , withRefresh
  ) where

--------------------------------------------------------------------------------

import Web.Marketo.Common
import Web.Marketo.Internal
import qualified Data.Text.Encoding as TS

--------------------------------------------------------------------------------

-- | Generate an access token and return the 'Auth' data needed for
-- authenticating API accesses.
getAuth
  :: MonadIO m
  => AppId
  -> ApiAccess
  -> Manager
  -> m Auth
getAuth appId ApiAccess {..} mgr =
  parseUrl appId ["identity", "oauth", "token"]
  >>= acceptJSON
  >>= setQuery [ ( "grant_type"    , Just "client_credentials" )
               , ( "client_id"     , Just apiClientId          )
               , ( "client_secret" , Just apiClientSecret      )
               ]
  >>= flip httpLbs mgr
  >>= fromJSONResponse "getAuth"
  >>= mkAuth

-- | Check if the access token has expired and, if needed, refresh the token.
-- Return the new 'Auth', if updated.
refreshAuth
  :: MonadIO m
  => AppId
  -> ApiAccess
  -> Auth
  -> Manager
  -> m (Maybe Auth)
refreshAuth appId apiAccess Auth {..} mgr = do
  tm <- liftIO getCurrentTime
  let expired = authExpiration `diffUTCTime` tm < 5 * 60 {- 5 minutes -}
  if expired then Just `liftM` getAuth appId apiAccess mgr else return Nothing

-- | Given a function that requires authentication, first refresh the access
-- token, if needed. Then, run the function argument. Return the new 'Auth', if
-- updated, along with the result of the argument.
withRefresh
  :: MonadIO m
  => (AppId -> ApiAccess -> Auth -> Manager -> m a)
  -> AppId
  -> ApiAccess
  -> Auth
  -> Manager
  -> m (Maybe Auth, a)
withRefresh run appId apiAccess auth mgr = do
  mAuth' <- refreshAuth appId apiAccess auth mgr
  result <- run appId apiAccess (fromMaybe auth mAuth') mgr
  return (mAuth', result)

--------------------------------------------------------------------------------

data AccessTokenResponse = AccessTokenResponse
  { atrAccessToken :: !ByteString  -- ^ Temporary authorization code
  , atrExpiresIn   :: !Int         -- ^ Seconds
  , atrScope       :: !Email       -- ^ Email address for API account
  }

instance FromJSON AccessTokenResponse where
  parseJSON = withObject "AccessTokenResponse" $ \o -> do
    "bearer" :: Text <- o .: "token_type"
    AccessTokenResponse <$> (TS.encodeUtf8 <$> o .: "access_token")
                        <*> o .: "expires_in"
                        <*> o .: "scope"

-- | Construct a new 'Auth'
mkAuth :: MonadIO m => AccessTokenResponse -> m Auth
mkAuth AccessTokenResponse {..} = do
  tm <- expireTime atrExpiresIn `liftM` liftIO getCurrentTime
  return $ Auth atrAccessToken tm
