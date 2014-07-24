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
  => ApiAccess
  -> Manager
  -> m Auth
getAuth apiAccess@ApiAccess {..} mgr =
  parseUrl [apiDomain, "identity", "oauth", "token"]
  >>= acceptJSON
  >>= setQuery [ ( "grant_type"    , Just "client_credentials" )
               , ( "client_id"     , Just apiClientId          )
               , ( "client_secret" , Just apiClientSecret      )
               ]
  >>= flip httpLbs mgr
  >>= fromJSONResponse "getAuth"
  >>= mkAuth apiAccess

-- | Check if the access token has expired and, if needed, refresh the token.
-- Return the new 'Auth', if updated.
refreshAuth
  :: MonadIO m
  => Auth
  -> Manager
  -> m (Maybe Auth)
refreshAuth Auth {..} mgr = do
  tm <- liftIO getCurrentTime
  let expired = authExpiration `diffUTCTime` tm < 5 * 60 {- 5 minutes -}
  if expired then Just `liftM` getAuth authApi mgr else return Nothing

-- | Given a function that requires authentication, first refresh the access
-- token, if needed. Then, run the function argument. Return the new 'Auth', if
-- updated, along with the result of the argument.
withRefresh
  :: MonadIO m
  => (Auth -> Manager -> m a)
  -> Auth
  -> Manager
  -> m (Maybe Auth, a)
withRefresh run auth mgr = do
  mAuth' <- refreshAuth auth mgr
  result <- run (fromMaybe auth mAuth') mgr
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
mkAuth :: MonadIO m => ApiAccess -> AccessTokenResponse -> m Auth
mkAuth apiAccess AccessTokenResponse {..} = do
  tm <- expireTime atrExpiresIn `liftM` liftIO getCurrentTime
  return $ Auth apiAccess atrAccessToken tm
