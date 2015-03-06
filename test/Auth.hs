module Auth where

import           Data.IORef
import           Data.Monoid
import qualified Data.Text.Encoding as TS
import           Network.HTTP.Client (withManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Web.Marketo as Mkto
import           Yesod.Core

import Foundation

postAuthR :: Handler ()
postAuthR = do
  App{..} <- getYesod

  fromAppId <- lookupPostParamBS "appId"
  let appId = Mkto.AppId{..}
  liftIO $ writeIORef appIdRef $ Just appId

  apiClientId     <- lookupPostParamBS "clientId"
  apiClientSecret <- lookupPostParamBS "clientSecret"
  let apiClient = Mkto.ApiClient{..}
  liftIO $ writeIORef apiClientRef $ Just apiClient

  auth <- liftIO $ withManager tlsManagerSettings $ Mkto.getAuth appId apiClient
  liftIO $ writeIORef authRef $ Just auth
  redirect HomeR
  where
    lookupPostParamBS key = lookupPostParam key >>= \case
      Nothing -> invalidArgs ["Missing " <> key]
      Just t  -> return $ TS.encodeUtf8 t
