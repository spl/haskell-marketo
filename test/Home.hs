module Home where

import           Control.Applicative
import qualified Data.Aeson as A
import           Data.IORef (readIORef)
import qualified Text.Blaze as Blaze
import qualified Web.Marketo as Mkto
import           Yesod.Core

import           Foundation

getHomeR :: Handler Html
getHomeR = do
  App{..} <- getYesod
  mAppId     <- liftIO $ readIORef appIdRef
  mApiClient <- liftIO $ readIORef apiClientRef
  mAuth      <- liftIO $ readIORef authRef
  let mClientId     = Mkto.apiClientId     <$> mApiClient
  let mClientSecret = Mkto.apiClientSecret <$> mApiClient
  defaultLayout $ do
    let title = "Marketo Test"
    setTitle title
    [whamlet|
      <h1>#{title}
      <h2>Authenticate
      <p>
        <form method="POST" action=@{AuthR}>
          <label>App ID
          $maybe appId <- mAppId
            <input type="text" name="appId" value="#{Blaze.string $ show appId}">
          $nothing
            <input type="text" name="appId">
          <br>
          <label>Client ID
          $maybe clientId <- mClientId
            <input type="text" name="clientId" value="#{Blaze.unsafeByteString clientId}">
          $nothing
            <input type="text" name="clientId">
          <br>
          <label>Client Secret
          $maybe clientSecret <- mClientSecret
            <input type="text" name="clientSecret" value="#{Blaze.unsafeByteString clientSecret}">
          $nothing
            <input type="text" name="clientSecret">
          <br>
          <input type="submit">
      $maybe auth <- mAuth
        <h2>Authenticated
        <p>
          Access Token:
          <code>
            #{Blaze.unsafeLazyByteString $ A.encode auth}
    |]
