module Foundation where

import           Data.IORef
import qualified Web.Marketo as Mkto
import           Yesod.Core

data App = App
  { appIdRef      :: IORef (Maybe Mkto.AppId)
  , apiClientRef  :: IORef (Maybe Mkto.ApiClient)
  , authRef       :: IORef (Maybe Mkto.Auth)
  }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  approot = ApprootStatic "http://localhost:3000"
