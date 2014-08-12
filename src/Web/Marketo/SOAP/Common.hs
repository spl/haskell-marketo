module Web.Marketo.SOAP.Common
  ( module Web.Marketo.Common
  , module Network.SOAP
  , module Network.SOAP.Transport.HTTP
  , module Network.HTTP.Client.TLS
  ) where

--------------------------------------------------------------------------------

import Web.Marketo.Common hiding (Parser)
import Network.SOAP
import Network.SOAP.Transport.HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
