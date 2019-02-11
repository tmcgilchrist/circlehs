{-|
Module      : Network.CircleCI.Common.HTTPS
Copyright   : (c) Denis Shevchenko, 2016
License     : MIT
Maintainer  : me@dshevchenko.biz
Stability   : alpha

Connection manager for @https@-requests. CircleCI uses TLS.
-}

module Network.CircleCI.Common.HTTPS (
    httpsManager
  ) where

import           Control.Monad.IO.Class ( MonadIO (..) )
import           Network.HTTP.Client ( Manager, newManager )
import           Network.HTTP.Client.TLS ( tlsManagerSettings )

-- | Connection manager with TLS support.
httpsManager :: MonadIO m => m Manager
httpsManager = liftIO $ newManager tlsManagerSettings
