{-|
Module      : Network.CircleCI.Common.Lift
Copyright   : (c) Ben Gamari, 2018
License     : MIT
Maintainer  : ben@well-typed.com
Stability   : alpha

Lift @servant-client@ computations into the 'CircleCIResponse' monad.
-}

module Network.CircleCI.Common.Lift (
    liftClientM
  ) where

import Control.Monad.Reader ( liftIO )
import Network.CircleCI.Common.HTTPS ( httpsManager )
import Network.CircleCI.Common.Types ( CircleCIResponse )
import Network.CircleCI.Common.URL ( apiBaseUrl )
import Servant.Client ( ClientM, mkClientEnv, runClientM )

-- | Lift a 'ClientM' computation into the 'CircleCIResponse' monad.
liftClientM :: ClientM a -> CircleCIResponse a
liftClientM action = do
    manager <- httpsManager
    let env = mkClientEnv manager apiBaseUrl
    liftIO $ runClientM action env
