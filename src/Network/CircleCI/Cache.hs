{-|
Module      : Network.CircleCI.Cache
Copyright   : (c) Denis Shevchenko, 2016
License     : MIT
Maintainer  : me@dshevchenko.biz
Stability   : alpha

API call for work with project build's cache.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Network.CircleCI.Cache (
  -- * API call
    clearCache
  -- * Type for response
  , CacheCleared (..)
  , module X
  ) where

import           Network.CircleCI.Common.Lift ( liftClientM )
import           Network.CircleCI.Common.Types as X
import           Network.CircleCI.Common.Run as X

import           Control.Monad                  ( mzero )
import           Control.Monad.Reader           ( ask )
import           Data.Aeson ( FromJSON (..), Value (..), (.:))
import           Data.Aeson.Types ( Parser )
import qualified Data.Proxy                     as P
import           Data.Text                      ( Text )

import           Servant.API ( QueryParam, (:>), Capture, Delete, JSON )
import           Servant.Client ( ClientM, client )

-- | Clears build cache. Based on https://circleci.com/docs/api/#clear-cache.
--
-- Usage example:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- {-\# LANGUAGE LambdaCase \#-}
--
-- import Network.CircleCI
--
-- main :: IO ()
-- main = runCircleCI (clearCache $ ProjectPoint "denisshevchenko" "circlehs")
--                    (AccountAPIToken "e64c674195bbc0dbe3f9676c6ba2whatever")
--     >>= \\case
--         Left problem    -> print problem
--         Right isCleared -> print isCleared
-- @
clearCache :: ProjectPoint                  -- ^ Names of GitHub user/project.
           -> CircleCIResponse CacheCleared -- ^ Info about clearing.
clearCache project = do
    AccountAPIToken token <- ask
    liftClientM $ servantClearCache
        (userName project)
        (projectName project)
        (Just token)

-- | Cache clearing status.
data CacheCleared = CacheSuccessfullyCleared
                  | UnableToClearCache ErrorMessage
                  deriving (Show)

-- How to make CacheCleared from JSON.
instance FromJSON CacheCleared where
    parseJSON (Object o) =
        o .: "status" >>= toCacheCleared
    parseJSON _ = mzero

toCacheCleared :: Text -> Parser CacheCleared
toCacheCleared rawStatus = return $
    if | rawStatus `elem` okMessages -> CacheSuccessfullyCleared
       | otherwise -> UnableToClearCache rawStatus
  where
    okMessages = [ "build dependency caches deleted"
                 , "build caches deleted"
                 ]

-------------------------------------------------------------------------------
-- API types for Servant ------------------------------------------------------
-------------------------------------------------------------------------------

-- Complete API for work with build cache.
type CacheAPI = ClearCacheCall

-- Clears build cache.
type ClearCacheCall =
       "project"
    :> Capture "username" UserName
    :> Capture "project" ProjectName
    :> "build-cache"
    :> QueryParam "circle-token" Token
    :> Delete '[JSON] CacheCleared
    -- DELETE: /project/:username/:project/build-cache?circle-token=:token

-------------------------------------------------------------------------------
-- API client calls for Servant -----------------------------------------------
-------------------------------------------------------------------------------

servantClearCache :: UserName
                  -> ProjectName
                  -> Maybe Token
                  -> ClientM CacheCleared
servantClearCache = client cacheAPI

cacheAPI :: P.Proxy CacheAPI
cacheAPI = P.Proxy
