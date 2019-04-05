{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import Control.Monad.IO.Class    (MonadIO)
import Data.Aeson                (Value (..), encode, object, (.=))
import Kubernetes.OpenAPI.Core
import Kubernetes.OpenAPI.Model
import KubernetesExtras.Client
import Network.HTTP.Types.Header
import Network.Wai               (Application)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import qualified Data.Map as Map

import Eirini

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  with (app $ EiriniOpts $ KubeConfigFile "/Users/axeman/.kube/config") $ do
    describe "desire LRP" $ do
      it "responds with 200" $ do
        let lrp = DesireLRPRequest { lrpGuid = "guid-1234"
                                   , lrpVersion = "version-1234"
                                   , lrpProcessGuid = "process-1234"
                                   , lrpPorts = [8080]
                                   , lrpRoutes = Map.empty
                                   , lrpDockerImageURL = "redis"
                                   , lrpDropletHash = "droplet-hash"
                                   , lrpStartCommand = "redis-server"
                                   , lrpEnvironment = Map.empty
                                   , lrpInstnaces = 1
                                   , lrpLastUpdated = "2018-01-01"
                                   , lrpHealthCheckType = "some-health-check-type"
                                   , lrpHealthCheckHTTPEndpoint = "/healthz"
                                   , lrpHealthCheckTimeoutMs = 2000
                                   , lrpMemoryMB = 1000
                                   , lrpCPUWeigh = 1
                                   , lrpVolumeMounts = []
                                   }
        put "/apps/process-1234" (encode lrp) `shouldRespondWith` 200
