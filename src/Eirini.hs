{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Eirini where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson                    (FromJSON, ToJSON, Value (..), object,
                                      (.=))
import Data.Map
import Data.Text.Lazy
import GHC.Generics
import Kubernetes.OpenAPI.API.AppsV1
import Kubernetes.OpenAPI.Client
import Kubernetes.OpenAPI.Core
import Kubernetes.OpenAPI.MimeTypes
import Kubernetes.OpenAPI.Model
import KubernetesExtras.Client
import Network.HTTP.Client           hiding (Response)
import Network.HTTP.Types.Status
import Network.Wai                   (Application, Response)
import Network.Wai.Handler.Warp      (run)
import System.Environment

import qualified Web.Scotty.Trans as S

data EiriniOpts = EiriniOpts KubeConfigSource

newtype AppT a = AppT
  { unAppT :: ReaderT EiriniOpts IO a
  } deriving  ( Applicative, Functor, Monad
              , MonadIO, MonadReader EiriniOpts)

class Monad m => KubernetesService m where
  getKubernetesClientConfig :: m (Manager, KubernetesClientConfig)

type App r m = (KubernetesService m, MonadIO m)

instance KubernetesService AppT where
  getKubernetesClientConfig = do
    (EiriniOpts kubeConfigSource) <- ask
    liftIO $ kubeClient kubeConfigSource

routes :: App r m => S.ScottyT Text m ()
routes = do
  S.put "/apps/:process_guid" $ do
    desireHandler

app :: EiriniOpts -> IO Application
app opts = let runner app = flip runReaderT opts $ unAppT app
           in S.scottyAppT runner routes

runApp :: IO ()
runApp = do
  home <- getEnv "HOME"
  run 8080 =<< app (EiriniOpts $ KubeConfigFile (home ++ "/.kube/config"))

data VolumeMount = VolumeMount { volumeID :: Text, volumeMountDir :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON VolumeMount
instance FromJSON VolumeMount

data DesireLRPRequest = DesireLRPRequest { lrpGuid :: Text
                                         , lrpVersion :: Text
                                         , lrpProcessGuid :: Text
                                         , lrpPorts :: []Int
                                         , lrpRoutes :: Map Text Value
                                         , lrpDockerImageURL :: Text
                                         , lrpDropletHash :: Text
                                         , lrpStartCommand :: Text
                                         , lrpEnvironment :: Map Text Text
                                         , lrpInstnaces :: Int
                                         , lrpLastUpdated :: Text
                                         , lrpHealthCheckType :: Text
                                         , lrpHealthCheckHTTPEndpoint :: Text
                                         , lrpHealthCheckTimeoutMs :: Int
                                         , lrpMemoryMB :: Int
                                         , lrpCPUWeigh :: Int
                                         , lrpVolumeMounts :: [VolumeMount]
                                         }
  deriving (Eq, Show, Generic)

instance ToJSON DesireLRPRequest
instance FromJSON DesireLRPRequest

desireHandler :: App r m => S.ActionT Text m ()
desireHandler = do
  guid <- S.param "process_guid"
  desireRequest <- S.jsonData
  let ssRequest = createStatefulSetRequest $ statefulSetFromLRPRequest guid desireRequest
  (manager, config) <- lift getKubernetesClientConfig
  (MimeResult result response) <- liftIO $ dispatchMime manager config ssRequest
  _ <- liftIO $ print response
  case result of
    Left (MimeError err _) -> do
      _ <- S.status status500
      S.text "Something went wrong"
    Right _ -> S.text . pack $ show response

statefulSetFromLRPRequest :: Text -> DesireLRPRequest -> V1StatefulSet
statefulSetFromLRPRequest processGUID (DesireLRPRequest{..}) = do
  let labels = pure $ Data.Map.singleton "deployed-by" "eirini"
      meta = mkV1ObjectMeta { v1ObjectMetaName = Just "statefulset-eirini-hs", v1ObjectMetaLabels = labels }
      container = (mkV1Container "appinstance"){ v1ContainerImage = pure $ toStrict lrpDockerImageURL
                                               , v1ContainerCommand = pure $ [toStrict lrpStartCommand]
                                               }
      podTemplateSpec = mkV1PodTemplateSpec { v1PodTemplateSpecMetadata = pure meta
                                            , v1PodTemplateSpecSpec = pure $ mkV1PodSpec [container]}
      labelSelector = mkV1LabelSelector {v1LabelSelectorMatchLabels = labels}
      spec = mkV1StatefulSetSpec labelSelector "eirinihs" podTemplateSpec
  mkV1StatefulSet { v1StatefulSetMetadata = pure meta
                  , v1StatefulSetSpec = pure spec
                  }

createStatefulSetRequest :: V1StatefulSet
                         -> KubernetesRequest CreateNamespacedStatefulSet MimeJSON V1StatefulSet MimeJSON
createStatefulSetRequest s =
  createNamespacedStatefulSet (ContentType MimeJSON) (Accept MimeJSON) s (Namespace "eirini")
