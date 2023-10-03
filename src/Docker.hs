module Docker where

import RIO
import qualified Network.HTTP.Simple as HTTP
import qualified Data.Aeson as Aeson
import qualified Socket

data CreateContainerOptions
  = CreateContainerOptions
    { image :: Image
    }

newtype Image = Image Text
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)


imageToText :: Docker.Image -> Text
imageToText (Docker.Image image) = image

exitCodeToInt :: Docker.ContainerExitCode -> Int
exitCodeToInt (Docker.ContainerExitCode code) = code

createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
  manager <- Socket.newManager "/var/run/docker.sock"
  let image = imageToText options.image
  let body = Aeson.object
        [("Image", Aeson.toJSON image)
        ]
  let req = HTTP.defaultRequest 
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath "/v1.43/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
  res <- HTTP.httpBS req 

  traceShowIO res

