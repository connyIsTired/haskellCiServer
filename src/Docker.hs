module Docker where

import RIO
import qualified Network.HTTP.Simple as HTTP

data CreateContainerOptions
  = CreateContainerOptions
    { image :: Image
    }

newtype Image = Image Text
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)


createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
  let body = ()
  let req = HTTP.defaultRequest 
          & HTTP.setRequestPath "/v1.40/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
  res <- HTTP.httpBS req 

  traceShowIO res

