module Docker where

import RIO

data CreateContainerOptions
  = CreateContainerOptions
    { image :: Image
    }

createContainer :: CreateContainerOptions -> IO ()
createContainer options = undefined
