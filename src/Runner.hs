module Runner where 

import RIO 
import Core 

import qualified Docker

data Service 
  = Service 
    { runBuild :: Build -> IO Build 
    }

createService :: Docker.Service -> IO Service 
createService docker = do 
  pure Service 
    { runBuild = undefined 
    }
