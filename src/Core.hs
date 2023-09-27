module Core where

import RIO
import qualified RIO.Map as Map

data Pipeline 
  = Pipeline
    { steps :: NonEmpty Step
    }
    deriving (Eq, Show)

data Step 
  = Step
    { name :: StepName
    , commands :: NonEmpty Text
    , image :: Image
    }
    deriving (Eq, Show)

data Build 
   = Build 
    { pipeline :: Pipeline 
    , state :: BuildState
    , completedSteps :: Map StepName StepResult
    }
    deriving (Eq, Show)

data StepResult 
  = StepFailed ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show)

data BuildState
  = BuildReady
  | BuildRunning
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

newtype StepName = StepName Text
  deriving (Eq, Show, Ord)

newtype Image = Image Text
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

imageToText :: Image -> Text
imageToText (Image image) = image

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult exit = 
  if exitCodeToInt exit == 0 
    then StepSucceeded
    else StepFailed exit

progress :: Build -> IO Build
progress build = 
  case build.state of
    BuildReady -> 
      case buildHasNextStep build of 
        Left result -> 
          pure $ build{state = BuildFinished result}
        Right step -> 
          pure $ build{state = BuildRunning}
    BuildRunning -> undefined
    BuildFinished _ -> 
      pure build

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build = 
  undefined

