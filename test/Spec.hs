module Main where

import qualified Docker

import RIO
import  Test.Hspec
import Core
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial 

makeStep name image commands
  = Step
  { name = StepName name
  , image = Docker.Image image
  , commands = NonEmpty.Partial.fromList commands
  }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline { steps = NonEmpty.Partial.fromList steps }

testPipeline :: Pipeline
testPipeline = makePipeline
  [ makeStep "First step" "ubuntu" ["date"]
  , makeStep "Second step" "ubuntu" ["uname -r"]
  ]

testBuild :: Build
testBuild = Build
  { pipeline = testPipeline
  , state = BuildReady
  , completedSteps = mempty
  }

main :: IO ()
main = hspec do 
  describe "Quad CI" do 
    it "should run a build (success)" do 
      1 `shouldBe` 1

runBuild :: Docker.Service -> Build -> IO Build 
runBuild docker build = do 
  newBuild <- Core.progress docker build 
  case newBuild.state of 
    BuildFinished _ -> 
      pure newBuild
    _ -> do 
      threadDelay (1 * 1000 * 1000)
      runBuild docker newBuild
