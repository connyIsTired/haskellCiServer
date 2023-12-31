module Main where

import qualified Docker
import qualified Runner

import RIO
import Test.Hspec
import Core
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial 
import qualified RIO.Map as Map
import qualified RIO.Process as Process
import GHC.Base (build)

makeStep name image commands
  = Step
  { name = StepName name
  , image = Docker.Image image
  , commands = NonEmpty.Partial.fromList commands
  }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline { steps = NonEmpty.Partial.fromList steps }

main :: IO ()
main = hspec do 
  docker <- runIO Docker.createService
  runner <- runIO $ Runner.createService docker
  beforeAll cleanupDocker $ describe "Quad CI" do
    it "should run a build (success)" do 
      testRunSuccess runner
    it "should run a build (failure)" do
      testRunFailure runner
    it "should share workspace between steps" do 
      testSharedWorkspace docker runner

cleanupDocker :: IO ()
cleanupDocker = void do 
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=quad\")"

  Process.readProcessStdout "docker volume rm -f $(docker volume ls -q --filter \"label=quad\")"

runBuild :: Docker.Service -> Build -> IO Build 
runBuild docker build = do 
  newBuild <- Core.progress docker build 
  case newBuild.state of 
    BuildFinished _ -> 
      pure newBuild
    _ -> do 
      threadDelay (1 * 1000 * 1000)
      runBuild docker newBuild

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do  
  build <- runner.prepareBuild $ makePipeline 
              [ makeStep "First step" "ubuntu" ["date"]
              , makeStep " Second step" "ubuntu" ["uname -r"]
              ]
  result <- runner.runBuild build

  result.state `shouldBe` BuildFinished BuildSucceeded 
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do 
  build <- runner.prepareBuild $ makePipeline 
              [ makeStep "Should fail" "ubuntu" ["exit 1"]
              ]
  result <- runner.runBuild build 

  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

testSharedWorkspace :: Docker.Service -> Runner.Service -> IO () 
testSharedWorkspace docker runner = do 
  build <- runner.prepareBuild $ makePipeline 
              [ makeStep "Create file" "ubuntu" ["echo hello > test"]
              , makeStep "Read file" "ubuntu" ["cat test"]
              ]

  result <- runner.runBuild build
  result.state `shouldBe` BuildFinished BuildSucceeded 
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded] 
