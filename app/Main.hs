module Main where

import Parser
import Parser.Interpreter
import Data.Interpreter
import Encodes.ChurchNumeral
import Control.Monad.Catch
import Control.Monad
import Control.Monad.State

initEnv :: LambdaStateIO ()
initEnv = sequence_ . fmap runCommand $ 
  [ AssignVar 's' succL
  , AssignVar 'a' addL
  , AssignVar 'm' multL
  , AssignVar 'p' powL
  ]

main :: IO ()
main = evalStateT (initEnv >> forever exeCmd) initLambdaStateData
  where
    exeCmd :: LambdaStateIO ()
    exeCmd =
      (liftIO getLine >>= runParser parseCmds >>= runCommand) `catch`
      (\(LambdaParserException s) -> liftIO $ putStrLn s)
