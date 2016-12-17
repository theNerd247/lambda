module Main where

import Parser
import Parser.Interpreter
import Data.Interpreter
import Control.Monad.Catch
import Control.Monad
import Control.Monad.State

main :: IO ()
main = evalStateT (forever exeCmd) initLambdaStateData
  where
    exeCmd :: LambdaStateIO ()
    exeCmd =
      (liftIO getLine >>= runParser parseCmds >>= runCommand) `catch`
      (\(LambdaParserException s) -> liftIO $ putStrLn s)
