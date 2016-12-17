{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Interpreter where

import Data.Lambda
import Data.Data
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Catch
import Encodes.ChurchNumeral

data Cmd
  = AssignVar VarName
              Lambda
  | EvalLambda Lambda
  | ShowEnv
  | EncodeNum VarName Int
  | Quit
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data QuitException = QuitException deriving (Show,Data,Typeable)

type LambdaStateData = (Lambda -> Lambda)

type LambdaStateIO = StateT LambdaStateData IO

instance Exception QuitException

initLambdaStateData = id

runCommand
  :: (MonadState LambdaStateData m, MonadIO m, MonadThrow m)
  => Cmd -> m ()
runCommand (AssignVar name newL) = do
  modify' $ (\l s -> App (Bind name (l s)) newL)
  displayOutput $ displayIVar name newL
runCommand (EvalLambda prog) = do
  l <- get
  displayOutput . displayLambda . betaReduce . l $ prog
runCommand (EncodeNum name n) = runCommand $ AssignVar name (encodeNum n)
runCommand Quit = throwM QuitException
runCommand ShowEnv = showEnv

displayOutput
  :: (MonadIO m)
  => String -> m ()
displayOutput s =
  liftIO $
  do putStr "> "
     putStrLn s

displayIVar :: VarName -> Lambda -> String
displayIVar n l = [n] ++ " := " ++ (displayLambda l)

showEnv
  :: (MonadState LambdaStateData m, MonadIO m)
  => m ()
showEnv = get >>= (showIVar . ($LVar 'M'))
  where
    showIVar (LVar _) = return ()
    showIVar (App (Bind name l) nl) = do
      displayOutput $ displayIVar name nl
      showIVar l
    showIVar _ = return ()
