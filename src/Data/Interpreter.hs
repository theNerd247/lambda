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
  | EncodeNum Int
  | Quit
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data QuitException = QuitException deriving (Show,Data,Typeable)

type LambdaStateData = (Lambda -> Lambda)

type LambdaStateIO = StateT LambdaStateData IO

instance Exception QuitException

initLambdaStateData = id

runScript  :: (Traversable t, MonadState LambdaStateData m, MonadIO m, MonadThrow m)
  => t Cmd -> m ()
runScript = sequence_ . fmap runCommand 

runCommand
  :: (MonadState LambdaStateData m, MonadIO m, MonadThrow m)
  => Cmd -> m ()
runCommand (AssignVar name newL) = do
  modify' $ (. \s -> App (Bind name s) newL)
  displayOutput $ displayIVar name newL
runCommand (EvalLambda prog) = do
  l <- get
  displayOutput . displayLambda . betaReduce . l $ prog
runCommand (EncodeNum n) = runCommand $ AssignVar (show n) (encodeNum n)
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
displayIVar n l = n ++ " := " ++ (displayLambda l)

showEnv
  :: (MonadState LambdaStateData m, MonadIO m)
  => m ()
showEnv = get >>= showIVar . ($LVar "PROG")
  where
    showIVar (LVar _) = return ()
    showIVar (App (Bind name l) nl) = do
      displayOutput $ displayIVar name nl
      showIVar l
    showIVar _ = return ()
