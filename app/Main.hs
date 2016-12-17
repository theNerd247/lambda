module Main where

import Lambda
import Parser
import Control.Monad.Catch
import Control.Monad

main :: IO ()
main = forever $ do
  putStrLn "Input Lambda Expression"
  t <- getLine
  l <- lambdaParser t
  putStr "> " >> (putStrLn . show $ l)
  putStr "> " >> (showL l)
  let nl = betaReduce $ l 
  putStr "> " >> (putStrLn . show $ nl)
  putStr "> " >> (showL nl)
  `catch` (\(LambdaParserException s) -> putStrLn s)
  where
    showL = putStrLn . displayLambda
