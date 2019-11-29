module Main where

import Lex 
import Parser
import System
import MyData
import Proofer
import qualified Data.Map.Strict as Map


getParsedExpression str = parseExpression $ alexScanTokens $ str


main = do
  contents <- getContents
  let expr = parseExpression $ alexScanTokens $ contents
  let (system, _ , _, mapFree, _) = getSystemForExpression expr
  case (solveSystem system) of
      (Just system) -> do
          let proofList = getProof expr system mapFree
          putStr (unlines proofList)
      Nothing -> do 
          putStrLn "Expression has no type"
          
    

infixl 1 &
x & f = f x



