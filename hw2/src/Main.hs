module Main where

import Lex 
import Parser
import MyData
import qualified Data.Map.Strict as Map
import Reducer


getParsedExpression str = parseExpression $ alexScanTokens $ str


main = do
  line <- getLine
  let [x, y] = words line
  let m = read x :: Int
  let k = read y :: Int
  expressionString <- getLine
  let expr = getParsedExpression expressionString
  putStrLn $ show expr
  let result = ($!) reduceExprMTimesKPrint expr m k
  mapM_ print $ reverse result


infixl 1 &
x & f = f x



