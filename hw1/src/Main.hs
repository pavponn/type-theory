module Main where

import Lex 
import Parser


main = do
  contents <- getContents
  putStrLn $ show $ parseExpression $ alexScanTokens $ contents
  
    

infixl 1 &
x & f = f x

