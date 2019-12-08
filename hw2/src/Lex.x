{
module Lex where
import Numeric
}

%wrapper "basic"

$alpha = [a-z]
$digit = [0-9]

tokens :-
  $white+                     ;
  "\"         { \_  -> TLambda }
  "."          { \_  -> TDot }
  "("          { \_  -> TOpenB }
  ")"          { \_  -> TCloseB }
  $alpha[$alpha $digit \' \’]*    { \s ->TVar s }
 
{

data Token = TVar String | TLambda | TDot | TOpenB | TCloseB | TEOF deriving (Eq)

instance Show Token where
  show x = case x of
    TVar s -> s
    TLambda -> "\\"
    TDot -> "."
    TOpenB -> "("
    TCloseB -> ")"
    TEOF -> "(EOF)"

}
