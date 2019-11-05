{
module Parser where
import Lex
}

%name      parseExpression
%tokentype { Token }
%error     { parseError }


%token VAR  { TVar $$ }
%token OPENB  { TOpenB }
%token CLOSEB { TCloseB }
%token LAMBDA   { TLambda }
%token DOT     { TDot }



%%

Expr
  : Appl LAMBDA VAR DOT Expr       { (Application $1 (Abstraction (Var $3) $5)) }
  | LAMBDA VAR DOT Expr            { Abstraction (Var $2) $4 }
  | Appl                           { $1 }

Appl
  : Appl Term                      { Application $1 $2 }
  | Term                           { $1 }

Term
  : OPENB Expr CLOSEB              { $2 }
  | VAR                            {Var $1 }


{

parseError =  fail "Parse error"

data Expression = Application Expression Expression | Abstraction Expression Expression | Var String deriving (Eq)


instance Show Expression where
 show (Application exp1 exp2) = "(" ++ show exp1 ++ " " ++ show exp2 ++ ")"
 show (Abstraction exp1 exp2) = "(\\" ++ show exp1 ++ "." ++ show exp2 ++ ")"
 show (Var variable) = variable

}
