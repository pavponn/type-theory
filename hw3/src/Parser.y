{
module Parser where
import Lex
import MyData
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
  : Appl LAMBDA VAR DOT Expr       { (Application $1 (Abstraction $3 $5)) }
  | LAMBDA VAR DOT Expr            { Abstraction $2 $4 }
  | Appl                           { $1 }

Appl
  : Appl Term                      { Application $1 $2 }
  | Term                           { $1 }

Term
  : OPENB Expr CLOSEB              { $2 }
  | VAR                            { Var $1 }


{

parseError =  fail "Parse error"


}
