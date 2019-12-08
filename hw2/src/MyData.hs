module MyData where

import qualified Data.Map.Strict as Map

data Expression = Application Expression Expression | Abstraction String Expression | Var String | Wrapper Int deriving (Eq)

data Reduction = Reducted Expression | None Expression deriving (Show)

data Dt = Dt (Map.Map Int Expression) Int Int deriving (Show)

instance Show Expression where
 show (Application exp1 exp2) = "(" ++ show exp1 ++ " " ++ show exp2 ++ ")"
 show (Abstraction var exp2) = "(\\" ++ var ++ "." ++ show exp2 ++ ")"
 show (Var variable) = variable
 show (Wrapper int) = "WRAPPER!!!!" ++ (show $ int)


--алгебраический тип
data AlgebraicType = Atom Int | Impl AlgebraicType AlgebraicType

instance Show AlgebraicType where
    show (Atom index) =  "t" ++ (show index)
    show (Impl a b) = "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"

instance Eq AlgebraicType where
    (==) (Atom x) (Atom y) = x == y
    (==) (Impl a b) (Impl c d) = a == c && b == d
    (==) _ _ = False

instance Ord AlgebraicType where
    compare a b = compare (show a) (show b)

data Equation = Equation AlgebraicType AlgebraicType

instance Show Equation where
    show (Equation l r) = (show l) ++ " = " ++ (show r)

instance Eq Equation where
    (==) (Equation a b) (Equation c d) = (a == c && b == d)  