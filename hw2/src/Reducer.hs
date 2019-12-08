module Reducer where

import MyData
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map


reduceExprMTimesKPrint :: Expression -> Int -> Int -> [Expression]
reduceExprMTimesKPrint expr m k = reduceAll expr [] (Dt Map.empty 1 0) m k 

reduceAll :: Expression ->  [Expression] -> Dt -> Int -> Int  -> [Expression]
reduceAll curExpr result dt m k = do
    let (maybeReducted, newDt) = reduce dt curExpr
    let (Dt map curK varId) = newDt
    let nextExpr = getExprFromReduction maybeReducted
    if  ((wasReduced maybeReducted) && m >= curK) then 
        if curK `mod` k == 0 then do
            let exprToPrint = ($!) unfoldExp map nextExpr
            let newResult = ($!) (:) exprToPrint result
            reduceAll nextExpr newResult (Dt map (curK + 1) varId) m k
        else 
            reduceAll nextExpr result (Dt map (curK + 1) varId) m k
    else 
        if ((not $ wasReduced maybeReducted) && ((curK - 1) `mod` k /= 0)) then do
            let exprToPrint = ($!) unfoldExp map nextExpr
            let finalResult = ($!) (:) exprToPrint result
            finalResult
        else result


wasReduced :: Reduction -> Bool
wasReduced (Reducted _) = True
wasReduced (None _) = False


getExprFromReduction :: Reduction -> Expression
getExprFromReduction (Reducted e) = e
getExprFromReduction (None e) = e


reduce :: Dt -> Expression -> (Reduction, Dt)
reduce  dt@(Dt map curK varId) expr = do
    case expr of 
        
        (Var s) -> (None (Var s), dt)
        
        (Abstraction var p) -> do
            let (maybeReducted, newDt) = ($!) reduce dt p
            case maybeReducted of 
                (Reducted re) -> (Reducted (Abstraction var (re)), newDt)
                (None re) -> (None (Abstraction var (re)), newDt)
        (Application left right) -> reduceApp dt (Application left right)
        
        (Wrapper i) -> do
            let realExpr = unwrap (($!) Map.lookup i map)
            let (maybeReducted, (Dt newMap newStep newId)) = reduce dt realExpr
            case (maybeReducted) of 
                Reducted re -> do
                    let newMap2 = ($!) Map.insert i re newMap
                    let reducted = (Reducted (Wrapper i))
                    (reducted, (Dt newMap2 newStep newId)) 
                None re -> do
                    let newMap2 = ($!) Map.insert i re newMap
                    let none = (None (Wrapper i))
                    (none, (Dt newMap2 newStep newId))


reduceApp :: Dt -> Expression -> (Reduction, Dt)
reduceApp dt@(Dt map curK varId) app =
    case app of 
        -- аппликация wrapper'а и выражения
        (Application (Wrapper i) p) -> do
            let maybeAbstr = ($!) checkIfAbstr map (Wrapper i)
            case (maybeAbstr) of
                Nothing -> reduceSimpleApp dt app
                Just abst -> reduce dt (Application abst p)

        -- редекс
        (Application (Abstraction s a) b) -> do
            let unfoldA = ($!) unfoldExp map a
            let unfoldB = ($!) unfoldExp map b
            let fvsB = fv unfoldB
            let (substExpr, newDt) = substitute unfoldA dt s (Wrapper curK) True fvsB Map.empty
            let (Dt map1 curK1 varId1) = newDt
            let newMap = ($!) Map.insert curK b map1
            let resDt = (Dt newMap curK1 varId1)
            (Reducted substExpr, resDt)


        -- любой другой вариант
        (Application left right) -> reduceSimpleApp dt (Application left right)


reduceSimpleApp :: Dt -> Expression -> (Reduction, Dt)
reduceSimpleApp dt (Application left right) = do
    let (leftMaybeReducted, lDt) = ($!) reduce dt left
    case leftMaybeReducted of
        Reducted re -> (Reducted (Application re right), lDt)
        _ -> do
            let (rightMaybeReducted, rDt) = reduce lDt right -- check
            case rightMaybeReducted of
                Reducted re -> (Reducted (Application left re), rDt)
                None re -> (None (Application left re), rDt)

substitute :: Expression -> Dt -> String -> Expression -> Bool -> (Set.Set String) -> (Map.Map String String) -> (Expression, Dt)
substitute (Abstraction s p) dt@(Dt map curK varId) varName term shouldSubst fvs namesMap = do
    if (s /= varName) then do
        case (Set.member s fvs) of
            True -> do
                let newId = varId + 1
                let newVar = ($!) (++) "v" (show varId)
                let newNames = ($!) Map.insert s newVar namesMap
                let newDt = (Dt map curK newId)
                let (substExpr, resDt) = substitute p newDt varName term shouldSubst fvs newNames
                (Abstraction newVar substExpr, resDt)
            False -> do
                let (substExpr, resDt) = substitute p dt varName term shouldSubst fvs namesMap
                (Abstraction s substExpr, resDt)
    else do 
        let (substExpr, resDt) = substitute p dt varName term False fvs namesMap  
        (Abstraction s substExpr, resDt)

substitute (Application p q) dt varName term shouldSubst fvs namesMap = do
    let (leftSubst, lDt) = ($!) substitute p dt varName term shouldSubst fvs namesMap
    let (rightSubst, rDt) = ($!) substitute q lDt varName term shouldSubst fvs namesMap
    ((Application leftSubst rightSubst), rDt)

substitute (Var s) dt varName term shouldSubst fvs namesMap = do
    if (s /= varName || not shouldSubst) then
        case (Map.lookup s namesMap) of
            Nothing -> (Var s, dt)
            (Just realName) -> (Var realName, dt)
    else (term, dt)


unfoldExp :: (Map.Map Int Expression) -> Expression -> Expression
unfoldExp map exp = do
    case exp of
        (Abstraction x p) -> (Abstraction x (unfoldExp map p))
        (Var s) -> Var s
        (Application p q) -> (Application (unfoldExp map p) (unfoldExp map q))
        (Wrapper v) -> unfoldExp map (unwrap $ ($!) Map.lookup v map)
    

fv :: Expression -> (Set.Set String)
fv expr = case expr of 
    Var s -> Set.singleton s
    Application exp1 exp2 -> ($!) Set.union (fv exp1) (fv exp2)
    (Abstraction var exp) -> ($!) Set.delete var (fv exp)


checkIfAbstr :: (Map.Map Int Expression)  -> Expression -> Maybe Expression
checkIfAbstr map (Wrapper ind) = do
    let realExp = unwrap (Map.lookup ind map)
    case realExp of
        (Abstraction x p) -> Just (Abstraction x p)
        (Wrapper i) -> checkIfAbstr map (Wrapper i)
        _ -> Nothing


---
unwrap :: Maybe a -> a
unwrap (Just n) = n
unwrap Nothing = error "Nothing has no value"