module Proofer where


import System
import MyData
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map


getProof expr solution mapFree = do
    let ass = getAssString solution mapFree
    let (proofList, _, _) = proof expr solution mapFree Map.empty ass "" 0
    proofList


getAssString solution mapFree = do
    let list = ($!) Map.toList mapFree
    intercalate ", " (map (assElementString solution) list)


-- get assumption string for one term
assElementString solution (k, a) = k ++ " : " ++ (byIndexToTypeString a solution)

-- get string type of Atom by index
byIndexToTypeString:: Int -> [Equation] -> String
byIndexToTypeString index solution = show $ finalSubstitution (Atom index) solution



-------

proof expr solution mapFree mapAbstr ass treeLevelString ind = do
    let assLength = ($!) length ass
    case (expr) of
        Var s -> do
            let id = case (Map.lookup s mapAbstr) of
                    Just i -> i
                    Nothing -> unwrap (Map.lookup s mapFree)
            let sType = ($!) finalSubstitution (Atom id) solution
            let proofLine = treeLevelString ++ ass ++ " |- " ++ s ++ " : " ++ (show sType) ++ " [rule #1]"
            ([proofLine], sType, (ind + 1))

        Application p q -> do
            let (pProof, pType, ind1) = proof p solution mapFree mapAbstr ass ("*   " ++ treeLevelString) ind
            let (qProof, qType, ind2) = proof q solution mapFree mapAbstr ass ("*   " ++ treeLevelString) ind1
            let nextProof = ($!) (++) pProof qProof
            let exprType = case (pType) of
                    Impl a b -> b
                    _ -> error "Smth went wrong :/"
            let exprString = show expr
            let space = if (assLength /= 0) then " " else ""
            let curProofLine = treeLevelString ++  ass ++ space ++ "|- " ++ exprString ++ " : " ++ (show exprType) ++ " [rule #2]"
            (($!)(:) curProofLine nextProof, exprType, ind2 + 1)

        Abstraction s p -> do
            let sep = if (assLength /= 0) then  ", " else ""
            let newMapAbstr = ($!) Map.insert s (ind + 1) mapAbstr
            let sType = finalSubstitution (Atom (ind + 1)) solution
            let sTypeString = show sType
            let newAss = ass ++ sep ++ s ++ " : " ++ sTypeString
            
            let (nextProof, pType, newInd) = proof p solution mapFree newMapAbstr newAss ("*   " ++ treeLevelString) (ind + 1)
           
            let exprString = show expr
            let space = if (assLength /= 0) then " " else ""
            let curProofLine = treeLevelString ++  ass ++ space ++ "|- " ++ exprString ++ " : " ++ "(" ++ (show sType) ++ " -> " ++ (show pType) ++ ") [rule #3]"
            (($!)(:) curProofLine nextProof, (Impl sType pType), newInd)





----
finalSubstitution::  AlgebraicType -> [Equation] -> AlgebraicType
finalSubstitution term solution = do
    case (term) of
        Atom i -> case (find (leftPartEqual term) solution) of
            Just (Equation le re) -> re
            Nothing -> Atom i
        Impl a b -> (Impl (finalSubstitution a solution) (finalSubstitution b solution))


leftPartEqual term (Equation l r) = term == l


---
unwrap (Just n) = n
unwrap Nothing = error "Nothing has no value"