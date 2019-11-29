module System where
import MyData
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map


solveSystem system = solve system Map.empty

solve system substedMap =
    case (any wrongEquation system) of
        True -> Nothing
        False -> do
           let prev = system
           let newSystem = filter atomsNotEqual . map rewrite . concat $ map doReduction system
           case (find (isSubstituted substedMap) newSystem) of
               Just (Equation l r) -> do
                   let newSubstedMap = ($!) Map.insert l True substedMap
                   let finalSystem = ($!) map (substitute (Equation l r)) newSystem
                   solve finalSystem newSubstedMap 
               Nothing -> do
                   case (length prev == length newSystem) of
                       True -> if (prev == newSystem) then Just(newSystem) else (solve newSystem substedMap)
                       False -> (solve newSystem substedMap)


wrongEquation eq = do
    case (eq) of
        Equation (Atom x) (Impl b c) -> containsAtomInTerm (Atom x) (Impl b c)
        _ -> False


containsAtomInTerm atom term = do
    case (term) of
        (Atom _) -> term == atom
        (Impl l r) -> (containsAtomInTerm atom l) || (containsAtomInTerm atom r)

atomsNotEqual eq = do 
    case (eq) of
        Equation (Atom x) (Atom y) | (x == y) -> False
        _ -> True

rewrite eq = do 
    case (eq) of
        Equation (Impl a b) (Atom c) -> (Equation (Atom c) (Impl a b))
        _ -> eq

doReduction eq = do
    case (eq) of
        Equation (Impl a b) (Impl c d) -> [(Equation a c), (Equation b d)]
        _ -> [eq]

isSubstituted mapOfSubsted eq  = do
    case (eq) of
        Equation (Atom x) b -> not (Map.member (Atom x) mapOfSubsted)
        _ -> False

substitute (Equation l r) (Equation el er) = do
    let formula = (Equation l r)
    let eq = (Equation el er)
    if (formula == eq) then eq 
        else (Equation (substituteRec formula el) (substituteRec formula er))

substituteRec (Equation l r) term = do
    case (term) of
        (Atom a) | (l == term) -> r
        (Impl a b) -> (Impl (substituteRec (Equation l r) a) (substituteRec (Equation l r) b))
        _ -> term


getSystemForExpression expr = getSystem expr 0 Map.empty Map.empty

-- s - always string (var name); p / q -- expressions
getSystem expr ind mapFree mapAbstr = do
    case (expr) of
        Var s -> do
            case (Map.lookup s mapAbstr) of
                Just x -> ([], (Atom x), ind + 1, mapFree, mapAbstr)
                Nothing -> case (Map.lookup s mapFree) of
                    Just y -> ([], (Atom y), ind + 1, mapFree, mapAbstr)
                    Nothing -> do
                        let newMapFree = ($!) Map.insert s (ind + 1) mapFree
                        ([], (Atom (ind + 1)), ind + 1, newMapFree, mapAbstr) 
        Abstraction s p -> do
            let newMapAbstr = ($!) Map.insert s (ind + 1) mapAbstr
            let (e, tp, newIndex, newMapFree, newMapAbstr2) = ($!) getSystem p (ind + 1) mapFree newMapAbstr
            let newMapAbstr3 = ($!) Map.delete s  newMapAbstr2
            (e, (Impl (Atom (ind + 1)) tp), newIndex, newMapFree, newMapAbstr3)
        Application p q -> do
            let (ep, tp, ind1, mapFree1, mapAbstr1) = ($!) getSystem p ind mapFree mapAbstr
            let (eq, tq, ind2, mapFree2, mapAbstr2) = ($!) getSystem q ind1 mapFree1 mapAbstr1
            let newE = Equation tp (Impl tq (Atom (ind2 + 1)))
            let e = newE : ((reverse ep) ++ eq)
            (e, Atom (ind2 + 1), (ind2 + 1), mapFree2, mapAbstr2)

