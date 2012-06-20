module Wigner.Trace(
    trace,
    normalizeTraces
    ) where

import Wigner.Expression
import Wigner.ExpressionHelpers


class Traceable a where
    trace :: a -> a

instance Traceable Expr where
    trace (Expr2by2 e11 e12 e21 e22) = trace e11 + trace e22
    trace expr@(Expr e) = mapOpFactors (makeExpr . toTrace) expr where
        toTrace opf@(MatProduct mats) = Trace opf
        toTrace _ = error "Not implemented: trace of operator product"


-- A specialized function that takes a trace Tr{ABCD}, and:
-- 1) if B==A^T: CD -> min(CD, (CD)^T),
-- 2) if C==D^T: AB -> min(AB, (AB)^T).
-- This helps match terms in FPE-SDE related calculations,
-- and, obviously, does not change the value of the trace
normalizeTraces :: Expr -> Expr
normalizeTraces expr@(Expr _) = (firstPass . secondPass) expr where
    firstPass = mapFuncFactors (normalizeTrace checkFirst)
    secondPass = mapFuncFactors (normalizeTrace checkSecond)

    normalizeTrace checkFunc tr@(Trace (MatProduct p))
        | length ms /= 4 = makeExpr tr
        | otherwise = trace (checkFunc ms)
        where
            ms = factorsExpanded p

checkFirst :: [Matrix] -> Expr
checkFirst [m1, m2, m3, m4] = if (transpose m1) == m2
    then first_half * (if (m3, m4) < (m4, m3) then second_half else second_swapped)
    else first_half * second_half
    where
        first_half = makeExpr m1 * makeExpr m2
        second_half = makeExpr m3 * makeExpr m4
        second_swapped = transpose second_half

checkSecond :: [Matrix] -> Expr
checkSecond [m1, m2, m3, m4] = if (transpose m3) == m4
    then (if (m1, m2) < (m2, m1) then first_half else first_swapped) * second_half
    else first_half * second_half
    where
        first_half = makeExpr m1 * makeExpr m2
        first_swapped = transpose first_half
        second_half = makeExpr m3 * makeExpr m4
