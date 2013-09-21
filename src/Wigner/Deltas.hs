module Wigner.Deltas(
    sameSymbol,
    makeIndexDelta,
    makeVariableDelta,
    makeDeltas
    ) where

import qualified Data.List as L
import qualified Wigner.DefineExpression as D
import qualified Wigner.Symbols as S
import Wigner.Expression
import Wigner.ExpressionHelpers

delta = S.delta

differences :: Eq a => [a] -> [a] -> [(a, a)]
differences x y = filter (uncurry (/=)) (zip x y)

makeIndexDelta :: (Index, Index) -> Expr
makeIndexDelta (IndexInt x, IndexInt y) = if x /= y
    then D.zero
    else D.one
makeIndexDelta (x, y) = makeExpr (Func (Element delta (L.sort [x, y]) []))

makeVariableDelta :: (Function, Function) -> Expr
makeVariableDelta (x, y) = makeExpr (Func (Element delta [] [y, x]))

sameSymbol :: Element -> Element -> Bool
sameSymbol (Element s1 _ _) (Element s2 _ _) = s1 == s2

makeDeltas :: Element -> Element -> Expr
-- uncomment when different components have different basis sets,
-- and therefore restricted deltas depend on the component
--makeDeltas (Element s1 [i1] [v1]) (Element s2 [i2] [v2])
--    | (s1 /= s2) || (i1 /= i2) = makeExpr (0 :: Integer)
--    | otherwise = makeExpr (Func (Element delta [i1] [v2, v1]))
makeDeltas (Element s1 i1 v1) (Element s2 i2 v2) = product deltas where
        deltas = map makeIndexDelta indices_diff ++ map makeVariableDelta variables_diff
        indices_diff = differences i1 i2
        variables_diff = zip v1 v2
