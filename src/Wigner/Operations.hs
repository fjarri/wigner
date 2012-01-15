module Wigner.Operations where

    import Data.Map(Map)
    import Data.List(sort, zip)
    import Wigner.Expression
    import Wigner.Symbols(delta)

    differences :: Eq a => [a] -> [a] -> [(a, a)]
    differences x y = filter (\(u, v) -> (u == v)) (zip x y)
{-
    makeIndexDelta :: (Index, Index) -> Function
    makeIndexDelta (x, y) = Func (Element delta (sort [x, y]) []) 1

    makeVariableDelta :: (Variable, Variable) -> Function
    makeVariableDelta (x, y) = Func (Element delta [] (sort [x, y])) 1

    makeDeltas :: Element -> Element -> Expr
    makeDeltas (Element s1 i1 v1) (Element s2 i2 v2)
        | s1 /= s2 = makeExpr 0
        | otherwise = if null deltas
            then makeExpr 1
            else product deltas
        where
            indices_diff = differences i1 i2
            variables_diff = differences v1 v2
            deltas = (map makeIndexDelta indices_diff) ++ (map makeVariableDelta variables_diff)


    commutator :: Expr -> Expr -> Expr
    commutator (DaggerOperator x) (DaggerOperator y) = makeExpr 0
    commutator (Operator x) (DaggerOperator y) = makeDeltas x y
    commutator (DaggerOperator x) (Operator y) = - (makeDeltas x y)
    commutator (Operator x) (Operator y) = makeExpr 0
-}
{-
    variance :: (Map Symbol Symbol) -> (Sum OpTerm) -> (Sum OpTerm) -> (Sum FuncTerm)
    variance cs x y = (expectation cs (x * y) + expectation cs (y * x) -
        2 * (expectation cs x) * (expectation cs y)) / 2

    deltaSquared cs x = variance cs x x

    replaceSymmetricProducts expr = undefined
    toSymmetricProduct expr = undefined

    expectation cs x = replaceSymmetricProducts cs (toSymmetricProduct x)
-}