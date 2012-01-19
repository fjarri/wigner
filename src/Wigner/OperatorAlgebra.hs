module Wigner.OperatorAlgebra where

    import qualified Data.Map as M
    import qualified Data.List as L
    import qualified Wigner.DefineOpExpr as DO
    import qualified Wigner.Symbols as S
    import Wigner.Expression

    delta = S.delta

    differences :: Eq a => [a] -> [a] -> [(a, a)]
    differences x y = filter (\(u, v) -> (u == v)) (zip x y)

    makeIndexDelta :: (Index, Index) -> Function
    makeIndexDelta (x, y) = Func (Element delta (L.sort [x, y]) [])

    makeVariableDelta :: (Variable, Variable) -> Function
    makeVariableDelta (x, y) = Func (Element delta [] (L.sort [x, y]))

    {-

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
    asSymmetricProduct (Sum ts) = Sum (M.mapKeys asSym ts) where
        asSym (OpTerm fs Nothing) = OpTerm fs Nothing
        asSym (OpTerm fs (Just (NormalProduct ops))) =
            OpTerm fs (Just (SymmetricProduct (M.fromListWith (+) ops)))
        asSym (OpTerm fs (Just (SymmetricProduct ops))) = OpTerm fs (Just (SymmetricProduct ops))

    expandOpList :: [(Operator, Int)] -> [(Operator, Int)]
    expandOpList [] = []
    expandOpList ((op, p):xs) = (take p (repeat (op, 1))) ++ expandOpList xs

    normalProduct :: [(Operator, Int)] -> OpExpr
    normalProduct ops = product (map singleOpSum ops) where
        singleOpSum op = Sum (M.singleton (OpTerm M.empty (Just (NormalProduct [op]))) 1)

    toNormalProduct (Sum ts) = sum (map toNormSum (M.assocs ts)) where
        toNormSum (ot@(OpTerm fs Nothing), c) = Sum $ M.singleton ot c
        toNormSum (OpTerm fs (Just opf), c) = sum (toNorm opf) *
            (Sum (M.singleton (OpTerm fs Nothing) c))
        toNorm (SymmetricProduct ops) = map (\x -> normalProduct x / pm_num) pms where
            pms = L.permutations (expandOpList (M.assocs ops))
            pm_num = DO.fromInt (length pms)
        toNorm (NormalProduct ops) = [normalProduct ops]


{-
    variance :: (Map Symbol Symbol) -> (Sum OpTerm) -> (Sum OpTerm) -> (Sum FuncTerm)
    variance cs x y = (expectation cs (x * y) + expectation cs (y * x) -
        2 * (expectation cs x) * (expectation cs y)) / 2

    deltaSquared cs x = variance cs x x

    replaceSymmetricProducts expr = undefined
    toSymmetricProduct expr = undefined

    expectation cs x = replaceSymmetricProducts cs (toSymmetricProduct x)
-}