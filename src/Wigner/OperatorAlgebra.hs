module Wigner.OperatorAlgebra where

    import qualified Data.Map as M
    import qualified Data.List as L
    import qualified Wigner.DefineOpExpr as DO
    import qualified Wigner.Symbols as S
    import Wigner.Expression

    delta = S.delta
    zeroExpr = fromInteger 0 :: OpExpr

    differences :: Eq a => [a] -> [a] -> [(a, a)]
    differences x y = filter (\(u, v) -> (u /= v)) (zip x y)

    makeIndexDelta :: (Index, Index) -> Function
    makeIndexDelta (x, y) = Func (Element delta (L.sort [x, y]) [])

    makeVariableDelta :: (Variable, Variable) -> Function
    makeVariableDelta (x, y) = Func (Element delta [] (L.sort [x, y]))

    sameSymbol :: Element -> Element -> Bool
    sameSymbol (Element s1 _ _) (Element s2 _ _) = s1 == s2

    makeDeltas :: Element -> Element -> [Function]
    makeDeltas (Element s1 i1 v1) (Element s2 i2 v2) =
        (map makeIndexDelta indices_diff) ++ (map makeVariableDelta variables_diff) where
            indices_diff = differences i1 i2
            variables_diff = differences v1 v2

    commutator :: OpExpr -> OpExpr -> OpExpr
    commutator x y = x * y - y * x

    type CommutationRelation = Operator -> Operator -> OpExpr

    bosonicCommutationRelation :: CommutationRelation
    bosonicCommutationRelation (Op x) (Op y) = zeroExpr
    bosonicCommutationRelation (DaggerOp x) (DaggerOp y) = zeroExpr
    bosonicCommutationRelation (Op x) (DaggerOp y) = if sameSymbol x y
        then DO.makeExpr $ M.fromList $ collapseProduct (makeDeltas x y)
        else zeroExpr
    bosonicCommutationRelation (DaggerOp x) (Op y) = - bosonicCommutationRelation (Op x) (DaggerOp y)


    toNormalProduct (Sum ts) = sum (map termToNP (M.assocs ts)) where
        termToNP (ot@(OpTerm fs Nothing), c) = DO.makeExpr ot * DO.makeExpr c
        termToNP (OpTerm fs (Just opf), c) = opFactorToNP opf * DO.makeExpr fs * DO.makeExpr c

        opFactorToNP (opf@(NormalProduct ops)) = DO.makeExpr opf -- mulTuples ops
        opFactorToNP (SymmetricProduct ops) = (sum $ map (product . (map DO.makeExpr)) pms) / pm_num where
            pms = L.permutations (expandProduct (M.assocs ops))
            pm_num = DO.makeExpr (length pms)

    expandProduct :: [(a, Int)] -> [a]
    expandProduct x = L.intercalate [] (map (\(op, p) -> replicate p op) x)

    collapseProduct :: Eq a => [a] -> [(a, Int)]
    collapseProduct x = map (\x -> (head x, L.length x)) (L.group x)

    toSymmetricProduct :: CommutationRelation -> OpExpr -> OpExpr
    toSymmetricProduct comm (Sum ts) = sum (map (termToSP comm) (M.assocs ts))

    termToSP comm (ot@(OpTerm fs Nothing), c) = DO.makeExpr ot * DO.makeExpr c
    termToSP comm (OpTerm fs (Just opf), c) = opFactorToSP comm opf * DO.makeExpr fs * DO.makeExpr c

    terms (Sum ts) = M.assocs ts
    extractOps (OpTerm fs (Just (NormalProduct ops))) = ops

    -- Given target operator, finds first occurence of target_op in operator product
    -- and moves it closer to the beginning.
    -- Returns new product, coefficient for lower order remainder and the remainder
    swapOnFirstEncounter :: CommutationRelation -> Operator -> [Operator] -> ([Operator], OpExpr, [Operator])
    swapOnFirstEncounter comm target_op (op1:op2:ops)
        | op2 /= target_op = (op1:swapped, coeff, op1:lo)
        | otherwise = (op2:op1:ops, comm op1 op2, ops) where
            (swapped, coeff, lo) = swapOnFirstEncounter comm target_op (op2:ops)

    swapDifferent :: CommutationRelation -> [Operator] -> [Operator] -> ([Operator], OpExpr, [Operator])
    swapDifferent comm [] [] = ([], zeroExpr, [])
    swapDifferent comm (op1:ops1) (op2:ops2)
        | op1 == op2 = (op2:swapped, coeff, op2:lo)
        -- two lists are supposed to contain the same set of elements,
        -- so if op1 /= op2, it means that ops1 and ops2 are not empty,
        -- and ops2 has op1 somewhere inside
        | otherwise = swapOnFirstEncounter comm op1 (op2:ops2) where
            (swapped, coeff, lo) = swapDifferent comm ops1 ops2

    permuteTerm :: CommutationRelation -> [Operator] -> [Operator] -> OpExpr
    permuteTerm comm target ops
--        | (not $ (null (target L.\\ ops))) = error (show target ++ "\n" ++ show ops)
        | target == ops = zeroExpr
        | otherwise = (permuteTerm comm target swapped) +
            coeff * DO.makeExpr (NormalProduct $ collapseProduct lo) where
            (swapped, coeff, lo) = swapDifferent comm target ops

    opFactorToSP :: CommutationRelation -> OpFactor -> OpExpr
    opFactorToSP comm (opf@(SymmetricProduct ops)) = DO.makeExpr opf
    opFactorToSP comm (NormalProduct []) = DO.unit
    opFactorToSP comm (opf@(NormalProduct [op])) = DO.symmetric (DO.makeExpr opf)
    opFactorToSP comm (opf@(NormalProduct ops)) = same_order - lower_part where
        same_order = DO.symmetric (DO.makeExpr opf)
        expanded_target = expandProduct ops -- [Operator]
        from_symm = toNormalProduct same_order -- OpExpr

        expand (term, c) = (expandProduct (extractOps term), c)
        expanded_terms = map expand (terms from_symm) -- [([Operator], Int)]

        lowerPartForTerm (ops, c) = (DO.makeExpr c) * (permuteTerm comm expanded_target ops)
        lower_part_normal = sum $ map lowerPartForTerm expanded_terms

        lower_part = toSymmetricProduct comm lower_part_normal

{-
    variance :: (Map Symbol Symbol) -> (Sum OpTerm) -> (Sum OpTerm) -> (Sum FuncTerm)
    variance cs x y = (expectation cs (x * y) + expectation cs (y * x) -
        2 * (expectation cs x) * (expectation cs y)) / 2

    deltaSquared cs x = variance cs x x

    replaceSymmetricProducts expr = undefined
    toSymmetricProduct expr = undefined

    expectation cs x = replaceSymmetricProducts cs (toSymmetricProduct x)
-}