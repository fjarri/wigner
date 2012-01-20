module Wigner.OperatorAlgebra(
    bosonicCommutationRelation,
    makeIndexDelta,
    makeVariableDelta,
    toNormalProduct,
    toSymmetricProduct
    ) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Wigner.DefineOpExpr as DO
import qualified Wigner.Symbols as S
import Wigner.Expression

delta = S.delta

differences :: Eq a => [a] -> [a] -> [(a, a)]
differences x y = filter (uncurry (/=)) (zip x y)

makeIndexDelta :: (Index, Index) -> Function
makeIndexDelta (x, y) = Func (Element delta (L.sort [x, y]) [])

makeVariableDelta :: (Variable, Variable) -> Function
makeVariableDelta (x, y) = Func (Element delta [] (L.sort [x, y]))

sameSymbol :: Element -> Element -> Bool
sameSymbol (Element s1 _ _) (Element s2 _ _) = s1 == s2

makeDeltas :: Element -> Element -> [Function]
makeDeltas (Element s1 i1 v1) (Element s2 i2 v2) =
    map makeIndexDelta indices_diff ++ map makeVariableDelta variables_diff where
        indices_diff = differences i1 i2
        variables_diff = differences v1 v2

commutator :: OpExpr -> OpExpr -> OpExpr
commutator x y = x * y - y * x

type CommutationRelation = Operator -> Operator -> OpExpr

bosonicCommutationRelation :: CommutationRelation
bosonicCommutationRelation (Op x) (Op y) = DO.zero
bosonicCommutationRelation (DaggerOp x) (DaggerOp y) = DO.zero
bosonicCommutationRelation (Op x) (DaggerOp y) = if sameSymbol x y
    then product (map DO.makeExpr (makeDeltas x y))
    else DO.zero
bosonicCommutationRelation (DaggerOp x) (Op y) = - bosonicCommutationRelation (Op x) (DaggerOp y)

-- Transform all operator products in the expression to normal products
toNormalProduct :: OpExpr -> OpExpr
toNormalProduct expr = sum (map (termToNP . splitOpTermCoeff) (terms expr)) where
    termToNP (f, Nothing) = f
    termToNP (f, Just (NormalProduct ops)) = f * DO.normalProduct (factorsExpanded ops)
    -- By definition: sum of all normal products made of permutations of the operators
    -- divided by the number of permutations.
    termToNP (f, Just (SymmetricProduct ops)) = f * sum (map DO.normalProduct pms) / pm_num where
        pms = L.permutations (factorsExpanded ops)
        pm_num = DO.makeExpr (length pms)

-- Transform all operator products in the expression to symmetric products
toSymmetricProduct :: CommutationRelation -> OpExpr -> OpExpr
toSymmetricProduct comm expr = sum (map (termToSP comm . splitOpTermCoeff) (terms expr)) where
    termToSP comm (f, Nothing) = f
    termToSP comm (f, Just (SymmetricProduct ops)) = f * DO.symmetricProduct (factorsExpanded ops)
    termToSP comm (f, Just (NormalProduct ops)) = f * opsToSP comm (factorsExpanded ops)

-- Given target operator, finds first occurence of target_op in operator product
-- and moves it closer to the beginning.
-- Returns new product, coefficient for lower order remainder and the remainder
swapOnFirstEncounter :: CommutationRelation -> Operator -> [Operator] -> ([Operator], OpExpr, [Operator])
swapOnFirstEncounter comm target_op (op1:op2:ops)
    -- if target is not found, traverse the list further
    | op2 /= target_op = (op1:swapped, coeff, op1:lo)
    -- if target is found, move it closer to the beginning of the list
    -- (which means closer to the place where this operator is located in target operator sequence)
    | otherwise = (op2:op1:ops, comm op1 op2, ops) where
        (swapped, coeff, lo) = swapOnFirstEncounter comm target_op (op2:ops)

-- Given target list of operators, make the other list of operators
-- a bit closer to it by swapping one pair of operators.
swapDifferent :: CommutationRelation -> [Operator] -> [Operator] -> ([Operator], OpExpr, [Operator])
swapDifferent comm [] [] = ([], DO.zero, [])
swapDifferent comm (op1:ops1) (op2:ops2)
    | op1 == op2 = (op2:swapped, coeff, op2:lo)
    -- Two lists are supposed to contain the same set of elements,
    -- so if op1 /= op2, it means that ops1 and ops2 are not empty,
    -- and ops2 has op1 somewhere inside.
    | otherwise = swapOnFirstEncounter comm op1 (op2:ops2) where
        (swapped, coeff, lo) = swapDifferent comm ops1 ops2

-- Given target list of operators, swap operators in the other list
-- until the target is reached.
-- Returns the difference between the other list and the target
-- in form of the expression.
permuteTerm :: CommutationRelation -> [Operator] -> [Operator] -> OpExpr
permuteTerm comm target ops
    | target == ops = DO.zero
    -- Each call to 'swapDifferent' moves us a bit closer to the target
    | otherwise = permuteTerm comm target swapped +
        coeff * DO.normalProduct lo where
        (swapped, coeff, lo) = swapDifferent comm target ops

-- Transform operator sequence to the symmetric product
opsToSP :: CommutationRelation -> [Operator] -> OpExpr
opsToSP comm [] = DO.one
opsToSP comm [op] = DO.symmetricProduct [op]
opsToSP comm target = same_order - lower_part where
    -- Expand operator list from the Maybe OpFactor
    -- We do not need to cover all possibilities,
    -- because we know that the second parameter is always Just NormalProduct
    expand (f, Just (NormalProduct ops)) = (f, factorsExpanded ops)

    -- Permutes list of operators until the high order term reaches target form
    -- and returns the result minus this high order term
    lowerPartForTerm (f, ops) = f * permuteTerm comm target ops

    -- Symmetric product will have the only term containing the same operators
    -- as the initial product.
    same_order = DO.symmetricProduct target
    -- If we transform this term back to normal form, it will contain all
    -- permutations of the initial list of operators
    from_symm = toNormalProduct same_order
    -- Transform the expression to the list of operator lists
    expanded_terms = map (expand . splitOpTermCoeff) (terms from_symm)
    -- Permute each of these lists until we reach the target form
    -- and sum the remainders
    lower_part_normal = sum $ map lowerPartForTerm expanded_terms
    -- The result for the initial operator sequence is the high order term
    -- minus the symmetric form of the remainders of all permutations.
    -- We are going to the next recursion step, with operator list length lower by 2
    -- (because two operators get swapped and replaced by delta symbol in 'swapDifferent')
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