module Wigner.OperatorAlgebra(
    bosonicCommutationRelation,
    toNormalProduct,
    toSymmetricProduct
    ) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Wigner.DefineExpression as D
import qualified Wigner.Symbols as S
import Wigner.Expression
import Wigner.ExpressionHelpers
import Wigner.Deltas


type CommutationRelation = Operator -> Operator -> Expr

bosonicCommutationRelation :: CommutationRelation
bosonicCommutationRelation (Op _) (Op _) = D.zero
bosonicCommutationRelation (DaggerOp _) (DaggerOp _) = D.zero
bosonicCommutationRelation (Op x) (DaggerOp y) = if sameSymbol x y
    then makeDeltas x y
    else D.zero
bosonicCommutationRelation (DaggerOp x) (Op y) = - bosonicCommutationRelation (Op x) (DaggerOp y)

-- Transform all operator products in the expression to normal products
toNormalProduct :: Expr -> Expr
toNormalProduct = mapOpFactors factorToNP where
    -- By definition: sum of all normal products made of permutations of the operators
    -- divided by the number of permutations.
    factorToNP (SymmetricProduct ops) = sum (map (product . map makeExpr) pms) / pm_num where
        pms = L.permutations (factorsExpanded ops)
        pm_num = makeExpr (length pms)
    factorToNP opf = makeExpr opf

-- Transform all operator products in the expression to symmetric products
toSymmetricProduct :: CommutationRelation -> Expr -> Expr
toSymmetricProduct comm = mapOpFactors (factorToSP comm) where
    factorToSP comm (NormalProduct ops) = opsToSP comm (factorsExpanded ops)
    factorToSP comm opf = makeExpr opf

-- Given target operator, finds first occurence of target_op in operator product
-- and moves it closer to the beginning.
-- Returns new product, coefficient for lower order remainder and the remainder
swapOnFirstEncounter :: CommutationRelation -> Operator -> [Operator] -> ([Operator], Expr, [Operator])
swapOnFirstEncounter comm target_op (op1:op2:ops)
    -- if target is not found, traverse the list further
    | op2 /= target_op = (op1:swapped, coeff, op1:lo)
    -- if target is found, move it closer to the beginning of the list
    -- (which means closer to the place where this operator is located in target operator sequence)
    | otherwise = (op2:op1:ops, comm op1 op2, ops) where
        (swapped, coeff, lo) = swapOnFirstEncounter comm target_op (op2:ops)

-- Given target list of operators, make the other list of operators
-- a bit closer to it by swapping one pair of operators.
swapDifferent :: CommutationRelation -> [Operator] -> [Operator] -> ([Operator], Expr, [Operator])
swapDifferent comm [] [] = ([], D.zero, [])
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
permuteTerm :: CommutationRelation -> [Operator] -> [Operator] -> Expr
permuteTerm comm target ops
    | target == ops = D.zero
    -- Each call to 'swapDifferent' moves us a bit closer to the target
    | otherwise = permuteTerm comm target swapped +
        coeff * product (map makeExpr lo) where
        (swapped, coeff, lo) = swapDifferent comm target ops

-- Transform operator sequence to the symmetric product
opsToSP :: CommutationRelation -> [Operator] -> Expr
opsToSP comm [] = D.one
opsToSP comm [op] = D.symmetric (makeExpr op)
opsToSP comm target = same_order - lower_part where
    -- Symmetric product will have the only term containing the same operators
    -- as the initial product.
    same_order = D.symmetric (product (map makeExpr target))
    -- If we transform this term back to normal form, it will contain all
    -- permutations of the initial list of operators
    from_symm = toNormalProduct same_order
    -- The result for the initial operator sequence is the high order term
    -- minus the symmetric form of the remainders of all permutations.
    -- We are going to the next recursion step, with operator list length lower by 2
    -- (because two operators get swapped and replaced by delta symbol in 'swapDifferent').
    lowerPart (NormalProduct ops) = permuteTerm comm target (factorsExpanded ops)
    lower_part_normal = mapOpFactors lowerPart from_symm

    lower_part = toSymmetricProduct comm lower_part_normal
