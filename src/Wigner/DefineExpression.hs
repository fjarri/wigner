{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Wigner.DefineExpression(
    operatorFuncIx, operatorFunc, operatorIx, operator,
    functionIx, function, constantIx, constant,
    differentialFuncIx, differentialIx, differential,
    zero, one, i,
    symmetric
    ) where

import Wigner.Complex
import Wigner.Expression
import Wigner.ExpressionHelpers
import Data.Ratio

operatorFuncIx :: Symbol -> [Index] -> [Function] -> Expr
operatorFuncIx s i v = makeExpr $ Op $ Element s i v
operatorFunc s = operatorFuncIx s []
operatorIx s i = operatorFuncIx s i []
operator s = operatorFuncIx s [] []

functionIx :: Symbol -> [Index] -> [Function] -> Expr
functionIx s i v = makeExpr $ Func $ Element s i v
function s = functionIx s []

constantIx s i = functionIx s i []
constant s = functionIx s [] []

differentialFuncIx :: Symbol -> [Index] -> [Function] -> Expr
differentialFuncIx s i v = makeExpr $ Diff $ Func $ Element s i v
differentialIx s i = differentialFuncIx s i []
differential s = differentialFuncIx s [] []

--normalProduct :: [Operator] -> OpExpr
--normalProduct ops = product (map makeExpr ops)
--symmetricProduct ops = asSymmetric (normalProduct ops)

zero = 0 :: Expr
one = 1 :: Expr
i = makeExpr (0 :+ 1 :: Complex Rational)

-- Changes all NormalProducts to Symmetric ones (without changing anything inside).
-- Use if you want to define symmetric product.
symmetric :: Expr -> Expr
symmetric = mapOpFactors asSym where
    asSym (NormalProduct ops) = makeExpr $ SymmetricProduct (fromFactors (factors ops))
    asSym opf = makeExpr opf
