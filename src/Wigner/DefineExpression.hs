{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Wigner.DefineExpression(
    operatorFuncIx, operatorFunc, operatorIx, operator,
    matrixFuncIx, matrixFunc, matrixIx, matrix,
    functionIx, function, constantIx, constant,
    differentialFuncIx, differentialIx, differential,
    zero, one, i,
    symmetric,
    expr2by2
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

matrixFuncIx :: Symbol -> [Index] -> [Function] -> Expr
matrixFuncIx s i v = makeExpr $ Mat $ Element s i v
matrixFunc s = matrixFuncIx s []
matrixIx s i = matrixFuncIx s i []
matrix s = matrixFuncIx s [] []

constantIx s i = functionIx s i []
constant s = functionIx s [] []

differentialFuncIx :: Symbol -> [Index] -> [Function] -> Expr
differentialFuncIx s i v = makeExpr $ Diff $ Func $ Element s i v
differentialIx s i = differentialFuncIx s i []
differential s = differentialFuncIx s [] []

zero = 0 :: Expr
one = 1 :: Expr
i = makeExpr (0 :+ 1 :: Complex Rational)

-- Changes all NormalProducts to Symmetric ones (without changing anything inside).
-- Use if you want to define symmetric product.
symmetric :: Expr -> Expr
symmetric = mapOpFactors asSym where
    asSym (NormalProduct ops) = makeExpr $ SymmetricProduct (fromFactors (factors ops))
    asSym opf = makeExpr opf

expr2by2 :: Expr -> Expr -> Expr -> Expr -> Expr
expr2by2 e1 e2 e3 e4 = Expr2by2 e1 e2 e3 e4
