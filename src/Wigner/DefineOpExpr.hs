{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Wigner.DefineOpExpr(
    operatorFuncIx, operatorFunc, operatorIx, operator,
    functionIx, function, constantIx, constant,
    zero, one, i, makeExpr, symmetric, normal) where

    import Wigner.Complex
    import Wigner.Expression
    import qualified Data.Map as M
    import Data.Ratio

    operatorFuncIx s i v = fromOperator $ Op $ Element s i v :: OpExpr
    operatorFunc s = operatorFuncIx s []
    operatorIx s i = operatorFuncIx s i []
    operator s = operatorFuncIx s [] []

    functionIx s i v = fromFunction $ Func $ Element s i v :: OpExpr
    function s = functionIx s []

    constantIx s i = functionIx s i []
    constant s = functionIx s [] []

    normal ops = product (map fromOperator ops)
    symmetric ops = asSymmetric (normal ops)

    zero = 0 :: OpExpr
    one = 1 :: OpExpr
    i = makeExpr (0 :+ 1 :: Complex Rational)

    class Expressable a where
        makeExpr :: a -> OpExpr

    instance Expressable Int where makeExpr x = fromInteger (fromIntegral x :: Integer) :: OpExpr
    instance Expressable Rational where makeExpr x = fromRational x :: OpExpr
    instance Expressable (Complex Rational) where makeExpr x = fromComplexRational x :: OpExpr
    instance Expressable Operator where makeExpr = fromOperator
    instance Expressable Function where makeExpr = fromFunction
    instance Expressable [Operator] where makeExpr x = product (map fromOperator x)
    instance Expressable [(Operator, Int)] where
        makeExpr pairs = product (map (\(x, p) -> fromOperator x ^ p) pairs)

{-    instance Expressable FuncTerm where
        makeExpr (FuncTerm []) = one
        makeExpr (FuncTerm [FuncProduct fp]) = makeExpr fp
        makeExpr ft = error "Not implemented: conversion of non-function product to operator term"
    instance Expressable FuncExpr where
        makeExpr (Sum ts) = sum $ map (\(x, c) -> makeExpr x * makeExpr c) (M.assocs ts)
    instance Expressable OpTerm where makeExpr ot = Sum $ M.singleton ot 1
    instance Expressable OpFactor where makeExpr opf = makeExpr (OpTerm M.empty (Just opf))
    instance Expressable Operator where makeExpr op = makeExpr (NormalProduct [(op, 1)])
    instance Expressable Int where makeExpr x = fromInteger (fromIntegral x :: Integer) :: OpExpr
    instance Expressable Rational where makeExpr x = fromRational x :: OpExpr
    instance Expressable (M.Map Function Int) where makeExpr fs = Sum $ M.singleton (OpTerm fs Nothing) 1
    instance Expressable Coefficient where makeExpr c = Sum $ M.singleton identity c
-}