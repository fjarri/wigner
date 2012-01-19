{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Wigner.DefineOpExpr(
    operatorFuncIx, operatorFunc, operatorIx, operator,
    functionIx, function, constantIx, constant,
    i, symmetric,
    makeExpr) where

    import Wigner.Complex
    import Wigner.Expression
    import qualified Data.Map as M

    operatorFuncIx s i v = Sum $ M.singleton op_term 1 where
        op = Op $ Element s i v
        op_factor = Just $ NormalProduct [(op, 1)]
        op_term = OpTerm M.empty op_factor
    operatorFunc s = operatorFuncIx s []
    operatorIx s i = operatorFuncIx s i []
    operator s = operatorFuncIx s [] []

    functionIx s i v = Sum $ M.singleton op_term 1 where
        func = Func $ Element s i v
        op_term = OpTerm (M.singleton func 1) Nothing
    function s = functionIx s []

    constantIx s i = functionIx s i []
    constant s = functionIx s [] []

    i = Sum $ M.singleton (identity :: OpTerm) (Coefficient (0 :+ 1 :: ComplexRational))

    symmetric :: OpExpr -> OpExpr
    symmetric (Sum ts) = Sum (M.mapKeys asSym ts) where
        asSym (OpTerm fs Nothing) = OpTerm fs Nothing
        asSym (OpTerm fs (Just (NormalProduct ops))) =
            OpTerm fs (Just (SymmetricProduct (M.fromListWith (+) ops)))
        asSym (OpTerm fs (Just (SymmetricProduct ops))) = OpTerm fs (Just (SymmetricProduct ops))


    class Expressable a where
        makeExpr :: a -> OpExpr

    instance Expressable FuncTerm where
        makeExpr (FuncTerm []) = Sum $ M.singleton identity 1
        makeExpr (FuncTerm [FuncProduct fp]) = makeExpr fp
        makeExpr ft = error "Not implemented: conversion of non-function product to operator term"
    instance Expressable FuncExpr where
        makeExpr (Sum ts) = sum $ map (\(x, c) -> makeExpr x * makeExpr c) (M.assocs ts)
    instance Expressable OpTerm where makeExpr ot = Sum $ M.singleton ot 1
    instance Expressable OpFactor where makeExpr opf = makeExpr (OpTerm M.empty (Just opf))
    instance Expressable Operator where makeExpr op = makeExpr (NormalProduct [(op, 1)])
    instance Expressable Int where makeExpr x = fromInteger (fromIntegral x :: Integer) :: OpExpr
    instance Expressable (M.Map Function Int) where makeExpr fs = Sum $ M.singleton (OpTerm fs Nothing) 1
    instance Expressable Coefficient where makeExpr c = Sum $ M.singleton identity c
