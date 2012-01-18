module Wigner.DefineOpExpr(
    operatorFuncIx, operatorFunc, operatorIx, operator,
    functionIx, function, constantIx, constant,
    fromFuncExpr, i) where

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

    fromFuncTerm (FuncTerm []) = OpTerm M.empty Nothing
    fromFuncTerm (FuncTerm [FuncProduct fp]) = OpTerm fp Nothing
    fromFuncTerm ft = error "Not implemented: conversion of non-function product to operator term"

    fromFuncExpr :: Sum FuncTerm -> Sum OpTerm
    fromFuncExpr (Sum ts) = Sum $ M.mapKeys fromFuncTerm ts

    i = Sum $ M.singleton (identity :: OpTerm) (Coefficient (0 :+ 1 :: ComplexRational))
