module Wigner.DefineFuncExpr(
    functionIx, function, constantIx, constant,
    differentialFuncIx, differentialIx, differential,
    fromOpExpr, i) where

    import Wigner.Complex
    import Wigner.Expression
    import qualified Data.Map as M

    makeExpr factor = Sum $ M.singleton (FuncTerm [factor]) 1

    functionIx s i v = makeExpr func_factor where
        func = Func $ Element s i v
        func_factor = FuncProduct $ M.singleton func 1
    function s = functionIx s []

    constantIx s i = functionIx s i []
    constant s = functionIx s [] []

    differentialFuncIx s i v = makeExpr func_factor where
        diff = Diff $ Element s i v
        func_factor = DiffProduct $ M.singleton diff 1
    differentialIx s i = differentialFuncIx s i []
    differential s = differentialFuncIx s [] []

    fromOpTerm (OpTerm fs Nothing)
        | M.null fs = FuncTerm []
        | otherwise = FuncTerm [FuncProduct fs]
    fromOpTerm opt = error "Cannot convert operators to functions"

    fromOpExpr :: Sum OpTerm -> Sum FuncTerm
    fromOpExpr (Sum ts) = Sum $ M.mapKeys fromOpTerm ts

    i = Sum $ M.singleton (identity :: FuncTerm) (Coefficient (0 :+ 1 :: ComplexRational))
