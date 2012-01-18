module Wigner.DefineFuncExpr(
    functionIx, function, constantIx, constant,
    fromOpExpr, i) where

    import Wigner.Complex
    import Wigner.Expression
    import qualified Data.Map as M

    functionIx s i v = Sum $ M.singleton func_term 1 where
        func = Func $ Element s i v
        func_factor = FuncProduct $ M.singleton func 1
        func_term = FuncTerm [func_factor]
    function s = functionIx s []

    constantIx s i = functionIx s i []
    constant s = functionIx s [] []

    fromOpTerm (OpTerm fs Nothing)
        | M.null fs = FuncTerm []
        | otherwise = FuncTerm [FuncProduct fs]
    fromOpTerm opt = error "Cannot convert operators to functions"

    fromOpExpr :: Sum OpTerm -> Sum FuncTerm
    fromOpExpr (Sum ts) = Sum $ M.mapKeys fromOpTerm ts

    i = Sum $ M.singleton (identity :: FuncTerm) (Coefficient (0 :+ 1 :: ComplexRational))
