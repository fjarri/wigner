{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Wigner.DefineFuncExpr(
    functionIx, function, constantIx, constant,
    differentialFuncIx, differentialIx, differential,
    zero, one, i
    ) where

import Wigner.Complex
import Wigner.Expression
import qualified Data.Map as M

functionIx s i v = fromFunction $ Func $ Element s i v :: FuncExpr
function s = functionIx s []

constantIx s i = functionIx s i []
constant s = functionIx s [] []

differentialFuncIx s i v = fromDifferential $ Diff $ Element s i v :: FuncExpr
differentialIx s i = differentialFuncIx s i []
differential s = differentialFuncIx s [] []

zero = 0 :: FuncExpr
one = 1 :: FuncExpr
i = makeExpr (0 :+ 1 :: Complex Rational)

class Expressable a where
    makeExpr :: a -> FuncExpr

instance Expressable Int where makeExpr x = fromInteger (fromIntegral x :: Integer) :: FuncExpr
instance Expressable Rational where makeExpr x = fromRational x :: FuncExpr
instance Expressable (Complex Rational) where makeExpr x = fromComplexRational x :: FuncExpr
instance Expressable Differential where makeExpr = fromDifferential
instance Expressable Function where makeExpr = fromFunction
