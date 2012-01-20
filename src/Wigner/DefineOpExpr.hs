{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Wigner.DefineOpExpr(
    operatorFuncIx, operatorFunc, operatorIx, operator,
    functionIx, function, constantIx, constant,
    zero, one, i, makeExpr, symmetricProduct, normalProduct
    ) where

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

normalProduct ops = product (map fromOperator ops)
symmetricProduct ops = asSymmetric (normalProduct ops)

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
