{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Wigner.ExpressionHelpers(
    makeExpr, mapTerms,
    mapOpFactors, mapFuncGroups, mapFuncFactors
    ) where

import Wigner.Expression
import Wigner.Complex


mapTerms :: (Term -> Expr) -> Expr -> Expr
mapTerms f (Expr s) = sum (map (\(c, t) -> makeExpr c * f t) (terms s))

mapOpFactors :: (OpFactor -> Expr) -> Expr -> Expr
mapOpFactors f = mapTerms processTerm where
    processTerm (Term Nothing fs) = makeExpr fs
    processTerm (Term (Just opf) fs) = makeExpr fs * f opf

mapFuncGroups :: (FuncGroup -> Expr) -> Expr -> Expr
mapFuncGroups f = mapTerms processTerm where
    processTerm (Term opf fs) = product (map f fs) * makeExpr opf

mapFuncFactors :: (FuncFactor -> Expr) -> Expr -> Expr
mapFuncFactors f = mapFuncGroups processGroup where
    processGroup (FuncProduct ffs) = product (map f (factorsExpanded ffs))
    processGroup x = makeExpr x


exprFromTerm t = Expr (fromTerms [(1 :: Coefficient, t)])
productFromFactor f = fromFactors [(f, 1)]

class Expressable a where
    makeExpr :: a -> Expr

instance Expressable a => Expressable [a] where
    makeExpr xs = product (map makeExpr xs)

instance Expressable Integer where makeExpr x = fromInteger x :: Expr
instance Expressable Int where makeExpr x = makeExpr (fromIntegral x :: Integer)
instance Expressable Rational where makeExpr x = fromRational x :: Expr
instance Expressable (Complex Rational) where makeExpr x = fromComplexRational x :: Expr
instance Expressable Coefficient where makeExpr x = Expr (fromCoeff x)
instance Expressable Differential where
    makeExpr x = exprFromTerm term where
        term = Term Nothing [diff_product]
        diff_product = DiffProduct (productFromFactor x)
instance Expressable Function where
     makeExpr x = exprFromTerm term where
        term = Term Nothing [func_product]
        func_product = FuncProduct (productFromFactor (Factor x))
instance Expressable Operator where
    makeExpr x = exprFromTerm term where
        term = Term (Just op_product) []
        op_product = NormalProduct (productFromFactor x)
instance Expressable Matrix where
    makeExpr x = exprFromTerm term where
        term = Term (Just mat_product) []
        mat_product = MatProduct (productFromFactor x)
instance Expressable OpFactor where
    makeExpr x = exprFromTerm (Term (Just x) [])
instance Expressable (Maybe OpFactor) where
    makeExpr Nothing = exprFromTerm identityTerm
    makeExpr (Just x) = makeExpr x
instance Expressable FuncGroup where
    makeExpr x = exprFromTerm (Term Nothing [x])
instance Expressable Term where
    makeExpr = exprFromTerm
instance Expressable FuncFactor where
    makeExpr x = exprFromTerm (Term Nothing [FuncProduct (productFromFactor x)])
