module Wigner.Expectations(
    expectation, variance, deltaSquared,
    wignerExpectation,
    evaluateExpectations,
    asExpectation
    ) where

import qualified Data.List as L
import qualified Wigner.DefineExpression as D
import qualified Wigner.Symbols as S
import Wigner.OperatorAlgebra
import Wigner.Expression


type TargetFormConverter = Expr -> Expr
wignerExpectation = toSymmetricProduct bosonicCommutationRelation


variance :: Expr -> Expr -> Expr
variance x y = (expectation (x * y) + expectation (y * x) -
    2 * (expectation x) * (expectation y)) / 2

deltaSquared :: Expr -> Expr
deltaSquared x = variance x x

expectation :: Expr -> Expr
expectation expr = mapOpFactors toExpectation expr where
    toExpectation opf = makeExpr (OpExpectation opf)

evaluateExpectations :: TargetFormConverter -> S.SymbolCorrespondence -> Expr -> Expr
evaluateExpectations tfc sc = (replaceExpectations sc) . (applyConverter tfc)


mapOpFactors :: (OpFactor -> Expr) -> Expr -> Expr
mapOpFactors f (Expr s) = sum (map (\(c, t) -> makeExpr c * processTerm t) (terms s)) where
    processTerm (Term Nothing fs) = makeExpr fs
    processTerm (Term (Just opf) fs) = makeExpr fs * f opf

mapFuncGroups :: (FuncGroup -> Expr) -> Expr -> Expr
mapFuncGroups f (Expr s) = sum (map (\(c, t) -> makeExpr c * processTerm t) (terms s)) where
    processTerm (Term opf fs) = product (map f fs) * makeExpr opf

mapFuncFactors :: (FuncFactor -> Expr) -> Expr -> Expr
mapFuncFactors f expr = mapFuncGroups processGroup expr where
    processGroup (FuncProduct ffs) = product (map f (factorsExpanded ffs))
    processGroup x = makeExpr x

-- Applies converter to all operator products inside expectations
applyConverter :: TargetFormConverter -> Expr -> Expr
applyConverter tfc expr = mapFuncFactors convert expr where
    convert (OpExpectation opf) = expectation (tfc (makeExpr opf))
    convert x = makeExpr x

-- Replaces OpExpectations with FuncExpectations
replaceExpectations ::S.SymbolCorrespondence -> Expr -> Expr
replaceExpectations sc expr = mapFuncFactors convert expr where
    convert (OpExpectation opf) = makeExpr (FuncExpectation (processFactor opf))
    convert x = makeExpr x
    replaceOperators ops = fromFactorsExpanded (map (opToFunc sc) (factorsExpanded ops))
    processFactor (NormalProduct ops) = replaceOperators ops
    processFactor (SymmetricProduct ops) = replaceOperators ops
    opToFunc sc (Op e) = Func (S.mapElementWith sc e)
    opToFunc sc (DaggerOp e) = ConjFunc (S.mapElementWith sc e)

asExpectation :: Expr -> Expr
asExpectation expr = mapFuncGroups processGroup expr where
    processGroup (DiffProduct _) = error "Not implemented: differentials inside expectation"
    processGroup (FuncProduct ffs) = makeExpr (FuncExpectation (fromFactorsExpanded
        (map processFactor (factorsExpanded ffs))))
    processFactor (Factor f) = f
    processFactor _ = error "Not implemented: expectations of expectations"
