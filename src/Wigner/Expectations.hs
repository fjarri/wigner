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
import Wigner.ExpressionHelpers


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
