module Wigner.Expectations(
    expectation, variance, deltaSquared,
    wignerExpectation,
    evaluateExpectations
    ) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Wigner.DefineOpExpr as DO
import qualified Wigner.DefineFuncExpr as DF
import qualified Wigner.Symbols as S
import Wigner.OperatorAlgebra
import Wigner.Expression


type TargetFormConverter = OpExpr -> OpExpr
wignerExpectation = toSymmetricProduct bosonicCommutationRelation


variance :: OpExpr -> OpExpr -> FuncExpr
variance x y = (expectation (x * y) + expectation (y * x) -
    2 * (expectation x) * (expectation y)) / 2

deltaSquared :: OpExpr -> FuncExpr
deltaSquared x = variance x x

expectation :: OpExpr -> FuncExpr
expectation expr = sum (map (toExpectation . splitOpTermCoeffFunc) (terms expr)) where
    toExpectation (f, Nothing) = f
    toExpectation (f, Just opf) = f * (toExpr (wrapWithExpectation opf))

evaluateExpectations :: TargetFormConverter -> S.SymbolCorrespondence -> FuncExpr -> FuncExpr
evaluateExpectations tfc sc = (replaceExpectations sc) . (applyConverter tfc)

-- Applies converter to all operator products inside expectations
applyConverter :: TargetFormConverter -> FuncExpr -> FuncExpr
applyConverter tfc expr = result where
    ts = terms expr
    (coeffs, tms) = unzip ts
    factors = map splitFuncTerm tms
    exprs = map (map (convertOpExpectation tfc)) factors
    exprs2 = map product exprs
    exprs3 = zip coeffs exprs2
    result = sum (map (\(c, x) -> DF.makeExpr c * x) exprs3)

convertOpExpectation :: TargetFormConverter -> FuncFactor -> FuncExpr
convertOpExpectation tfc (OpExpectation opf) = expectation (tfc (DO.makeExpr opf))
convertOpExpectation tfc ff = DF.makeExpr ff

-- Replaces OpExpectations with FuncExpectations
replaceExpectations ::S.SymbolCorrespondence -> FuncExpr -> FuncExpr
replaceExpectations sc expr = result where
    ts = terms expr -- [(Coefficient, FuncTerm)]
    (coeffs, tms) = unzip ts
    factors = map splitFuncTerm tms -- [[FuncFactor]]
    exprs = map (map (DF.makeExpr . (replaceOpExpectation sc))) factors -- [[FuncFactor]]
    exprs2 = map product exprs
    exprs3 = zip coeffs exprs2
    result = sum (map (\(c, x) -> DF.makeExpr c * x) exprs3)

replaceOpExpectation :: S.SymbolCorrespondence -> FuncFactor -> FuncFactor
replaceOpExpectation sc (OpExpectation (NormalProduct opf)) =
    FuncExpectation (fromFactorsExpanded (replaceOperators sc (factorsExpanded opf)))
replaceOpExpectation sc (OpExpectation (SymmetricProduct opf)) =
    FuncExpectation (fromFactorsExpanded (replaceOperators sc (factorsExpanded opf)))
replaceOpExpectation sc ff = ff

replaceOperators :: S.SymbolCorrespondence -> [Operator] -> [Function]
replaceOperators sc ops = map opToFunc ops where
    opToFunc (Op e) = Func (S.mapElementWith sc e)
    opToFunc (DaggerOp e) = ConjFunc (S.mapElementWith sc e)
