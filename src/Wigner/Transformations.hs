module Wigner.Transformations(
    wignerTransformation,
    truncateDifferentials,
    showTexByDifferentials,
    ) where

import Wigner.Complex
import Wigner.Expression
import Wigner.Deltas
import Wigner.ExpressionHelpers
import Wigner.Texable
import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D

import qualified Data.Map as M
import qualified Data.List as L

data OperatorPosition = Before | After
type FunctionCorrespondence = S.SymbolCorrespondence -> OperatorPosition -> Operator -> Expr

-- In order to simplify resulting expressions we need to know which deltas are real-valued.
-- This function conjugates only complex-valued deltas in the expression
-- (which means those with different variables).
-- WARNING: works only with products of delta-functions;
-- all delta-functions with variables must be restricted delta functions
conjugateDeltas expr = mapFuncFactors processFactor expr where
    processFactor (Factor (ConjFunc e)) = makeExpr (Func e)
    processFactor (Factor f@(Func (Element s i []))) = makeExpr f
    processFactor (Factor (Func (Element s i [v1, v2])))
        = makeExpr (Func (Element s i [v2, v1]))

funcDiffCommutator :: Function -> Differential -> Expr
funcDiffCommutator (Func _) (Diff (ConjFunc _)) = D.zero
funcDiffCommutator (ConjFunc _) (Diff (Func _)) = D.zero
funcDiffCommutator f@(Func fe) d@(Diff (Func de)) =
    if sameSymbol fe de then makeDeltas fe de else D.zero
funcDiffCommutator f@(ConjFunc _) d@(Diff (ConjFunc _)) =
    conjugateDeltas $ funcDiffCommutator (conjugate f) (conjugate d)

wignerCorrespondence :: FunctionCorrespondence
wignerCorrespondence s_corr Before (Op e) =
    makeExpr (Func ce) + makeExpr (Diff (ConjFunc ce)) / 2 where
    ce = S.mapElementWith s_corr e
wignerCorrespondence s_corr Before (DaggerOp e) =
    makeExpr (ConjFunc ce) - makeExpr (Diff (Func ce)) / 2 where
    ce = S.mapElementWith s_corr e
wignerCorrespondence s_corr After (Op e) =
    makeExpr (Func ce) - makeExpr (Diff (ConjFunc ce)) / 2 where
    ce = S.mapElementWith s_corr e
wignerCorrespondence s_corr After (DaggerOp e) =
    makeExpr (ConjFunc ce) + makeExpr (Diff (Func ce)) / 2 where
    ce = S.mapElementWith s_corr e

phaseSpaceTransformation :: FunctionCorrespondence -> S.SymbolCorrespondence -> Symbol -> Expr -> Expr
phaseSpaceTransformation f_corr s_corr kernel expr =
        derivativesToFront $ mapOpFactors processOpFactor expr where
    corr = f_corr s_corr

    isKernel (Op (Element kernel [] [])) = True
    isKernel _ = False

    processOpFactor (SymmetricProduct _) =
        error "Not implemented: phase-space transformation of symmetric operator product"
    processOpFactor (NormalProduct ops) =
        operatorsToFunctions (factorsExpanded ops)

    operatorsToFunctions [op]
        | isKernel op == True = D.one
        | otherwise = error "Kernel is missing from the expression"
    operatorsToFunctions (op:ops)
        | isKernel op == True = corr After (last ops) * operatorsToFunctions (op:(init ops))
        | otherwise = corr Before op * operatorsToFunctions ops

derivativesToFront :: Expr -> Expr
derivativesToFront expr = mapTerms processTerm expr where
    processTerm (Term opf gs) = makeExpr (Term opf []) * processGroups gs

    processGroups [] = D.one
    processGroups [g] = makeExpr g
    processGroups (g@(DiffProduct _):gs) = makeExpr g * processGroups gs
    processGroups ((FuncProduct fs):(DiffProduct ds):gs) =
        derivativesToFront (mixGroups (factorsExpanded fs) (factorsExpanded ds) * makeExpr gs)

    mixGroups fs [d] = makeExpr (init fs) * (d_expr * f_expr - comm f d) where
        f = last fs
        d_expr = makeExpr d
        f_expr = makeExpr f
    mixGroups fs (d:ds) = mixGroups fs [d] * makeExpr ds

    comm (Factor f) d = funcDiffCommutator f d

wignerTransformation :: S.SymbolCorrespondence -> Symbol -> Expr -> Expr
wignerTransformation = phaseSpaceTransformation wignerCorrespondence

truncateDifferentials :: Int -> Expr -> Expr
truncateDifferentials n expr = mapTerms processTerm expr where
    processTerm t@(Term Nothing [DiffProduct ds, FuncProduct fs])
        | length (factorsExpanded ds) <= n = makeExpr t
        | otherwise = D.zero

showTexByDifferentials :: Expr -> String
showTexByDifferentials (Expr s) = unlines result_lines where
    processTerm (c, Term Nothing [DiffProduct ds, f@(FuncProduct fs)]) =
        (ds, makeExpr c * makeExpr f)
    diff_to_expr = M.fromListWith (+) (map processTerm (terms s))
    pairs = M.assocs diff_to_expr
    showPair (diffs, funcs) = diff_str ++ " \\left( " ++ func_str ++ " \\right) " where
            diff_str = showTex (makeExpr (DiffProduct diffs))
            func_str = showTex funcs
    result_lines = [showPair (head pairs)] ++ (map (("+ " ++) . showPair) (tail pairs))
