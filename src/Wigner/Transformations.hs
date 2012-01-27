module Wigner.Transformations(
    wignerTransformation,
    ) where

import Wigner.Expression
import Wigner.ExpressionHelpers
import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D


data OperatorPosition = Before | After
type FunctionCorrespondence = S.SymbolCorrespondence -> OperatorPosition -> Operator -> Expr

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
phaseSpaceTransformation f_corr s_corr op_kernel (Expr s) =
        sum (map processTerm (terms s)) where
    corr = f_corr s_corr

    isKernel (Op (Element op_kernel [] [])) = True
    isKernel _ = False

    processTerm (c, Term opf fs) = makeExpr c * makeExpr (Term Nothing fs) * processOpFactor opf

    processOpFactor Nothing = D.one
    processOpFactor (Just (SymmetricProduct _)) =
        error "Not implemented: phase-space transformation of symmetric operator product"
    processOpFactor (Just (NormalProduct ops)) =
        operatorsToFunctions (factorsExpanded ops)

    operatorsToFunctions [op]
        | isKernel op == True = D.one
        | otherwise = error "Kernel is missing from the expression"
    operatorsToFunctions (op:ops)
        | isKernel op == True = corr After (last ops) * operatorsToFunctions (op:(init ops))
        | otherwise = corr Before op * operatorsToFunctions ops

wignerTransformation :: S.SymbolCorrespondence -> Symbol -> Expr -> Expr
wignerTransformation = phaseSpaceTransformation wignerCorrespondence
