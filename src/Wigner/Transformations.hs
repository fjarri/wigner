module Wigner.Transformations(
    wignerTransformation,
    positivePTransformation,
    truncateDifferentials,
    showTexByDifferentials,
    wignerOfLossTerm,
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
import qualified Control.Arrow as A

data OperatorPosition = Before | After
type PhaseSpaceCorrespondence = OperatorPosition -> Operator -> Expr
type FunctionCorrespondence = S.SymbolCorrespondence -> PhaseSpaceCorrespondence
type FunctionCorrespondence2 = S.SymbolCorrespondence2 -> PhaseSpaceCorrespondence

-- In order to simplify resulting expressions we need to know which deltas are real-valued.
-- This function conjugates only complex-valued deltas in the expression
-- (which means those with different variables).
-- WARNING: works only with products of delta-functions;
-- all delta-functions with variables must be restricted delta functions
conjugateDeltas = mapFuncFactors processFactor where
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

positivePCorrespondence :: FunctionCorrespondence2
positivePCorrespondence s_corr2 Before (Op e) =
    makeExpr (Func ce1) where
        (ce1, ce2) = S.mapElementPairWith s_corr2 e
positivePCorrespondence s_corr2 Before (DaggerOp e) =
    makeExpr (Func ce2) - makeExpr (Diff (Func ce1)) where
        (ce1, ce2) = S.mapElementPairWith s_corr2 e
positivePCorrespondence s_corr2 After (Op e) =
    makeExpr (Func ce1) - makeExpr (Diff (Func ce2)) where
        (ce1, ce2) = S.mapElementPairWith s_corr2 e
positivePCorrespondence s_corr2 After (DaggerOp e) =
    makeExpr (Func ce2) where
        (ce1, ce2) = S.mapElementPairWith s_corr2 e


phaseSpaceTransformation :: PhaseSpaceCorrespondence -> Symbol -> Expr -> Expr
phaseSpaceTransformation corr kernel expr =
        derivativesToFront $ mapOpFactors processOpFactor expr where

    isKernel (Op (Element sym [] [])) = sym == kernel
    isKernel _ = False

    processOpFactor (SymmetricProduct _) =
        error "Not implemented: phase-space transformation of symmetric operator product"
    processOpFactor (NormalProduct ops) =
        operatorsToFunctions (factorsExpanded ops)

    operatorsToFunctions [op]
        | isKernel op = D.one
        | otherwise = error "Kernel is missing from the expression"
    operatorsToFunctions (op:ops)
        | isKernel op = corr After (last ops) * operatorsToFunctions (op : init ops)
        | otherwise = corr Before op * operatorsToFunctions ops

derivativesToFront :: Expr -> Expr
derivativesToFront = mapTerms processTerm where
    processTerm (Term opf gs) = makeExpr (Term opf []) * processGroups gs

    processGroups [] = D.one
    processGroups [g] = makeExpr g
    processGroups (g@(DiffProduct _):gs) = makeExpr g * processGroups gs
    processGroups (FuncProduct fs : DiffProduct ds : gs) =
        derivativesToFront (mixGroups (factorsExpanded fs) (factorsExpanded ds) * makeExpr gs)

    mixGroups fs [d] = makeExpr (init fs) * (d_expr * f_expr - comm f d) where
        f = last fs
        d_expr = makeExpr d
        f_expr = makeExpr f
    mixGroups fs (d:ds) = mixGroups fs [d] * makeExpr ds

    comm (Factor f) = funcDiffCommutator f

wignerTransformation :: S.SymbolCorrespondence -> Symbol -> Expr -> Expr
wignerTransformation s_corr = phaseSpaceTransformation (wignerCorrespondence s_corr)

positivePTransformation :: S.SymbolCorrespondence2 -> Symbol -> Expr -> Expr
positivePTransformation s_corr2 = phaseSpaceTransformation (positivePCorrespondence s_corr2)

truncateDifferentials :: Int -> Expr -> Expr
truncateDifferentials n = mapTerms processTerm where
    processTerm t@(Term Nothing [DiffProduct ds, FuncProduct fs])
        | length (factorsExpanded ds) <= n = makeExpr t
        | otherwise = D.zero

showTexByDifferentials :: Expr -> String
showTexByDifferentials (Expr s) = unlines result_lines where
    processTerm (c, Term Nothing [DiffProduct ds, f@(FuncProduct fs)]) =
        (ds, makeExpr c * makeExpr f)
    diff_to_expr = M.fromListWith (+) (map processTerm (terms s))
    diffOrder (ds1, fs1) (ds2, fs2) = if ds1 /= ds2
        then compare (length (factorsExpanded ds1)) (length (factorsExpanded ds2))
        else compare ds1 ds2
    pairs = L.sortBy diffOrder (M.assocs diff_to_expr)
    shift s = unlines (map ("    " ++) (lines s))
    showPair (diffs, funcs) = diff_str ++ " \\left(\n" ++ shift func_str ++ "\\right) " where
            diff_str = showTex (makeExpr (DiffProduct diffs))
            func_str = showTex funcs
    result_lines = showPair (head pairs) : map (("+" ++) . showPair) (tail pairs)

-- Helper function which calculates a single term for the analytical loss term formula.
analyticalLossTerm :: S.SymbolCorrespondence -> [Operator] -> [Int] -> ([Int], [Int]) -> Expr
analyticalLossTerm corr ops ls (js, ks) = coeff * diff_product * func_product where

    -- Return function/differential with the same symbol as given operator
    funcForOp corr (Op e) = Func (S.mapElementWith corr e)
    diffForOp corr op = Diff (funcForOp corr op)

    -- Result of commutator [d/df, f], where function f corresponds to given operator
    delta (Op e) = makeDeltas e e

    -- Returns Integer, because the result of the expression
    -- using this result (in qTerm) can be quite big
    fact :: Int -> Integer
    fact n = product [1 .. (fromIntegral n :: Integer)]

    -- Creates product of differentials.
    constructDiffs :: (Operator, Int, Int) -> Expr
    constructDiffs (op, j, k) = (makeExpr . conjugate . diffForOp corr) op ^ j *
        (makeExpr . diffForOp corr) op ^ k

    -- Numerical coefficient for the product of functions.
    qTerm :: Int -> Int -> Int -> Int -> Expr
    qTerm l j k m = makeExpr ((-1) ^ m * fact l ^ 2) /
        makeExpr (fact j * fact k * fact m * fact (l - k - m) * fact (l - j - m) * 2 ^ (j + k + m))

    -- Functional term for given order of delta-function.
    constructFuncTerm :: Operator -> Int -> Int -> Int -> Int -> Expr
    constructFuncTerm op l j k m = qTerm l j k m * delta op ^ m *
        ((makeExpr . funcForOp corr) op ^ (l - j - m)) *
        ((makeExpr . conjugate . funcForOp corr) op ^ (l - k - m))

    -- Creates product of functions.
    constructFuncs :: (Operator, Int, Int, Int) -> Expr
    constructFuncs (op, l, j, k) = sum (map (constructFuncTerm op l j k) [0..l - max j k])

    coeff = makeExpr (2 - (-1) ^ sum js - (-1) ^ sum ks :: Int)
    diff_product = product (map constructDiffs (zip3 ops js ks))
    func_product = product (map constructFuncs (L.zip4 ops ls js ks))

-- Calculates Wigner transformation of loss term using analytical formula
-- WARNING: the following assumptions are made:
-- 1) operators in the expr commute with each other
-- 2) [d/df, f] for every operator is the same (usually it is either 1 or delta(x, x))
wignerOfLossTerm :: S.SymbolCorrespondence -> Expr -> Expr
wignerOfLossTerm corr expr = op_expr * func_expr where

    -- carthesian product for several lists
    carthesianProduct' :: [[a]] -> [[a]]
    carthesianProduct' [] = []
    carthesianProduct' [l] = [[x] | x <- l]
    carthesianProduct' (l:ls) = [x:xs | x <- l, xs <- carthesianProduct' ls]

    -- carthesian product for two lists
    -- (we could use more generic version, but it is easier to pattern match a tuple)
    carthesianProduct :: [a] -> [b] -> [(a, b)]
    carthesianProduct l1 l2 = [(x, y) | x <- l1, y <- l2]

    -- Splits initial expression into functional part and the sequence of operators
    extractOpFactors :: Expr -> (Expr, [(Operator, Int)])
    extractOpFactors (Expr s) = head (map processTerm (terms s)) where
        processTerm (c, Term (Just (NormalProduct ops)) fs) = (func_expr, op_list) where
            func_expr = makeExpr (Term Nothing fs) * makeExpr c
            op_list = factors ops

    (func_expr, op_list) = extractOpFactors expr
    (ops, ls) = unzip op_list
    js = carthesianProduct' (map (\x -> [0..x]) ls)
    ks = carthesianProduct' (map (\x -> [0..x]) ls)
    op_expr = sum (map (analyticalLossTerm corr ops ls) (carthesianProduct js ks))
