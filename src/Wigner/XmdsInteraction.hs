module Wigner.XmdsInteraction(
    extractExpectations,
    variableForExpectation,
    xmdsBlock,
    ) where

import Wigner.Expression
import Wigner.Texable
import Data.Map as M
import Data.Set as S
import Data.List as L

extractExpectations :: Expr -> S.Set FuncFactor
extractExpectations (Expr s) = foldl processTerm S.empty (terms s) where
    processTerm accum (_, Term opf groups) = foldl processGroup accum groups
    processGroup accum (FuncProduct fs) = foldl processFactor accum (factors fs)
    processFactor accum (Factor f, p) = accum
    processFactor accum (f@(FuncExpectation fs), p) = S.insert f accum

variableForFunction :: Function -> String
variableForFunction (Func (Element e i _)) = elem_str ++ indices_str where
    elem_str = L.filter (/= '\\') (showTex e)
    indices_str = L.intercalate "" (L.map showTex i)
variableForFunction (ConjFunc e) = "c" ++ variableForFunction (Func e)

variableForExpectation :: FuncFactor -> String
variableForExpectation (FuncExpectation fs) = L.intercalate "_" (L.map processFactor (factors fs)) where
    processFactor (f, 1) = variableForFunction f
    processFactor (f, p) = variableForFunction f ++ "_" ++ (show p)

xmdsCalculationString :: FuncFactor -> String
xmdsCalculationString (FuncExpectation fs) = L.intercalate " * " factorExpressions where
    factorExpressions = (L.map processFactor (factorsExpanded fs))
    processFactor (f@(Func e)) = variableForFunction f
    processFactor (f@(ConjFunc e)) = "conj(" ++ variableForFunction (Func e) ++ ")"

xmdsCalculationCode :: S.Set FuncFactor -> [String]
xmdsCalculationCode s = L.map calculationLine (S.elems s) where
    calculationLine f = variableForExpectation f ++ " = " ++ xmdsCalculationString f ++ ";"

xmdsBlock :: [Expr] -> String
xmdsBlock exprs = "<moments>\n" ++ unwords moments ++ "\n</moments>\n" ++
        "<dependencies>main</dependencies><![CDATA[\n" ++
        unlines momentsExpressions ++ "]]>" where
    exps = S.unions (L.map extractExpectations exprs)
    moments = L.map variableForExpectation (S.elems exps)
    momentsExpressions = xmdsCalculationCode exps
