{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Wigner.XmdsInteraction(
    xmdsGenerateCode,
    PythonExpression(..),
    ResultValue(..)
    ) where

import Data.Maybe
import Data.Ratio

import Wigner.Complex
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
variableForFunction (ConjFunc e) = 'c' : variableForFunction (Func e)

variableForExpectation :: FuncFactor -> String
variableForExpectation (FuncExpectation fs) = L.intercalate "_" (L.map processFactor (factors fs)) where
    processFactor (f, 1) = variableForFunction f
    processFactor (f, p) = variableForFunction f ++ "_" ++ show p

realPart = (++) "re_"
imagPart = (++) "im_"

xmdsMoments :: FuncFactor -> String
xmdsMoments x = unwords [realPart var, imagPart var] where
    var = variableForExpectation x

xmdsCalculationString :: FuncFactor -> String
xmdsCalculationString (FuncExpectation fs) = L.intercalate " * " factorExpressions where
    factorExpressions = L.map processFactor (factorsExpanded fs)
    processFactor (f@(Func e)) = variableForFunction f
    processFactor (f@(ConjFunc e)) = "conj(" ++ variableForFunction (Func e) ++ ")"

xmdsCalculationCode :: S.Set FuncFactor -> [String]
xmdsCalculationCode s = L.map calculationLine (S.elems s) where
    calculationLine f = realPart var_name ++ " = (" ++ var_calc ++ ").Re();\n" ++
                        imagPart var_name ++ " = (" ++ var_calc ++ ").Im();" where
        var_name = variableForExpectation f
        var_calc = xmdsCalculationString f

xmdsBlock :: [Expr] -> String
xmdsBlock exprs = "<moments>\n" ++ unwords moments ++ "\n</moments>\n" ++
        "<dependencies>main</dependencies><![CDATA[\n" ++
        unlines momentsExpressions ++ "]]>" where
    exps = S.unions (L.map extractExpectations exprs)
    moments = L.map xmdsMoments (S.elems exps)
    momentsExpressions = xmdsCalculationCode exps

data ResultValue = RealValue | ComplexValue
data PythonExpression = Result ResultValue Expr String
                      | Lambda ResultValue Expr String [String]
                      | UserCalculation String
type ConstantsMap = M.Map Function String

makeConstantsMap :: [(Expr, String)] -> ConstantsMap
makeConstantsMap constants = M.fromList (L.map (\(x, s) -> (exprToFunction x, s)) constants) where
    exprToFunction (Expr s) = head $ L.map (\(_, x) -> termToFunction x) (terms s)
    termToFunction (Term _ fs) = groupToFunction (head fs)
    groupToFunction (FuncProduct fs) = factorToFunction (head (factorsExpanded fs))
    factorToFunction (Factor f) = f

xmdsGenerateCode :: [(Expr, String)] -> [PythonExpression] -> String
xmdsGenerateCode constants pyexprs = "XMDS code:\n\n" ++
        xmdsBlock ((L.map getExprs . L.filter removeUserPart) pyexprs) ++
        "\n\nPython code:\n\n" ++
        pythonBlock constants_map pyexprs where
    constants_map = makeConstantsMap constants
    getExprs (Result _ expr _) = expr
    getExprs (Lambda _ expr _ _) = expr
    removeUserPart (UserCalculation _) = False
    removeUserPart _ = True

valueConverter :: ResultValue -> String -> String
valueConverter RealValue s = "numpy.real(" ++ s ++ ")"
valueConverter ComplexValue s = s

pythonBlock :: ConstantsMap -> [PythonExpression] -> String
pythonBlock constants pyexprs = unlines (L.map (pythonExpr constants) pyexprs) where
    pythonExpr _ (UserCalculation s) = s ++ " = NotImplementedError()"
    pythonExpr constants (Result val expr s) = s ++ " = " ++ valueConverter val (showPython constants expr)
    pythonExpr constants (Lambda val expr s vars) = s ++ " = lambda " ++
        L.intercalate ", " vars ++ ": " ++ valueConverter val (showPython constants expr)


class PythonShowable a where
    showPython :: ConstantsMap -> a -> String

instance PythonShowable Expr where
    showPython constants (Expr s)
        | emptySum s = "0"
        | otherwise = L.intercalate " + \\\n\t" (L.map showTerm (terms s)) where
        showTerm (c, t)
            | c == 1 && t == identityTerm = "1"
            | c == 1 = showPython constants t
            | t == identityTerm = "(" ++ showPython constants c ++ ")"
            | otherwise = showTerm (c, identityTerm) ++ " * " ++ showPython constants t
instance PythonShowable Coefficient where
    showPython constants (Coefficient c) = showPython constants c
instance PythonShowable (Complex Rational) where
    showPython constants (x :+ y)
        | y == 0 = showPython constants x
        | x == 0 = "1j * " ++ showPython constants y
        | otherwise = showPython constants x ++ " + 1j * " ++ showPython constants y
instance PythonShowable Rational where
    showPython _ x = "float(" ++ show n ++ ") / " ++ show d where
		n = numerator x
		d = denominator x
instance PythonShowable Term where
    showPython constants (Term Nothing fs) = L.intercalate " * " (L.map (showPython constants) fs)
instance PythonShowable FuncGroup where
    showPython constants (FuncProduct fs) = L.intercalate " * " (L.map showFactor (factors fs)) where
        showFactor (x, p)
            | p == 1 = showPython constants x
            | otherwise = showPython constants x ++ " ** " ++ show p
instance PythonShowable FuncFactor where
    showPython constants (Factor f) = fromJust (M.lookup f constants)
    showPython _ (fe@(FuncExpectation fs)) = "data." ++ variableForExpectation fe
