{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module Wigner.Expression(
    Expr(..),
    Symbol(..),
    Index(..),
    Element(..),
    Function(..),
    Operator(..),
    Matrix(..),
    Differential(..),
    Term(..),
    OpFactor(..),
    FuncFactor(..),
    FuncGroup(..),
    Coefficient(..),
    terms, fromTerms, emptySum,
    identityTerm,
    fromCoeff,
    dagger, transpose,
    factors, factorsExpanded, fromFactors, fromFactorsExpanded
    ) where

import Wigner.Complex
import Wigner.Texable
import qualified Data.Map as M
import qualified Data.Tuple as T
import qualified Data.List as L
import qualified Control.Arrow as A


-- Data types

data Symbol = Symbol String deriving (Show, Eq, Ord)
data Index = IndexSymbol Symbol | IndexInt Int deriving (Show, Eq, Ord)
data Element = Element Symbol [Index] [Function] deriving (Show, Eq)
data Function = Func Element | ConjFunc Element deriving (Show, Eq)

data Operator = Op Element | DaggerOp Element deriving (Show, Eq)
data Differential = Diff Function deriving (Show, Eq, Ord)
data Matrix = Mat Element | TMat Element deriving (Show, Eq)

data Coefficient = Coefficient (Complex Rational) deriving (Show, Eq)

type SortedProduct a = M.Map a Int
type OrderedProduct a = [(a, Int)]
type SortedSum a = M.Map a Coefficient

data Expr = Expr (SortedSum Term)
          | Expr2by2 Expr Expr Expr Expr
          deriving (Show, Eq)
data Term = Term (Maybe OpFactor) [FuncGroup] deriving (Show, Eq, Ord)
data FuncGroup = DiffProduct (SortedProduct Differential)
               | FuncProduct (SortedProduct FuncFactor)
               deriving (Show, Eq, Ord)
data OpFactor = NormalProduct (OrderedProduct Operator)
              | SymmetricProduct (SortedProduct Operator)
              | MatProduct (OrderedProduct Matrix)
              deriving (Show, Eq, Ord)
data FuncFactor = Factor Function
                | FuncExpectation (SortedProduct Function)
                | OpExpectation OpFactor
                | Trace OpFactor
                deriving (Show, Eq, Ord)


-- Compositions/decompositions

identityTerm = Term Nothing []

class Sum a where
    terms :: a -> [(Coefficient, Term)]
    fromTerms :: [(Coefficient, Term)] -> a
    mapTermPairs :: ((Coefficient, Term) -> (Coefficient, Term)) -> a -> a
    mapCoefficients :: (Coefficient -> Coefficient) -> a -> a
    zeroSum :: a
    unitSum :: a
    emptySum :: a -> Bool
    fromCoeff :: Coefficient -> a

    mapTermPairs f x = fromTerms (map f (terms x))
    emptySum x = null (terms x)
    zeroSum = fromTerms []
    unitSum = fromTerms [(1 :: Coefficient, identityTerm)]
    fromCoeff c = fromTerms [(c, identityTerm)]

instance Sum (SortedSum Term) where
    terms x = map T.swap (M.assocs x)
    fromTerms x = M.filter (/= 0) (M.fromListWith (+) (map T.swap x))
    mapCoefficients = M.map


class Product a b | a -> b where
    factors :: a -> [(b, Int)]
    fromFactors :: [(b, Int)] -> a
    fromFactorsExpanded :: [b] -> a
    factorsExpanded :: a -> [b]
    mapFactors :: (b -> b) -> a -> a

    factorsExpanded x = L.intercalate [] (map (\(f, p) -> replicate p f) (factors x))
    fromFactorsExpanded x = fromFactors (map (\x -> (x, 1 :: Int)) x)
    mapFactors f x = fromFactors (map (A.first f) (factors x))

instance Ord a => Product (SortedProduct a) a where
    factors = M.assocs
    fromFactors = M.fromListWith (+)
instance Eq a => Product (OrderedProduct a) a where
    factors = id
    fromFactors fs = map (foldr1 mulTuples) (L.groupBy eqTest fs) where
        eqTest (x1, p1) (x2, p2) = x1 == x2
        mulTuples (x1, p1) (_, p2) = (x1, p1 + p2)


-- Ord instances

compareWithFallback :: Ord a => Ordering -> a -> a -> Ordering
compareWithFallback fb x y = if res == EQ then fb else res where
    res = compare x y

instance Ord Element where
    compare (Element s1 i1 v1) (Element s2 i2 v2) = compare (s1, v1, i1) (s2, v2, i2)
instance Ord Function where
    compare (Func e1) (Func e2) = compare e1 e2
    compare (Func e1) (ConjFunc e2) = compareWithFallback GT e1 e2
    compare (ConjFunc e1) (Func e2) = compareWithFallback LT e1 e2
    compare (ConjFunc e1) (ConjFunc e2) = compare e1 e2
instance Ord Operator where
    compare (Op e1) (Op e2) = compare e1 e2
    compare (Op e1) (DaggerOp e2) = compareWithFallback GT e1 e2
    compare (DaggerOp e1) (Op e2) = compareWithFallback LT e1 e2
    compare (DaggerOp e1) (DaggerOp e2) = compare e1 e2
instance Ord Matrix where
    compare (Mat e1) (Mat e2) = compare e1 e2
    compare (Mat e1) (TMat e2) = compareWithFallback GT e1 e2
    compare (TMat e1) (Mat e2) = compareWithFallback LT e1 e2
    compare (TMat e1) (TMat e2) = compare e1 e2


-- ComplexValued instances

instance ComplexValued a => ComplexValued (Maybe a) where
    conjugate Nothing = Nothing
    conjugate (Just x) = Just (conjugate x)
instance ComplexValued Function where
    conjugate (Func e) = ConjFunc e
    conjugate (ConjFunc e) = Func e
instance ComplexValued Differential where
    conjugate (Diff e) = Diff (conjugate e)
instance ComplexValued Coefficient where
    conjugate (Coefficient x) = Coefficient (conjugate x)
instance ComplexValued Expr where
    conjugate (Expr s) = Expr (mapTermPairs (conjugate A.*** conjugate) s)
instance ComplexValued Term where
    conjugate (Term opf fs) = Term (conjugate opf) (map conjugate fs)
instance ComplexValued OpFactor where
    conjugate mp@(MatProduct _) = mp
    conjugate _ = error "Not implemented: cannot conjugate operators"
instance ComplexValued FuncGroup where
    conjugate (DiffProduct fs) = DiffProduct (mapFactors conjugate fs)
    conjugate (FuncProduct fs) = FuncProduct (mapFactors conjugate fs)
instance ComplexValued FuncFactor where
    conjugate (Factor f) = Factor (conjugate f)
    conjugate (FuncExpectation fs) = FuncExpectation (mapFactors conjugate fs)
    conjugate (OpExpectation opf) = error "Not implemented: conjugation of operator product expectation"
instance ComplexValued Matrix where
    conjugate x = x


-- OperatorValued instances

class OperatorValued a where
    dagger :: a -> a

instance OperatorValued a => OperatorValued (Maybe a) where
    dagger Nothing = Nothing
    dagger (Just x) = Just (dagger x)
instance OperatorValued Operator where
    dagger (Op e) = DaggerOp e
    dagger (DaggerOp e) = Op e
instance OperatorValued Expr where
    dagger (Expr s) = Expr (mapTermPairs (conjugate A.*** dagger) s)
instance OperatorValued Term where
    dagger (Term opf fs) = Term (dagger opf) (map conjugate fs)
instance OperatorValued OpFactor where
    dagger (NormalProduct ops) = NormalProduct (fromFactorsExpanded dagger_operators) where
        operators = factorsExpanded ops
        dagger_operators = (reverse . map dagger) operators
    dagger (SymmetricProduct ops) = SymmetricProduct (mapFactors dagger ops)
instance OperatorValued Matrix where
    dagger (Mat e) = TMat e
    dagger (TMat e) = Mat e


-- MatrixValued instances

class MatrixValued a where
    transpose :: a -> a

instance MatrixValued a => MatrixValued (Maybe a) where
    transpose Nothing = Nothing
    transpose (Just x) = Just (transpose x)
instance MatrixValued Operator where
    transpose _ = error "Not implemented: transposing operators"
instance MatrixValued Expr where
    transpose (Expr s) = Expr (mapTermPairs (id A.*** transpose) s)
    transpose (Expr2by2 e11 e12 e21 e22) =
        Expr2by2 (transpose e11) (transpose e21) (transpose e12) (transpose e22)
instance MatrixValued Term where
    transpose (Term opf fs) = Term (transpose opf) fs
instance MatrixValued OpFactor where
    transpose (MatProduct mats) = MatProduct (fromFactorsExpanded transpose_matrices) where
        matrices = factorsExpanded mats
        transpose_matrices = (reverse . map transpose) matrices
    transpose (NormalProduct ops) = error "Not implemented: transposing operators"
    transpose (SymmetricProduct ops) = error "Not implemented: transposing operators"
instance MatrixValued Matrix where
    transpose (Mat e) = TMat e
    transpose (TMat e) = Mat e


-- Arithmetic helpers

glueLists :: (a -> a -> [a]) -> [a] -> [a] -> [a]
glueLists connect l1 l2
    | null l1 || null l2 = l1 ++ l2
    | otherwise = init l1 ++ intersection ++ tail l2 where
        intersection = connect (last l1) (head l2)

connectGroups :: FuncGroup -> FuncGroup -> [FuncGroup]
connectGroups (DiffProduct g1) (DiffProduct g2) = [DiffProduct (fromFactors (factors g1 ++ factors g2))]
connectGroups (FuncProduct g1) (FuncProduct g2) = [FuncProduct (fromFactors (factors g1 ++ factors g2))]
connectGroups g1 g2 = [g1, g2]

mulOpFactors :: Maybe OpFactor -> Maybe OpFactor -> Maybe OpFactor
mulOpFactors Nothing Nothing = Nothing
mulOpFactors (Just x) Nothing = Just x
mulOpFactors Nothing (Just x) = Just x
mulOpFactors (Just (NormalProduct x)) (Just (NormalProduct y)) = Just (NormalProduct op_list) where
    op_list = fromFactors (factors x ++ factors y)
mulOpFactors (Just (MatProduct x)) (Just (MatProduct y)) = Just (MatProduct mat_list) where
    mat_list = fromFactors (factors x ++ factors y)
mulOpFactors _ _ = error "Not implemented: multiplication with symmetric products"

mulTerms :: Term -> Term -> Term
mulTerms (Term opf1 fs1) (Term opf2 fs2) = Term op_product f_product where
    op_product = mulOpFactors opf1 opf2
    f_product = glueLists connectGroups fs1 fs2

addSums :: Sum a => a -> a -> a
addSums x y = fromTerms (terms x ++ terms y)

mulSums :: Sum a => a -> a -> a
mulSums x y = fromTerms products where
    products = [(c1 * c2, mulTerms t1 t2) | (c1, t1) <- terms x, (c2, t2) <- terms y]


-- Num instances

instance Num Coefficient where
    negate (Coefficient x) = Coefficient (negate x)
    (Coefficient x) + (Coefficient y) = Coefficient (x + y)
    (Coefficient x) * (Coefficient y) = Coefficient (x * y)
    fromInteger x = Coefficient (fromInteger x :: Complex Rational)
    abs = undefined
    signum = undefined

instance Num Expr where
    negate (Expr ts) = Expr (mapCoefficients negate ts)
    negate (Expr2by2 x11 x12 x21 x22) = Expr2by2 (negate x11) (negate x12) (negate x21) (negate x22)
    (Expr ts1) + (Expr ts2) = Expr (addSums ts1 ts2)
    (Expr2by2 x11 x12 x21 x22) + (Expr2by2 y11 y12 y21 y22)
        = Expr2by2 (x11 + y11) (x12 + y12) (x21 + y21) (x22 + y22)
    (Expr ts1) * (Expr ts2) = Expr (mulSums ts1 ts2)
    (Expr2by2 x11 x12 x21 x22) * y@(Expr _) = Expr2by2 (x11 * y) (x12 * y) (x21 * y) (x22 * y)
    y@(Expr _) * (Expr2by2 x11 x12 x21 x22) = Expr2by2 (y * x11) (y * x12) (y * x21) (y * x22)
    (Expr2by2 x11 x12 x21 x22) * (Expr2by2 y11 y12 y21 y22)
        = Expr2by2
            (x11 * y11 + x12 * y21)
            (x11 * y12 + x12 * y22)
            (x21 * y11 + x22 * y21)
            (x21 * y12 + x22 * y22)
    fromInteger 0 = Expr zeroSum
    fromInteger x = Expr $ fromTerms [(c, t)] where
        t = identityTerm
        c = fromInteger x :: Coefficient
    abs = undefined
    signum = undefined


-- Fractional instances

instance Fractional Coefficient where
    (Coefficient x) / (Coefficient y) = Coefficient (x / y)
    fromRational x = Coefficient (fromRational x :: Complex Rational)

instance Fractional Expr where
    x / (Expr ts)
        | emptySum ts  = error "Division by zero"
        | length (terms ts) > 1 = error "Not implemented: division by sum"
        | term /= identityTerm = error "Not implemented: division by non-scalar expression"
        | otherwise = Expr (fromCoeff (1 / coeff)) * x where
            term = snd (head (terms ts))
            coeff = fst (head (terms ts))
    fromRational x
        | x == 0 = Expr zeroSum
        | otherwise = Expr (fromCoeff (fromRational x :: Coefficient))


-- ComplexNum instances

instance ComplexNum Expr where
    fromComplexRational x
        | x == 0 = Expr zeroSum
        | otherwise = Expr (fromCoeff (fromComplexRational x :: Coefficient))

instance ComplexNum Coefficient where fromComplexRational = Coefficient


-- Texable instances

class Texable a => Superscriptable a where
    needsParentheses :: a -> Bool
    showTexWithExponent :: (a, Int) -> String
    showTexWithExponent (x, p) = addPower p (showTex x) (needsParentheses x)

instance Superscriptable Operator where
    needsParentheses (Op e) = False
    needsParentheses (DaggerOp e) = True
instance Superscriptable Function where
    needsParentheses (Func e) = False
    needsParentheses (ConjFunc e) = True
instance Superscriptable Differential where
    needsParentheses _ = True
instance Superscriptable FuncFactor where
    needsParentheses (Factor f) = needsParentheses f
    needsParentheses _ = False
instance Superscriptable Matrix where
    needsParentheses (Mat e) = False
    needsParentheses (TMat e) = True

showTexIV :: [Index] -> [Function] -> String
showTexIV is vs = indices_str ++ variables_str where
    indices_str = case length is of
        0 -> ""
        1 -> '_' : showTex is
        n -> "_{" ++ unwords (map showTex is) ++ "}"
    variables_str = if null vs
        then ""
        else "(" ++ L.intercalate ", " (map showTex vs) ++ ")"

addPower :: Int -> String -> Bool -> String
addPower 1 s need_parentheses = s
addPower i s True = addPower i ("(" ++ s ++ ")") False
addPower i s False = s ++ "^" ++ show i

makeDiff :: String -> String -> String
makeDiff diff_s s = "\\frac{" ++  diff_s ++ "}{" ++ diff_s ++ " " ++ s ++ "}"

diffSymbolForElement :: Element -> String
diffSymbolForElement (Element _ _ vs) = if null vs then "\\partial" else "\\delta"

diffSymbol :: Function -> String
diffSymbol (Func e) = diffSymbolForElement e
diffSymbol (ConjFunc e) = diffSymbolForElement e

instance Texable Symbol where
    showTex (Symbol s) = s

instance Texable Index where
    showTex (IndexSymbol s) = showTex s
    showTex (IndexInt s) = show s

instance Texable Element where
    showTex (Element s is vs) = showTex s ++ showTexIV is vs

instance Texable a => Texable [a] where
    showTex x = unwords (map showTex x)

class TexableProduct a where
    showTexProduct :: a -> String

instance (Superscriptable b, Texable b, Product a b) => TexableProduct a where
    showTexProduct x = unwords (map showTexWithExponent (factors x))

instance Texable Coefficient where
    showTex (Coefficient x) = showTex x

instance Texable Function where
    showTex (Func e) = showTex e
    showTex (ConjFunc e) = showTex (Func e) ++ "^*"

instance Texable Operator where
    showTex (Op (Element s is vs)) = "\\hat{" ++ showTex s ++ "}" ++ showTexIV is vs
    showTex (DaggerOp e) = showTex (Op e) ++ "^\\dagger"

instance Texable Matrix where
    showTex (Mat e) = showTex e
    showTex (TMat e) = showTex (Mat e) ++ "^T"

instance Texable Differential where
    showTex (Diff f) = makeDiff (diffSymbol f) (showTex f)

instance Texable OpFactor where
    showTex (NormalProduct ops) = showTexProduct ops
    showTex (SymmetricProduct ops) = "\\symprod{" ++ showTexProduct ops ++ "}"
    showTex (MatProduct mats) = showTexProduct mats

instance Texable FuncGroup where
    showTex (FuncProduct fs) = showTexProduct fs
    showTex (DiffProduct ds) = showTexProduct ds

instance Texable FuncFactor where
    showTex (Factor f) = showTex f
    showTex (OpExpectation opf) = "\\langle" ++ showTex opf ++ "\\rangle"
    showTex (FuncExpectation fs) = "\\pathavg{" ++ showTexProduct fs ++ "}"
    showTex (Trace opf) = "\\mathrm{Tr}\\{ " ++ showTex opf ++ " \\}"

instance Texable Term where
    showTex (Term Nothing fs) = showTex fs
    showTex (Term (Just opf) fs) = showTex (Term Nothing fs) ++ " " ++ showTex opf

instance Texable Expr where
    showTex (Expr s)
        | emptySum s = "0"
        | otherwise = showTexList ts where
            ts = terms s
            positive (Coefficient (x :+ y)) = x > 0 || (x == 0 && y > 0)
            showCoeff c before_identity explicit_plus
                | c == 1 && not before_identity = plus_str
                | c == -1 && not before_identity = "-"
                | positive c = plus_str ++ showTex c
                | otherwise = showTex c where
                    plus_str = if explicit_plus then "+" else ""
            showTexTuple explicit_plus (c, t)
                | t == identityTerm = showCoeff c True explicit_plus
                | otherwise = showCoeff c False explicit_plus ++ " " ++ showTex t
            showTexList (tc:[]) = showTexTuple False tc
            showTexList (tc:tcs) = showTexList [tc] ++ "\n" ++
                unlines (map (showTexTuple True) tcs)
    showTex (Expr2by2 x11 x12 x21 x22) = "\\begin{pmatrix}\n" ++
            showTex x11 ++ " & " ++ showTex x12 ++ " \\\\\n" ++
            showTex x21 ++ " & " ++ showTex x22 ++ "\n\\end{pmatrix}"
