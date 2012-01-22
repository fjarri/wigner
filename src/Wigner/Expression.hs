{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Wigner.Expression(
    OpExpr,
    FuncExpr,
    Symbol(..),
    Index(..),
    Variable(..),
    Element(..),
    Function(..),
    Operator(..),
    Differential(..),
    FuncTerm(..),
    OpTerm(..),
    OpFactor(..),
    FuncFactor(..),
    Coefficient(..),
    fromComplexRational,
    fromFunction,
    fromOperator,
    fromDifferential,
    asSymmetric,
    asFuncExpr,
    asOpExpr,
    toExpr,
    terms, identity,
    fromTerms,
    splitOpTermCoeff, splitOpTermCoeffFunc,
    factors, factorsExpanded, fromFactors, fromFactorsExpanded,
    dagger,
    wrapWithExpectation, splitFuncTerm,
    ) where

import Wigner.Complex
import Wigner.Texable
import qualified Data.Map as M
import qualified Data.Tuple as T
import qualified Data.List as L


-- Data types

type ComplexRational = Complex Rational
data Coefficient = Coefficient (ComplexRational) deriving (Show, Eq)

data Symbol = Symbol String deriving (Show, Eq, Ord)
data Index = IndexSymbol Symbol | IndexInt Int deriving (Show, Eq, Ord)
data Variable = VariableSymbol Symbol deriving (Show, Eq, Ord)
data Element = Element Symbol [Index] [Variable] deriving (Show, Eq)

data Sum a = Sum (M.Map a Coefficient) deriving (Show, Eq)

type OpExpr = Sum OpTerm
data OpTerm = OpTerm (M.Map Function Int) (Maybe OpFactor) deriving (Show, Eq)
data OpFactor = NormalProduct [(Operator, Int)]
              | SymmetricProduct (M.Map Operator Int)
              deriving (Ord, Show, Eq)

type FuncExpr = Sum FuncTerm
data FuncTerm = FuncTerm [FuncFactor] deriving (Show, Eq, Ord)
data FuncFactor = OpExpectation OpFactor
                | FuncExpectation (M.Map Function Int)
                | FuncProduct (M.Map Function Int)
                | DiffProduct (M.Map Differential Int)
                deriving (Show, Eq, Ord)

data Function = Func Element
              | ConjFunc Element
              deriving (Show, Eq)
data Operator = Op Element
              | DaggerOp Element
              deriving (Show, Eq)
data Differential = Diff Element
                  | ConjDiff Element
                  deriving (Show, Eq)


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
instance Ord Differential where
    compare (Diff e1) (Diff e2) = compare e1 e2
    compare (Diff e1) (ConjDiff e2) = compareWithFallback GT e1 e2
    compare (ConjDiff e1) (Diff e2) = compareWithFallback LT e1 e2
    compare (ConjDiff e1) (ConjDiff e2) = compare e1 e2
instance Ord OpTerm where
    compare (OpTerm fs1 opf1) (OpTerm fs2 opf2) =
        compare (opf1, fs1) (opf2, fs2)


-- ComplexValued instances

instance ComplexValued Coefficient where
    conjugate (Coefficient x) = Coefficient (conjugate x)
instance (Ord a, ComplexValued a) => ComplexValued (Sum a) where
    conjugate (Sum ts) = Sum (M.map conjugate (M.mapKeys conjugate ts))
instance ComplexValued FuncTerm where
    conjugate (FuncTerm fs) = FuncTerm (map conjugate fs)
instance ComplexValued FuncFactor where
    conjugate (OpExpectation op) =
        error "Not implemented: conjugation of operator product expectation"
    conjugate (FuncExpectation fs) = FuncExpectation (M.mapKeys conjugate fs)
    conjugate (FuncProduct fs) = FuncProduct (M.mapKeys conjugate fs)
    conjugate (DiffProduct ds) = DiffProduct (M.mapKeys conjugate ds)
instance ComplexValued Function where
    conjugate (Func e) = ConjFunc e
    conjugate (ConjFunc e) = Func e
instance ComplexValued Differential where
    conjugate (Diff e) = ConjDiff e
    conjugate (ConjDiff e) = Diff e


-- OperatorValued instances

class OperatorValued a where
    dagger :: a -> a

instance OperatorValued a => OperatorValued (Maybe a) where
    dagger Nothing = Nothing
    dagger (Just x) = Just (dagger x)
instance (Ord a, OperatorValued a) => OperatorValued (Sum a) where
    dagger (Sum ts) = Sum (M.map conjugate (M.mapKeys dagger ts))
instance OperatorValued OpTerm where
    dagger (OpTerm fs opf) = OpTerm (M.mapKeys conjugate fs) (dagger opf)
instance OperatorValued OpFactor where
    dagger (NormalProduct ops) = NormalProduct (reverse (map (\(x, y) -> (dagger x, y)) ops))
    dagger (SymmetricProduct ops) = SymmetricProduct (M.mapKeys dagger ops)
instance OperatorValued Operator where
    dagger (Op e) = DaggerOp e
    dagger (DaggerOp e) = Op e


-- Multipliable instances

glueLists :: (a -> a -> [a]) -> [a] -> [a] -> [a]
glueLists connect l1 l2
    | null l1 || null l2 = l1 ++ l2
    | otherwise = init l1 ++ intersection ++ tail l2 where
        intersection = connect (last l1) (head l2)

connectFactors :: FuncFactor -> FuncFactor -> [FuncFactor]
connectFactors (FuncProduct x) (FuncProduct y) = [FuncProduct (M.unionWith (+) x y)]
connectFactors (DiffProduct x) (DiffProduct y) = [DiffProduct (M.unionWith (+) x y)]
connectFactors x y = [x, y]

connectTuples :: (Operator, Int) -> (Operator, Int) -> [(Operator, Int)]
connectTuples (x1, y1) (x2, y2)
    | x1 == x2 = [(x1, y1 + y2)]
    | otherwise = [(x1, y1), (x2, y2)]

class Multipliable a where
    mul :: a -> a -> a

instance (Multipliable a) => Multipliable (Maybe a) where
    Nothing `mul` Nothing = Nothing
    (Just x) `mul` Nothing = Just x
    Nothing `mul` (Just y) = Just y
    (Just x) `mul` (Just y) = Just (x `mul` y)

instance Multipliable OpTerm where
    (OpTerm f1 opf1) `mul` (OpTerm f2 opf2) =
        OpTerm (M.unionWith (+) f1 f2) (opf1 `mul` opf2)

instance Multipliable OpFactor where
    (NormalProduct ops1) `mul` (NormalProduct ops2) = NormalProduct $ glueLists connectTuples ops1 ops2
    x `mul` y = error "Not implemented: multiplication with symmetric products"

instance Multipliable FuncTerm where
    (FuncTerm ffs1) `mul` (FuncTerm ffs2) = FuncTerm $ glueLists connectFactors ffs1 ffs2


-- Num instances

instance Num Coefficient where
    negate (Coefficient x) = Coefficient (negate x)
    (Coefficient x) + (Coefficient y) = Coefficient (x + y)
    (Coefficient x) * (Coefficient y) = Coefficient (x * y)
    fromInteger x = Coefficient (fromInteger x :: ComplexRational)
    abs = undefined
    signum = undefined

instance (Term a, Ord a, Eq a, Show a, Multipliable a) => Num (Sum a) where
    negate (Sum ts) = Sum (M.map negate ts)
    (Sum ts1) + (Sum ts2) = Sum $ M.filter (/= 0) (M.unionWith (+) ts1 ts2)
    (Sum ts1) * (Sum ts2) = Sum $ M.filter (/= 0) (M.fromListWithKey combine products) where
        combine _ x y = x + y
        products = [(t1 `mul` t2, c1 * c2) | (t1, c1) <- M.assocs ts1, (t2, c2) <- M.assocs ts2]
    fromInteger 0 = Sum M.empty
    fromInteger x = Sum $ M.singleton identity (fromInteger x :: Coefficient)
    abs = undefined
    signum = undefined


-- Fractional instances

instance Fractional Coefficient where
    (Coefficient x) / (Coefficient y) = Coefficient (x / y)
    fromRational x = Coefficient (fromRational x :: ComplexRational)

instance (Term a, Ord a, Eq a, Show a, Multipliable a) => Fractional (Sum a) where
    x / (Sum ts)
        | M.null ts  = error "Division by zero"
        | M.size ts > 1 = error "Not implemented: division by sum"
        | fst (head pairs) /= identity = error "Not implemented: division by non-scalar expression"
        | otherwise = (Sum $ M.singleton identity (1 / snd (head pairs))) * x where
            pairs = M.assocs ts
    fromRational x
        | x == 0 = Sum M.empty
        | otherwise = Sum $ M.singleton identity (fromRational x :: Coefficient)


-- ComplexNum instances

class ComplexNum a where
    fromComplexRational :: Complex Rational -> a

instance Term a => ComplexNum (Sum a) where
    fromComplexRational x = Sum $ M.singleton identity (fromComplexRational x :: Coefficient)

instance ComplexNum Coefficient where fromComplexRational = Coefficient


-- Term instances

class Term a where
    identity :: a
    fromFunction :: Function -> a
    fromOperator :: Operator -> a
    fromDifferential :: Differential -> a
    toExpr :: a -> Sum a
    toExpr x = Sum (M.singleton x 1)

instance Term OpTerm where
    identity = OpTerm M.empty Nothing
    fromFunction x = OpTerm (M.singleton x 1) Nothing
    fromOperator x = OpTerm M.empty (Just (NormalProduct [(x, 1)]))
    fromDifferential x = error "Not implemented: operator expressions containing differentials"

instance Term FuncTerm where
    identity = FuncTerm []
    fromFunction x = FuncTerm [FuncProduct (M.singleton x 1)]
    fromOperator x = error "Cannot create functional term from an operator"
    fromDifferential x = FuncTerm [DiffProduct (M.singleton x 1)]


-- Auxiliary functions

asSymmetric :: OpExpr -> OpExpr
asSymmetric (Sum ts) = Sum $ M.mapKeys asSym ts where
    asSym (OpTerm fs Nothing) = OpTerm fs Nothing
    asSym (OpTerm fs (Just (NormalProduct ops))) =
        OpTerm fs (Just (SymmetricProduct (M.fromListWith (+) ops)))
    asSym (OpTerm fs (Just (SymmetricProduct ops))) = OpTerm fs (Just (SymmetricProduct ops))

asFuncExpr :: OpExpr -> FuncExpr
asFuncExpr (Sum ts) = Sum $ M.mapKeys asFuncTerm ts where
    asFuncTerm (OpTerm fs Nothing)
        | M.null fs = FuncTerm []
        | otherwise = FuncTerm [FuncProduct fs]
    asFuncTerm opt = error "Cannot convert operators to functions"

asOpExpr :: FuncExpr -> OpExpr
asOpExpr (Sum ts) = Sum $ M.mapKeys asOpTerm ts where
    asOpTerm (FuncTerm []) = OpTerm M.empty Nothing
    asOpTerm (FuncTerm [FuncProduct fp]) = OpTerm fp Nothing
    asOpTerm opt = error "Not implemented: operator expressions containing differentials"

wrapWithExpectation :: OpFactor -> FuncTerm
wrapWithExpectation opf = FuncTerm [OpExpectation opf]

splitFuncTerm :: FuncTerm -> [FuncFactor]
splitFuncTerm (FuncTerm fs) = fs

terms :: Sum a -> [(Coefficient, a)]
terms (Sum ts) = map T.swap (M.assocs ts)

fromTerms :: Ord a => [(Coefficient, a)] -> Sum a
fromTerms ts = Sum (M.fromList (map T.swap ts))

splitOpTerm :: OpTerm -> (OpExpr, Maybe OpFactor)
splitOpTerm (ot@(OpTerm fs Nothing)) = (toExpr ot, Nothing)
splitOpTerm (OpTerm fs opf) = (toExpr (OpTerm fs Nothing), opf)

splitOpTermFunc :: OpTerm -> (FuncExpr, Maybe OpFactor)
splitOpTermFunc (OpTerm fs opf)
    | M.null fs = (toExpr (FuncTerm []), opf)
    | otherwise = (toExpr (FuncTerm [FuncProduct fs]), opf)

splitOpTermCoeff :: (Coefficient, OpTerm) -> (OpExpr, Maybe OpFactor)
splitOpTermCoeff (c, ot) = (f_expr * Sum (M.singleton identity c), opf) where
    (f_expr, opf) = splitOpTerm ot

splitOpTermCoeffFunc :: (Coefficient, OpTerm) -> (FuncExpr, Maybe OpFactor)
splitOpTermCoeffFunc (c, ot) = (f_expr * Sum (M.singleton identity c), opf) where
    (f_expr, opf) = splitOpTermFunc ot


-- Factorisable instances

class Factorisable a b where
    factors :: a -> [(b, Int)]
    fromFactors :: [(b, Int)] -> a
    fromFactorsExpanded :: [b] -> a
    factorsExpanded :: a -> [b]

    factorsExpanded x = L.intercalate [] (map (\(f, p) -> replicate p f) (factors x))
    fromFactorsExpanded x = fromFactors (map (\x -> (x, 1 :: Int)) x)

instance Ord a => Factorisable (M.Map a Int) a where
    factors = M.assocs
    fromFactors = M.fromListWith (+)
instance Factorisable [(a, Int)] a where
    factors = id
    fromFactors = id


-- Texable instances

class Texable a => Superscriptable a where
    needsParentheses :: a -> Bool
    showTexWithExponent :: a -> Int -> String
    showTexWithExponent x p = addPower p (showTex x) (needsParentheses x)

instance Superscriptable Operator where
    needsParentheses (Op e) = False
    needsParentheses (DaggerOp e) = True
instance Superscriptable Function where
    needsParentheses (Func e) = False
    needsParentheses (ConjFunc e) = True
instance Superscriptable Differential where
    needsParentheses _ = True


showTexIV :: [Index] -> [Variable] -> String
showTexIV is vs = indices_str ++ variables_str where
    indices_str = case length is of
        0 -> ""
        1 -> '_' : showTex is
        n -> "_{" ++ unwords (map showTex is) ++ "}"
    variables_str = if null vs
        then ""
        else "(" ++ unwords (map showTex vs) ++ ")"

addPower :: Int -> String -> Bool -> String
addPower 1 s need_parentheses = s
addPower i s True = addPower i ("(" ++ s ++ ")") False
addPower i s False = s ++ "^" ++ show i

makeDiff :: String -> String -> String
makeDiff diff_s s = "\\frac{" ++  diff_s ++ "}{" ++ diff_s ++ " " ++ s ++ "}"

diffSymbol :: Element -> String
diffSymbol (Element _ _ vs) = if null vs then "\\partial" else "\\delta"

instance Texable Symbol where
    showTex (Symbol s) = s

instance Texable Index where
    showTex (IndexSymbol s) = showTex s
    showTex (IndexInt s) = show s

instance Texable Variable where
    showTex (VariableSymbol s) = showTex s

instance Texable Element where
    showTex (Element s is vs) = showTex s ++ showTexIV is vs

instance Texable a => Texable [a] where
    showTex x = unwords (map showTex x)

instance (Superscriptable a, Texable a) => Texable (a, Int) where
    showTex (x, p) = showTexWithExponent x p

instance (Superscriptable a, Texable a) => Texable (M.Map a Int) where
    showTex x = showTex (M.assocs x)

instance Texable Coefficient where
    showTex (Coefficient x) = showTex x

instance Texable Function where
    showTex (Func e) = showTex e
    showTex (ConjFunc e) = showTex (Func e) ++ "^*"

instance Texable Operator where
    showTex (Op (Element s is vs)) = "\\hat{" ++ showTex s ++ "}" ++ showTexIV is vs
    showTex (DaggerOp e) = showTex (Op e) ++ "^\\dagger"

instance Texable Differential where
    showTex (Diff e) = makeDiff (diffSymbol e) (showTex (Func e))
    showTex (ConjDiff e) = makeDiff (diffSymbol e) (showTex (ConjFunc e))

instance Texable OpFactor where
    showTex (NormalProduct ops) = showTex ops
    showTex (SymmetricProduct ops) = "\\symprod{" ++ showTex ops ++ "}"

instance Texable FuncFactor where
    showTex (OpExpectation opf) = "\\langle" ++ showTex opf ++ "\\rangle"
    showTex (FuncExpectation fs) = "\\pathavg{" ++ showTex fs ++ "}"
    showTex (FuncProduct fs) = showTex fs
    showTex (DiffProduct ds) = showTex ds

instance Texable OpTerm where
    showTex (OpTerm fs Nothing) = showTex fs
    showTex (OpTerm fs (Just opf)) = showTex (OpTerm fs Nothing) ++ " " ++ showTex opf

instance Texable FuncTerm where
    showTex (FuncTerm ffs) = showTex ffs

instance (Term a, Texable a, Eq a) => Texable (Sum a) where
    showTex s
        | null ts = "0"
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
                | t == identity = showCoeff c True explicit_plus
                | otherwise = showCoeff c False explicit_plus ++ " " ++ showTex t
            showTexList (tc:[]) = showTexTuple False tc
            showTexList (tc:tcs) = showTexList [tc] ++ " " ++
                unwords (map (showTexTuple True) tcs)
