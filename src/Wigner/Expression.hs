{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-
1) Construct operators:
    Sum [F], [NO] : (*), (+) -> Sum [F], [NO]
2) Mark expectations
    Sum [F], [NO] : expectation, deltaSquared, variance ->
        Sum [F, Expectation] where Expectation = Ex NO | Ex SO | Ex Product
3) Normal order to symmetric order
    [F], [NO] -> Sum [F], [SO]
4) Evaluate expectations
    [F], [NO] -> Sum [F], [SO]
    [SO] -> Sum [Exp SO] -> Sum [Exp Product]
5) Wigner transformation
    [NO] -> Sum [[F], [D], ...]
    Sum [[F], [D], ...] -> Sum [D], [F]
-}

module Wigner.Expression where

    import Wigner.Complex
    import Wigner.Texable
    import qualified Data.List as L
    import qualified Data.Set as S
    import qualified Data.Map as M

    type ComplexRational = Complex Rational

    data Symbol = Symbol String deriving (Show, Eq, Ord)
    data Index = IndexSymbol Symbol | IndexInt Int deriving (Show, Eq, Ord)
    data Variable = VariableSymbol Symbol deriving (Show, Eq, Ord)
    data Element = Element Symbol [Index] [Variable] deriving (Show, Eq)

    data Sum a = Sum (M.Map a ComplexRational)
               | Constant ComplexRational
               deriving (Show, Eq)

    type OpExpr = Sum OpTerm
    data OpTerm = OpTerm (S.Set Function) (Maybe OpFactor) deriving (Show, Eq)
    data OpFactor = NormalProduct [Operator]
                  | SymmetricProduct (S.Set Operator)
                  deriving (Ord, Show, Eq)

    type FuncExpr = Sum FuncTerm
    data FuncTerm = FuncTerm [FuncFactor] deriving (Show, Eq, Ord)
    data FuncFactor = OpExpectation OpFactor
                    | FuncExpectation (S.Set Function)
                    | FuncProduct (S.Set Function)
                    | DiffProduct (S.Set Differential)
                    deriving (Show, Eq, Ord)

    data Function = Func Element Integer
                  | ConjFunc Element Integer
                  deriving (Show, Eq)
    data Operator = Op Element Integer
                  | DaggerOp Element Integer
                  deriving (Show, Eq)
    data Differential = Diff Element Integer
                      | ConjDiff Element Integer
                      deriving (Show, Eq)


    instance Ord Element where
        compare (Element s1 i1 v1) (Element s2 i2 v2) = compare (s1, v1, i1) (s2, v2, i2)
    instance Ord Function where
        compare (Func e1 p1) (Func e2 p2) = compare e1 e2
        compare (Func e1 p1) (ConjFunc e2 p2) = GT
        compare (ConjFunc e1 p1) (Func e2 p2) = LT
        compare (ConjFunc e1 p1) (ConjFunc e2 p2) = compare e1 e2
    instance Ord Operator where
        compare (Op e1 p1) (Op e2 p2) = compare e1 e2
        compare (Op e1 p1) (DaggerOp e2 p2) = GT
        compare (DaggerOp e1 p1) (Op e2 p2) = LT
        compare (DaggerOp e1 p1) (DaggerOp e2 p2) = compare e1 e2
    instance Ord Differential where
        compare (Diff e1 p1) (Diff e2 p2) = compare e1 e2
        compare (Diff e1 p1) (ConjDiff e2 p2) = GT
        compare (ConjDiff e1 p1) (Diff e2 p2) = LT
        compare (ConjDiff e1 p1) (ConjDiff e2 p2) = compare e1 e2
    instance Ord OpTerm where
        compare (OpTerm fs1 opf1) (OpTerm fs2 opf2) =
            compare (opf1, fs1) (opf2, fs2)


    class OpExpression a where
        makeOpExpr :: a -> OpExpr

    instance OpExpression OpExpr where makeOpExpr = id
    instance OpExpression ComplexRational where makeOpExpr x = Constant x
    instance OpExpression Integer where
        makeOpExpr x = makeOpExpr ((fromInteger x :: Rational) :+ 0)
    instance OpExpression Function where
        makeOpExpr x = Sum $ M.singleton (OpTerm (S.singleton x) Nothing) 1
    instance OpExpression Operator where
        makeOpExpr x = Sum $ M.singleton (OpTerm S.empty (Just (NormalProduct [x]))) 1


    instance (Ord a, ComplexValued a) => ComplexValued (Sum a) where
        conjugate (Sum ts) = Sum (M.map conjugate (M.mapKeys conjugate ts))
        conjugate (Constant c) = Constant (conjugate c)
    instance ComplexValued FuncTerm where
        conjugate (FuncTerm fs) = FuncTerm (map conjugate fs)
    instance ComplexValued FuncFactor where
        -- technically we can, just do not need to, and I do not want to bloat the set of types
        conjugate (OpExpectation op) = error "Cannot conjugate an expectation of operator product"
        conjugate (FuncExpectation fs) = FuncExpectation (S.map conjugate fs)
        conjugate (FuncProduct fs) = FuncProduct (S.map conjugate fs)
        conjugate (DiffProduct ds) = DiffProduct (S.map conjugate ds)
    instance ComplexValued Function where
        conjugate (Func e p) = ConjFunc e p
        conjugate (ConjFunc e p) = Func e p
    instance ComplexValued Differential where
        conjugate (Diff e p) = ConjDiff e p
        conjugate (ConjDiff e p) = Diff e p


    class OperatorValued a where
        dagger :: a -> a

    instance OperatorValued a => OperatorValued (Maybe a) where
        dagger Nothing = Nothing
        dagger (Just x) = Just (dagger x)
    instance (Ord a, OperatorValued a) => OperatorValued (Sum a) where
        dagger (Sum ts) = Sum (M.map conjugate (M.mapKeys dagger ts))
        dagger (Constant c) = Constant (conjugate c)
    instance OperatorValued OpTerm where
        dagger (OpTerm fs opf) = OpTerm (S.map conjugate fs) (dagger opf)
    instance OperatorValued OpFactor where
        dagger (NormalProduct ops) = NormalProduct (reverse (map dagger ops))
        dagger (SymmetricProduct ops) = SymmetricProduct (S.map dagger ops)
    instance OperatorValued Operator where
        dagger (Op e p) = DaggerOp e p
        dagger (DaggerOp e p) = Op e p


    class Multipliable a where
        mul :: a -> a -> a

    instance (Multipliable a) => Multipliable (Maybe a) where
        Nothing `mul` Nothing = Nothing
        (Just x) `mul` Nothing = Just x
        Nothing `mul` (Just y) = Just y
        (Just x) `mul` (Just y) = Just (x `mul` y)

    instance Multipliable OpTerm where
        (OpTerm f1 opf1) `mul` (OpTerm f2 opf2) =
            OpTerm (S.union f1 f2) (opf1 `mul` opf2)

    instance Multipliable OpFactor where
        (NormalProduct ops1) `mul` (NormalProduct ops2) = NormalProduct (ops1 ++ ops2)
        (SymmetricProduct ops1) `mul` (SymmetricProduct ops2) = SymmetricProduct (S.union ops1 ops2)
        x `mul` y = error "Cannot multiply normal and symmetric product"

    groupTerms x = M.unionsWith (+) x

    instance (Ord a, Eq a, Show a, Multipliable a) => Num (Sum a) where
        negate (Sum ts) = Sum (M.map negate ts)
        negate (Constant c) = Constant (negate c)
        (Sum ts1) + (Sum ts2) = Sum (groupTerms [ts1, ts2])
        (Constant c) * (Sum ts) = Sum (M.map ((*) c) ts)
        (Sum ts) * (Constant c) = Sum (M.map ((*) c) ts)
        (Sum ts1) * (Sum ts2) = Sum (M.fromListWithKey combine products) where
            combine _ x y = x + y
            products = [(t1 `mul` t2, c1 * c2) | (t1, c1) <- M.assocs ts1, (t2, c2) <- M.assocs ts2]
        fromInteger x = Constant (fromInteger x :: ComplexRational)
        abs = undefined
        signum = undefined

    instance (Ord a, Eq a, Show a, Multipliable a) => Fractional (Sum a) where
        x / (Constant y) = Constant (1 / y) * x
        fromRational x = Constant (fromRational x :: ComplexRational)



    instance Texable Symbol where
        showTex (Symbol s) = s

    instance Texable Index where
        showTex (IndexSymbol s) = showTex s
        showTex (IndexInt s) = show s

    instance Texable Variable where
        showTex (VariableSymbol s) = showTex s

    showTexIV :: [Index] -> [Variable] -> String
    showTexIV is vs = indices_str ++ variables_str where
        indices_str = case length is of
            0 -> ""
            1 -> "_" ++ showTex is
            n -> "_{" ++ L.intercalate " " (map showTex is) ++ "}"
        variables_str = if null vs
            then ""
            else "(" ++ L.intercalate " " (map showTex vs) ++ ")"

    instance Texable Element where
        showTex (Element s is vs) = showTex s ++ showTexIV is vs

    addPower :: Integer -> String -> Bool -> String
    addPower 1 s need_parentheses = s
    addPower i s True = addPower i ("(" ++ s ++ ")") False
    addPower i s False = s ++ "^" ++ show i

    makeDiff diff_s s = "\\frac{" ++  diff_s ++ "}{" ++ diff_s ++ " " ++ s ++ "}"
    diffSymbol (Element _ _ vs) = if null vs then "\\partial" else "\\delta"

    instance Texable a => Texable [a] where
        showTex x = L.intercalate " " (map showTex x)

    instance Texable a => Texable (S.Set a) where
        showTex x = L.intercalate " " (map showTex (S.elems x))

    instance Texable Function where
        showTex (Func e i) = addPower i (showTex e) False
        showTex (ConjFunc e i) = addPower i (showTex (Func e 1) ++ "^*") True

    instance Texable Operator where
        showTex (Op (Element s is vs) i) = addPower i ("\\hat{" ++ showTex s ++ "}" ++ showTexIV is vs) False
        showTex (DaggerOp e i) = addPower i (showTex (Op e 1) ++ "^\\dagger") True

    instance Texable Differential where
        showTex (Diff e i) = makeDiff (diffSymbol e) (showTex (Func e i))
        showTex (ConjDiff e i) = makeDiff (diffSymbol e) (showTex (ConjFunc e i))

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
        showTex (OpTerm fs (Just opf)) = showTex fs ++ " " ++ showTex opf

    instance Texable FuncTerm where
        showTex (FuncTerm ffs) = showTex ffs

    instance Texable a => Texable (Sum a) where
        showTex (Constant c) = showTex c
        showTex (Sum ts)
            | M.null ts = "0"
            | otherwise = showTexList (M.assocs ts) where
                showTexTuple (t, c) = showCoeff c True ++ " " ++ showTex t
                showTexList ((t, c):[]) = showCoeff c False ++ " " ++ showTex t
                showTexList ((t, c):ts) = showTexList [(t, c)] ++ L.intercalate " " (map showTexTuple ts)

    showCoeff (x :+ y) explicit_plus
        | x == 1 && y == 0 = plus_str
        | x == -1 && y == 0 = "-"
        | x > 0 || (x == 0 && y > 0) = plus_str ++ (showTex (x :+ y))
        | otherwise = showTex (x :+ y) where
            plus_str = if explicit_plus then "+" else ""
