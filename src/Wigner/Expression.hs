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

    import Data.List
    import Wigner.Complex
    import Wigner.Texable
    import Data.List (sort, intercalate)

    type ComplexRational = Complex Rational

    data Symbol = Symbol String deriving (Show, Eq, Ord)
    data Index = IndexSymbol Symbol | IndexInt Int deriving (Show, Eq, Ord)
    data Variable = VariableSymbol Symbol deriving (Show, Eq, Ord)
    data Element = Element Symbol [Index] [Variable] deriving (Show, Eq)

    data Sum a = Sum [a]
               | Constant ComplexRational
               deriving (Show, Eq)

    type OpExpr = Sum OpTerm
    data OpTerm = OpTerm ComplexRational [Function] (Maybe OpFactor) deriving (Show, Eq)
    data OpFactor = NormalProduct [Operator]
                  | SymmetricProduct [Operator]
                  deriving (Ord, Show, Eq)

    type FuncExpr = Sum FuncTerm
    data FuncTerm = FuncTerm ComplexRational [FuncFactor] deriving (Show, Eq)
    data FuncFactor = OpExpectation OpFactor
                    | FuncExpectation [Function]
                    | FuncProduct [Function]
                    | DiffProduct [Differential]
                    deriving (Ord, Show, Eq)

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
        compare (Func e1 p1) (ConjFunc e2 p2) = compare e1 e2
        compare (ConjFunc e1 p1) (Func e2 p2) = compare e1 e2
        compare (ConjFunc e1 p1) (ConjFunc e2 p2) = compare e1 e2
    instance Ord Operator where
        compare (Op e1 p1) (Op e2 p2) = compare e1 e2
        compare (Op e1 p1) (DaggerOp e2 p2) = compare e1 e2
        compare (DaggerOp e1 p1) (Op e2 p2) = compare e1 e2
        compare (DaggerOp e1 p1) (DaggerOp e2 p2) = compare e1 e2
    instance Ord Differential where
        compare (Diff e1 p1) (Diff e2 p2) = compare e1 e2
        compare (Diff e1 p1) (ConjDiff e2 p2) = compare e1 e2
        compare (ConjDiff e1 p1) (Diff e2 p2) = compare e1 e2
        compare (ConjDiff e1 p1) (ConjDiff e2 p2) = compare e1 e2
    instance Ord OpTerm where
        compare (OpTerm c1 fs1 opf1) (OpTerm c2 fs2 opf2) =
            compare (opf1, fs1) (opf2, fs2)


    class OpExpression a where
        makeOpExpr :: a -> OpExpr

    instance OpExpression OpExpr where makeOpExpr = id
    instance OpExpression ComplexRational where makeOpExpr x = Constant x
    instance OpExpression Integer where makeOpExpr x = makeOpExpr ((fromInteger x :: Rational) :+ 0)
    instance OpExpression Function where makeOpExpr x = Sum [OpTerm 1 [x] Nothing]
    instance OpExpression Operator where makeOpExpr x = Sum [OpTerm 1 [] (Just (NormalProduct [x]))]


    instance (ComplexValued a) => ComplexValued (Sum a) where
        conjugate (Sum ts) = Sum (map conjugate ts)
        conjugate (Constant c) = Constant (conjugate c)
    instance ComplexValued FuncTerm where
        conjugate (FuncTerm c fs) = FuncTerm (conjugate c) (map conjugate fs)
    instance ComplexValued FuncFactor where
        -- technically we can, just do not need to, and I do not want to bloat the set of types
        conjugate (OpExpectation op) = error "Cannot conjugate an expectation of operator product"
        conjugate (FuncExpectation fs) = FuncExpectation (map conjugate fs)
        conjugate (FuncProduct fs) = FuncProduct (map conjugate fs)
        conjugate (DiffProduct ds) = DiffProduct (map conjugate ds)
    instance ComplexValued Function where
        conjugate (Func e p) = ConjFunc e p
        conjugate (ConjFunc e p) = Func e p
    instance ComplexValued Differential where
        conjugate (Diff e p) = ConjDiff e p
        conjugate (ConjDiff e p) = Diff e p


    class OperatorValued a where
        dagger :: a -> a

    instance (OperatorValued a) => OperatorValued (Maybe a) where
        dagger Nothing = Nothing
        dagger (Just x) = Just (dagger x)
    instance (OperatorValued a) => OperatorValued (Sum a) where
        dagger (Sum ts) = Sum (map dagger ts)
        dagger (Constant c) = Constant (conjugate c)
    instance OperatorValued OpTerm where
        dagger (OpTerm c fs opf) = OpTerm (conjugate c) (map conjugate fs) (dagger opf)
    instance OperatorValued Function where
        dagger = conjugate
    instance OperatorValued OpFactor where
        dagger (NormalProduct ops) = NormalProduct (reverse (map dagger ops))
        dagger (SymmetricProduct ops) = SymmetricProduct (map dagger ops)
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
        (OpTerm c1 f1 opf1) `mul` (OpTerm c2 f2 opf2) = OpTerm (c1 * c2) (sort $ f1 ++ f2) (opf1 `mul` opf2)

    instance Multipliable OpFactor where
        (NormalProduct ops1) `mul` (NormalProduct ops2) = NormalProduct (ops1 ++ ops2)
        (SymmetricProduct ops1) `mul` (SymmetricProduct ops2) = SymmetricProduct (sort $ ops1 ++ ops2)
        x `mul` y = error "Cannot multiply normal and symmetric product"

    class Term a where
        termCoeff :: a -> ComplexRational
        termAtom :: a -> a
        makeTerm :: ComplexRational -> a -> a

    instance Term OpTerm where
        termCoeff (OpTerm c fs opf) = c
        termAtom (OpTerm c fs opf) = OpTerm 1 fs opf
        makeTerm c (OpTerm _ fs opf) = OpTerm c fs opf
    instance Term FuncTerm where
        termCoeff (FuncTerm c fs) = c
        termAtom (FuncTerm c fs) = FuncTerm 1 fs
        makeTerm c (FuncTerm _ fs) = FuncTerm c fs

    groupTerms' :: (Ord a, Term a) => [a] -> [[a]]
    groupTerms' = groupBy eqTest . sortBy cmpTest where
        cmpTest x y = compare (termAtom x) (termAtom y)
        eqTest x y = (cmpTest x y) == EQ

    joinTerms :: Term a => [a] -> a
    joinTerms = foldr1 (\x y -> makeTerm (termCoeff x + termCoeff y) (termAtom x))

    groupTerms :: (Ord a, Term a) => [a] -> [a]
    groupTerms x = map joinTerms (groupTerms' x)

    termNegate :: Term a => a -> a
    termNegate x = makeTerm (negate c) atom where
        c = termCoeff x
        atom = termAtom x

    termMulConstant :: (Term a) => ComplexRational -> a-> a
    termMulConstant t x = makeTerm (c * t) atom where
        c = termCoeff x
        atom = termAtom x

    instance (Ord a, Eq a, Show a, Multipliable a, Term a) => Num (Sum a) where
        negate (Sum ts) = Sum (map termNegate ts)
        negate (Constant c) = Constant (negate c)
        (Sum ts1) + (Sum ts2) = Sum (groupTerms (ts1 ++ ts2))
        (Constant c) * (Sum ts) = Sum (map (termMulConstant c) ts)
        (Sum ts) * (Constant c) = Sum (map (termMulConstant c) ts)
        (Sum ts1) * (Sum ts2) = Sum (groupTerms combinations) where
            combinations = [x `mul` y | x <- ts1, y <- ts2]
        fromInteger x = Constant (fromInteger x :: ComplexRational)
        abs = undefined
        signum = undefined

    instance (Ord a, Eq a, Show a, Multipliable a, Term a) => Fractional (Sum a) where
        x / (Constant y) = Constant (1 / y) * x
        fromRational x = Constant (fromRational x :: ComplexRational)


{-
    instance Texable Symbol where
        showTex (SymbolString s) = s

    instance Texable Index where
        showTex (IndexSymbol s) = showTex s
        showTex (IndexInt s) = show s

    instance Texable Variable where
        showTex (VariableSymbol s) = showTex s

    instance Texable Function where
        showTex (Function {symbol, indices, variables}) =
            (showTex symbol) ++ indices_str ++ variables_str where
                indices_str = if indices == []
                    then ""
                    else "_{" ++ intercalate " " (map showTex indices) ++ "}"
                variables_str = if variables == []
                    then ""
                    else "(" ++ intercalate " " (map showTex variables) ++ ")"

    showTexWithSign :: Expr -> String
    showTexWithSign (Constant (x :+ y))
        | x > 0 || (x == 0 && y > 0) || (x /= 0 && y /= 0) = "+" ++ (showTex (x :+ y))
        | otherwise = showTex (x :+ y)
    showTexWithSign (Product c xs) = coeff ++ showTex (Product 1 xs) where
        coeff = if c == 1 then "" else (showTexWithSign (Constant c)) ++ " "
    showTexWithSign x = "+" ++ showTex x

    instance Texable Expr where
        showTex (Sum (x:xs)) = "(" ++ (showTex x) ++ " " ++ (intercalate " " (map showTexWithSign xs)) ++ ")"
        showTex (Product c xs) = coeff ++ (intercalate " " (map showTex xs)) where
            coeff = if c == 1 then "" else (showTex c) ++ " "
        showTex (Conjugated x) = (showTex x) ++ "^*"
        showTex (Transposed x) = (showTex x) ++ "^T"
        showTex (Dagger x) = (showTex x) ++ "^\\dagger"
        showTex (OperatorValue Function {symbol, indices, variables})
            = showTex Function {
                symbol=(SymbolString ("\\hat{" ++ (showTex symbol) ++ "}")),
                indices=indices,
                variables=variables }
        showTex (ComplexValue x) = showTex x
        showTex (Constant x) = showTex x
-}