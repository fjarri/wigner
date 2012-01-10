{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Wigner.Expression where

	import Wigner.Complex
	import Wigner.Texable
	import Data.List (sort, intercalate)

	data Symbol = SymbolString String deriving (Show, Eq, Ord)
	data Index = IndexSymbol Symbol | IndexInt Int deriving (Show, Eq, Ord)
	data Variable = VariableSymbol Symbol deriving (Show, Eq, Ord)
	type ComplexRational = Complex Rational

	data Function = Function {
		symbol :: Symbol,
		indices :: [Index],
		variables :: [Variable]
		} deriving (Show, Eq)

	instance Ord Function where
		compare
			(Function {symbol=s1, indices=i1, variables=v1})
			(Function {symbol=s2, indices=i2, variables=v2})
			= compare (s1, v1, i1) (s2, v2, i2)

	class MatrixValued a where
		transpose :: a -> a

	dagger :: (ComplexValued a, MatrixValued a) => a -> a
	dagger = conjugate . transpose

	-- | NormalProduct [Expr] | SymmetricProduct [Expr]
	data Expr = Sum [Expr]
		| Product ComplexRational [Expr]
		| Conjugated Expr
		| Transposed Expr
		| HermiteConjugated Expr
		| Power Expr Integer
		| OperatorValue Function
		| ComplexValue Function
		| Constant ComplexRational
		deriving (Show, Eq)

	instance Ord Expr where

		compare (Product c1 xs1) (Product c2 xs2) = compare xs1 xs2
		compare (Product c xs) y = compare (Product c xs) (Product 1 [y])
		compare x (Product c ys) = compare (Product 1 [x]) (Product c ys)

		compare (Conjugated x) y = compare x y
		compare x (Conjugated y) = compare x y

		compare (Transposed x) y = compare x y
		compare x (Transposed y) = compare x y

		compare (HermiteConjugated x) y = compare x y
		compare x (HermiteConjugated y) = compare x y

		compare (Power x e) y = compare x y
		compare x (Power y e) = compare x y

		compare (OperatorValue x) (OperatorValue y) = compare x y
		compare (ComplexValue x) (OperatorValue y) = compare x y
		compare (OperatorValue x) (ComplexValue y) = compare x y
		compare (ComplexValue x) (ComplexValue y) = compare x y
		compare (Constant x) (Constant y) = EQ
		compare (Constant x) y = LT
		compare x (Constant y) = GT

	instance ComplexValued Expr where
		conjugate (Sum xs) = Sum (map conjugate xs)
		conjugate (Product c xs) = Product (conjugate c) (map conjugate xs)
		conjugate (Power x e) = Power (conjugate x) e
		conjugate (Conjugated x) = x
		conjugate (Transposed x) = HermiteConjugated x
		conjugate (HermiteConjugated x) = Transposed x
		conjugate (Constant x) = Constant (conjugate x)
		conjugate x = Conjugated x

	instance MatrixValued Expr where
		transpose (Sum xs) = Sum (map transpose xs)
		transpose (Product c xs) = Product c (map transpose (reverse xs))
		transpose (Power x e) = Power (transpose x) e
		transpose (Transposed x) = x
		transpose (Conjugated x) = HermiteConjugated x
		transpose (HermiteConjugated x) = Conjugated x
		transpose (ComplexValue x) = ComplexValue x
		transpose (Constant x) = Constant x
		transpose x = Transposed x

	instance Num Expr where
		negate (Sum x) = Sum (map negate x)
		negate (Constant x) = Constant (negate x)
		negate (Product c xs) = Product (negate c) xs
		negate x = (Constant (-1 :: ComplexRational)) * x

		Constant 0 + x = x
		x + Constant 0 = x
		(Sum xs) + (Sum ys) = Sum $ sort (xs ++ ys)
		x + (Sum xs) = Sum [x] + Sum xs
		(Sum xs) + x = Sum xs + Sum [x]
		x + y = Sum [x] + Sum [y]

		Constant 1 * x = x
		x * Constant 1 = x
		Constant 0 * x = Constant 0
		x * Constant 0 = Constant 0

		(Constant x) * (Sum xs) = Sum (map ((Constant x) *) xs)

		(Product c1 xs1) * (Product c2 xs2) = Product (c1 * c2) (xs1 ++ xs2)
		x * (Product c xs) = (Product 1 [x]) * (Product c xs)
		(Product c xs) * x = (Product c xs) * (Product 1 [x])
		x * y = Product 1 [x, y]

		fromInteger x = Constant (fromInteger x :: ComplexRational)

		abs x = undefined
		signum x = undefined

	instance Fractional Expr where
		x / (Constant y) = Constant (1 / y) * x
		fromRational x = Constant (fromRational x :: ComplexRational)



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
		showTex (Sum (x:xs)) = "(" ++ (showTex x) ++ (intercalate " " (map showTexWithSign xs)) ++ ")"
		showTex (Product c xs) = coeff ++ (intercalate " " (map showTex xs)) where
			coeff = if c == 1 then "" else (showTex c) ++ " "
		showTex (Conjugated x) = (showTex x) ++ "^*"
		showTex (Transposed x) = (showTex x) ++ "^T"
		showTex (HermiteConjugated x) = (showTex x) ++ "^\\dagger"
		showTex (OperatorValue Function {symbol, indices, variables})
			= showTex Function {
				symbol=(SymbolString ("\\hat{" ++ (showTex symbol) ++ "}")),
				indices=indices,
				variables=variables }
		showTex (ComplexValue x) = showTex x
		showTex (Constant x) = showTex x
