{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
	OverlappingInstances, UndecidableInstances #-}

module Wigner.Complex (Complex((:+)), ComplexContainer,
	makeComplex, realPart, imagPart, conjugate, ComplexValued) where

	import Data.Ratio
	import Wigner.Texable

	class ComplexContainer a b | a -> b where
		realPart :: a -> b
		imagPart :: a -> b
		makeComplex :: b -> b -> a

	class ComplexValued a where
		conjugate :: a -> a

	data Complex a = a :+ a deriving (Show)

	instance ComplexContainer (Complex a) a where
		realPart (x :+ y) = x
		imagPart (x :+ y) = y
		makeComplex x y = x :+ y

	instance (ComplexContainer a b, Num b) => ComplexValued a where
		conjugate x = makeComplex (realPart x) (-imagPart x)

	instance (ComplexContainer a b, Eq b) => Eq a where
		x == y = (realPart x == realPart y) && (imagPart x == imagPart y)

	instance (ComplexContainer a b, Show a, Num b) => Num a where
		negate x = makeComplex (negate (realPart x)) (negate (imagPart x))
		x + y = makeComplex (realPart x + realPart y) (imagPart x + imagPart y)
		x * y = makeComplex
			(realPart x * realPart y - imagPart x * imagPart y)
			(realPart x * imagPart y + realPart x * imagPart y)
		abs x = undefined
		signum x = undefined
		fromInteger x = makeComplex (fromInteger x) (fromInteger 0)

	instance (ComplexContainer a b, Show a, Fractional b) => Fractional a where
		x / y = makeComplex ((x1 * y1 + x2 * y2) / m) ((x2 * y1 - x1 * y2) / m) where
			x1 = realPart x
			x2 = imagPart x
			y1 = realPart y
			y2 = imagPart y
			m = y1 * y1 + y2 * y2
		fromRational x = makeComplex (fromRational x) (fromRational 0)

	instance (ComplexContainer a b, Texable b, Ord b, Num b) => Texable a where
		showTex x
			| im == 0 = showTex re
			| re == 0 = (showTex im) ++ "i"
			| otherwise = "(" ++ (showTex re) ++ sign ++ (showTex im) ++ "i)"
			where
				re = realPart x
				im = imagPart x
				sign = if im < 0 then "-" else "+"
