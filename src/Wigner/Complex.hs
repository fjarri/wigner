module Wigner.Complex (Complex((:+)), conjugate, ComplexValued) where

	import Data.Ratio
	import Wigner.Texable

	data Complex a = a :+ a deriving (Show, Eq)

	class ComplexValued a where
		conjugate :: a -> a

	instance (Num a) => ComplexValued (Complex a) where
		conjugate (x :+ y) = x :+ (-y)

	instance (Num a) => Num (Complex a) where
		negate (x :+ y) = negate x :+ negate y
		(x1 :+ y1) + (x2 :+ y2) = (x1 + x2) :+ (y1 + y2)
		(x1 :+ y1) * (x2 :+ y2) = (x1 * x2 - y1 * y2) :+ (x1 * y2 + y1 * x2)
		abs x = undefined
		signum x = undefined
		fromInteger x = fromInteger x :+ 0

	instance (Fractional a) => Fractional (Complex a) where
		(x1 :+ y1) / (x2 :+ y2) = ((x1 * x2 + y1 * y2) / m) :+ ((x1 * y2 - y1 * x2) / m) where
			m = x2 * x2 + y2 * y2
		fromRational x = fromRational x :+ fromRational 0

	instance (Texable a, Ord a, Num a) => Texable (Complex a) where
		showTex (x :+ y)
			| y == 0 = sx
			| x == 0 = sy ++ "i"
			| otherwise = "(" ++ sx ++ sign ++ sy ++ "i)"
			where
				sx = showTex x
				sy = showTex y
				sign = if y < 0 then "" else "+"
