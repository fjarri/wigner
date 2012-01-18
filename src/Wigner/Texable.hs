module Wigner.Texable (Texable, showTex) where

	import Data.Ratio

	class Texable a where
		showTex :: a -> String

	instance Texable Integer where
		showTex = show

	instance (Texable a, Integral a) => Texable (Ratio a) where
		showTex x
			| d == 1 = showTex n
			| n < 0 = '-' : showTex ((-n) % d)
			| otherwise = "\\frac{" ++ showTex n ++ "}{" ++ showTex d ++ "}"
			where
				n = numerator x
				d = denominator x
