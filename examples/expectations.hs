import qualified Wigner.Symbols as S
import Wigner.Expr
import Wigner.Operations
import Wigner.Texable

a1 = Operator (Element S.a [S.ix_1] [])
a2 = Operator (Element S.a [S.ix_2] [])
b1 = Operator (Element S.b [S.ix_1] [])
b2 = Operator (Element S.b [S.ix_2] [])

e_idt = Function (Element (Symbol "e^{i \\Delta \\theta}") [] [])
e_m_idt = Function (Element (Symbol "e^{-i \\Delta \\theta}") [] [])
cos_t = Function (Element (Symbol "\\cos(\\theta)") [] [])
sin_t = Function (Element (Symbol "\\sin(\\theta)") [] [])

-- These must have Expr type
jAX = ((dagger a2) * a1 * e_idt + (dagger a1) * a2 * e_m_idt) / 2
jAY = ((dagger a2) * a1 * e_idt - (dagger a1) * a2 * e_m_idt) / (0 :+ 1 % 2)
jAZ = ((dagger a2) * a2 - (dagger a1) * a1) / 2
jAT = cos_t * jAZ + sin_t * jAX
a2a1 = (dagger a2) * a1

-- Expectations
exp_a2a1 = expectation a2a1
exp_jAY = expectation jAY
var_jAZjAX = variance jAZ jAX
d2_jAZ = deltaSquared jAZ
d2_jAX = deltaSquared jAX
d2_jAT = deltaSquared jAT

main = do
	putStrLn $ show exp_a2a1
	{-
	    < alpha2^* alpha1 >
	-}

	putStrLn $ show exp_jAY
	{-
	    (< alpha2^* alpha1 > e_idt - < alpha2^* alpha1 > e_m_idt) / 2i
	-}

    putStrLn $ show var_jAZjAX
    putStrLn $ show d2_jAZ
    putStrLn $ show d2_jAX
    putStrLn $ show d2_jAT
