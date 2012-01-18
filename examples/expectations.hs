import qualified Wigner.Symbols as S
import Data.Ratio
import Wigner.Complex
import Wigner.Expression
import Wigner.Operations
import Wigner.Texable

a1 = makeOpExpr $ Op (Element S.a [S.ix_1] [])
a2 = makeOpExpr $ Op (Element S.a [S.ix_2] [])
b1 = makeOpExpr $ Op (Element S.b [S.ix_1] [])
b2 = makeOpExpr $ Op (Element S.b [S.ix_2] [])

e_idt = makeOpExpr $ Func (Element (Symbol "e^{i \\Delta \\theta}") [] [])
e_m_idt = makeOpExpr $ Func (Element (Symbol "e^{-i \\Delta \\theta}") [] [])
cos_t = makeOpExpr $ Func (Element (Symbol "\\cos(\\theta)") [] [])
sin_t = makeOpExpr $ Func (Element (Symbol "\\sin(\\theta)") [] [])

-- These must have Expr type
jAX = ((dagger a2) * a1 * e_idt + (dagger a1) * a2 * e_m_idt) / 2
jAY = ((dagger a2) * a1 * e_idt - (dagger a1) * a2 * e_m_idt) / (makeOpExpr (0 :+ 2 :: ComplexRational))
jAZ = ((dagger a2) * a2 - (dagger a1) * a1) / 2
jAT = cos_t * jAZ + sin_t * jAX
a2a1 = (dagger a2) * a1

isqrt2 = makeOpExpr $ Func (Element (Symbol "\\frac{1}{\\sqrt{2}}") [] [])
c1 = isqrt2 * (a1 + b1 * (makeOpExpr (0 :+ 1 :: ComplexRational)))
c2 = isqrt2 * (a2 + b2 * (makeOpExpr (0 :+ 1 :: ComplexRational)))
jCZ = ((dagger c2) * c2 - (dagger c1) * c1) / 2

-- Expectations
{-exp_a2a1 = expectation a2a1
exp_jAY = expectation jAY
var_jAZjAX = variance jAZ jAX
d2_jAZ = deltaSquared jAZ
d2_jAX = deltaSquared jAX
d2_jAT = deltaSquared jAT-}

main = do

--    putStrLn $ show a2a1
    putStrLn $ showTex a2a1
    putStrLn ""

--    putStrLn $ show jAX
    putStrLn $ showTex jAX
    putStrLn ""

--    putStrLn $ show jAY
    putStrLn $ showTex jAY
    putStrLn ""

--    putStrLn $ show jAZ
    putStrLn $ showTex jAZ
    putStrLn ""

--    putStrLn $ show jAT
    putStrLn $ showTex jAT
    putStrLn ""

--    putStrLn $ show jCZ
    putStrLn $ showTex jCZ
    putStrLn ""
