import qualified Wigner.Symbols as S
import qualified Wigner.DefineOpExpr as DO

import Data.Ratio
import Wigner.Complex
import Wigner.Texable
import Wigner.Expression

import qualified Wigner.Symbols as S
import qualified Wigner.DefineOpExpr as DO
import qualified Wigner.DefineFuncExpr as DF
import qualified Wigner.Expectations as E

a1 = DO.operatorIx S.a [S.ix_1]
a2 = DO.operatorIx S.a [S.ix_2]
b1 = DO.operatorIx S.b [S.ix_1]
b2 = DO.operatorIx S.b [S.ix_2]

e_idt = DO.constant $ S.symbol "e^{i \\Delta \\theta}"
e_m_idt = DO.constant $ S.symbol "e^{-i \\Delta \\theta}"
cos_t = DO.constant $ S.symbol "\\cos(\\theta)"
sin_t = DO.constant $ S.symbol "\\sin(\\theta)"

-- These must have Expr type
jAX = ((dagger a2) * a1 * e_idt + (dagger a1) * a2 * e_m_idt) / 2
jAY = ((dagger a2) * a1 * e_idt - (dagger a1) * a2 * e_m_idt) / (2 * DO.i)
jAZ = ((dagger a2) * a2 - (dagger a1) * a1) / 2
jAT = cos_t * jAZ + sin_t * jAX
a2a1 = (dagger a2) * a1

isqrt2 = DO.constant $ S.symbol "\\frac{1}{\\sqrt{2}}"
c1 = isqrt2 * (a1 + b1 * DO.i)
c2 = isqrt2 * (a2 + b2 * DO.i)
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

    putStrLn $ showTex (DO.one)
