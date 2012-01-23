import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.Expectations as E
import qualified Data.Set as Set

import Data.Ratio
import Wigner.Complex
import Wigner.Texable
import Wigner.Expression

import Wigner.XmdsInteraction

eval = E.evaluateExpectations E.wignerExpectation S.default_map

a1 = D.operatorIx S.a [S.ix_1]
a2 = D.operatorIx S.a [S.ix_2]
b1 = D.operatorIx S.b [S.ix_1]
b2 = D.operatorIx S.b [S.ix_2]

e_idt = D.constant $ S.symbol "(e^{i \\Delta \\theta})"
e_m_idt = D.constant $ S.symbol "(e^{-i \\Delta \\theta})"
cos_t = D.constant $ S.symbol "\\cos(\\theta)"
sin_t = D.constant $ S.symbol "\\sin(\\theta)"

jAX = ((dagger a2) * a1 * e_idt + (dagger a1) * a2 * e_m_idt) / 2
jAY = ((dagger a2) * a1 * e_idt - (dagger a1) * a2 * e_m_idt) / (2 * D.i)
jAZ = ((dagger a2) * a2 - (dagger a1) * a1) / 2
jAT = cos_t * jAZ + sin_t * jAX
a2a1 = (dagger a2) * a1

isqrt2 = D.constant $ S.symbol "(\\frac{1}{\\sqrt{2}})"
c1 = isqrt2 * (a1 + b1 * D.i)
c2 = isqrt2 * (a2 + b2 * D.i)
jCZ = ((dagger c2) * c2 - (dagger c1) * c1) / 2

-- Expectations
exp_a2a1 = eval $ E.expectation a2a1
exp_jAY = eval $ E.expectation jAY
var_jAZjAX = eval $ E.variance jAZ jAX
d2_jAZ = eval $ E.deltaSquared jAZ
d2_jAX = eval $ E.deltaSquared jAX
d2_jAT = eval $ E.deltaSquared jAT

main = do

--    putStrLn $ "exp_a2a1 = " ++ showTex exp_a2a1 ++ "\n"
--    putStrLn $ "exp_jAY = " ++ showTex exp_jAY ++ "\n"
--    putStrLn $ "var_jAZjAX = " ++ showTex var_jAZjAX ++ "\n"
--    putStrLn $ "d2_jAZ = " ++ showTex d2_jAZ ++ "\n"
--    putStrLn $ "d2_jAX = " ++ showTex d2_jAX ++ "\n"
--    putStrLn $ "d2_jAT = " ++ showTex d2_jAT ++ "\n"

    putStrLn $ unwords $ xmdsMoments [exp_a2a1, exp_jAY]
    putStrLn $ unlines $ xmdsMomentsExpressions [exp_a2a1, exp_jAY]

    putStrLn $ unwords $ xmdsMoments [d2_jAT]
    putStrLn $ unlines $ xmdsMomentsExpressions [d2_jAT]


