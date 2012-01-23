import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.Expectations as E
import qualified Data.Set as Set
import qualified Data.Map as M

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
ca2a1 = (dagger a2) * a1

isqrt2 = D.constant $ S.symbol "(\\frac{1}{\\sqrt{2}})"
c1 = isqrt2 * (a1 + b1 * D.i)
c2 = isqrt2 * (a2 + b2 * D.i)
jCZ = ((dagger c2) * c2 - (dagger c1) * c1) / 2

-- Expectations
exp_ca2a1 = eval $ E.expectation ca2a1
exp_jAY = eval $ E.expectation jAY
var_jAZ_jAX = eval $ E.variance jAZ jAX
d2_jAZ = eval $ E.deltaSquared jAZ
d2_jAX = eval $ E.deltaSquared jAX
d2_jAT = eval $ E.deltaSquared jAT

constants = [
    (e_idt, "numpy.exp(1j * d_theta)"),
    (e_m_idt, "numpy.exp(-1j * d_theta)"),
    (cos_t, "numpy.cos(theta)"),
    (sin_t, "numpy.sin(theta)")
    ]
exprs = [
    Result exp_ca2a1 "exp_ca2a1",
    UserCalculation "d_theta",
    Result exp_jAY "exp_jAY",
	Result var_jAZ_jAX "var_jAZ_jAX",
	Result d2_jAZ "d2_jAZ",
	Result d2_jAX "d2_jAX",
	Lambda d2_jAT "d2_jAT" ["theta"]
    ]

main = do
    putStrLn $ xmdsGenerateCode constants exprs
--    putStrLn $ xmdsBlock exprs
--    putStrLn $ pythonBlock exprs ["exp_a2a1", "exp_jAY"] ()

