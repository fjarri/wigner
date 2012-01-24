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

isqrt2 = D.constant $ S.symbol "(\\frac{1}{\\sqrt{2}})"
c1 = isqrt2 * (a1 + b1 * D.i)
c2 = isqrt2 * (a2 + b2 * D.i)
d1 = isqrt2 * (b1 + a1 * D.i)
d2 = isqrt2 * (b2 + a2 * D.i)
g = D.constant $ S.symbol "g"

e_idt = D.constant $ S.symbol "(e^{i \\Delta \\theta})"
e_m_idt = D.constant $ S.symbol "(e^{-i \\Delta \\theta})"
cos_t = D.constant $ S.symbol "\\cos(\\theta)"
sin_t = D.constant $ S.symbol "\\sin(\\theta)"

ca2a1 = (dagger a2) * a1

jAX = ((dagger a2) * a1 * e_idt + (dagger a1) * a2 * e_m_idt) / 2
jAY = ((dagger a2) * a1 * e_idt - (dagger a1) * a2 * e_m_idt) / (2 * D.i)
jAZ = ((dagger a2) * a2 - (dagger a1) * a1) / 2
jAT = cos_t * jAZ + sin_t * jAX

jCX = (dagger c2 * c1 * e_idt + dagger c1 * c2 * e_m_idt) / 2
jCY = (dagger c2 * c1 * e_idt - dagger c1 * c2 * e_m_idt) / (2 * D.i)
jCZ = (dagger c2 * c2 - dagger c1 * c1) / 2
jDX = (dagger d2 * d1 * e_idt + dagger d1 * d2 * e_m_idt) / 2
jDY = (dagger d2 * d1 * e_idt - dagger d1 * d2 * e_m_idt) / (2 * D.i)
jDZ = (dagger d2 * d2 - dagger d1 * d1) / 2

jCT = cos_t * jCZ + sin_t * jCX
jDT = cos_t * jDZ + sin_t * jDX

constants = [
    (e_idt, "numpy.exp(1j * d_theta)"),
    (e_m_idt, "numpy.exp(-1j * d_theta)"),
    (cos_t, "numpy.cos(theta)"),
    (sin_t, "numpy.sin(theta)"),
    (g, "g"),
    (isqrt2, "numpy.sqrt(0.5)")
    ]
exprs = [
    Result ComplexValue (eval $ E.expectation ca2a1) "exp_ca2a1",
    UserCalculation "d_theta",
    Result RealValue (eval $ E.expectation jAY) "exp_jAY",
	Result RealValue (eval $ E.variance jAZ jAX) "var_jAZ_jAX",
	Result RealValue (eval $ E.deltaSquared jAZ) "d2_jAZ",
	Result RealValue (eval $ E.deltaSquared jAX) "d2_jAX",
	Lambda RealValue (eval $ E.deltaSquared jAT) "d2_jAT" ["theta"],
    Lambda RealValue (eval $ E.deltaSquared jDT) "d2_jDT" ["theta"],
    Lambda RealValue (eval $ E.variance jCT jDT) "var_jCTjDT" ["theta"],
    Result RealValue (eval $ E.expectation jCY) "exp_jCY",
    Result RealValue (eval $ E.expectation jDY) "exp_jDY",
    Lambda RealValue (eval $ E.deltaSquared (jCT + g * jDT)) "d2_jCT_gjDT" ["theta", "g"]
    ]

main = do
    putStrLn $ xmdsGenerateCode constants exprs
