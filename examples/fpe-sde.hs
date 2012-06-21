import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.Trace as T
import Wigner.Expression
import Wigner.Texable
import Wigner.Complex

import Data.Ratio

f1 = D.matrixIx (S.symbol "F") [S.ix_1]
g1 = D.matrixIx (S.symbol "G") [S.ix_1]
f2 = D.matrixIx (S.symbol "F") [S.ix_2]
g2 = D.matrixIx (S.symbol "G") [S.ix_2]
dx = D.matrix $ S.symbol "\\partial_x"
dy = D.matrix $ S.symbol "\\partial_y"
ej = D.matrix $ S.symbol "e_j"

dxt = transpose dx
dyt = transpose dy
ejt = transpose ej

dxdy = D.expr2by2 (dx * dxt) (dx * dyt) (dy * dxt) (dy * dyt)
dxdy_ejt_x = D.expr2by2 (dx * ejt) 0 (dy * ejt) 0
dxdy_ejt_y = D.expr2by2 0 (dx * ejt) 0 (dy * ejt)

dalpha = (dx - D.i * dy) / 2
b1 = f1 + D.i * g1
b2 = f2 + D.i * g2

hermite = transpose . conjugate
real x = (x + conjugate x) / 2
trace = T.normalizeTraces . T.trace

-- checking Tr{dalpha^* dalpha^T B B^H} <=> B dZ
result1 = from_fpe - from_sdes where
    from_fpe = trace (conjugate dalpha * transpose dalpha * b1 * hermite b1)
    b_tilde_sqrt2 = D.expr2by2 f1 (-g1) g1 f1
    bbt = b_tilde_sqrt2 * transpose b_tilde_sqrt2 / 2
    from_sdes = trace (dxdy * bbt) / 2

-- checking B dZ => s_j = Tr{B^H dalpha^* e_j^T B}
str_result1 = from_complex - from_real where
    from_complex = trace (hermite b1 * conjugate dalpha * ejt * b1) / 2
    b_tilde_sqrt2 = D.expr2by2 f1 (-g1) g1 f1
    from_real = trace (
        transpose b_tilde_sqrt2 * dxdy_ejt_x * b_tilde_sqrt2
        + D.i * transpose b_tilde_sqrt2 * dxdy_ejt_y * b_tilde_sqrt2
        ) / 4

-- checking Tr{dalpha dalpha^H B B^H} <=> B^* dZ
result2 = from_fpe - from_sdes where
    from_fpe = trace (dalpha * hermite dalpha * b1 * hermite b1)
    b_tilde_sqrt2 = D.expr2by2 f1 g1 (-g1) f1
    bbt = b_tilde_sqrt2 * transpose b_tilde_sqrt2 / 2
    from_sdes = trace (dxdy * bbt) / 2

-- checking
-- Tr{dalpha^* dalpha^T B_1 B_1^H} + Tr{dalpha^* dalpha^T B_2 B_2^H}
-- + Real( Tr{dalpha dalpha^T B_1 B_2^T} + Tr{dalpha dalpha^T B_2 B_1^T} )
-- <=> B_1 dZ + B_2 dZ^*
result3 = from_fpe - from_sdes where
    from_fpe = trace (
        conjugate dalpha * transpose dalpha * b1 * hermite b1
        + conjugate dalpha * transpose dalpha * b2 * hermite b2
        + real (dalpha * transpose dalpha * b1 * transpose b2)
        + real (dalpha * transpose dalpha * b2 * transpose b1)
        )
    b_tilde_sqrt2 = D.expr2by2 (f1 + f2) (g2 - g1) (g1 + g2) (f1 - f2)
    bbt = b_tilde_sqrt2 * transpose b_tilde_sqrt2 / 2
    from_sdes = trace (dxdy * bbt) / 2

-- checking B_1 dZ + B_2 dZ^*
-- => s_j = (Tr{B_1^H dalpha^* e_j^T B_1} + Tr{B_2^H dalpha^* e_j^T B_2}
-- + Tr{B_1 dalpha e_j^T B_2} + Tr{B_2 dalpha e_j^T B_1}) / 2
str_result3 = from_complex - from_real where
    from_complex = trace (
        hermite b1 * conjugate dalpha * ejt * b1 +
        hermite b2 * conjugate dalpha * ejt * b2 +
        transpose b1 * dalpha * ejt * b2 +
        transpose b2 * dalpha * ejt * b1
        ) / 2
    b_tilde_sqrt2 = D.expr2by2 (f1 + f2) (g2 - g1) (g1 + g2) (f1 - f2)
    from_real = trace (
        transpose b_tilde_sqrt2 * dxdy_ejt_x * b_tilde_sqrt2
        + D.i * transpose b_tilde_sqrt2 * dxdy_ejt_y * b_tilde_sqrt2
        ) / 4


main = do putStrLn (showTex result1)
          putStrLn (showTex str_result1)
          putStrLn (showTex result2)
          putStrLn (showTex result3)
          putStrLn (showTex str_result3)
