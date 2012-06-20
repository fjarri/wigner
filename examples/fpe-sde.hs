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
dxt = transpose dx
dyt = transpose dy
dxdy = D.expr2by2 (dx * dxt) (dx * dyt) (dy * dxt) (dy * dyt)

dalpha = (dx - D.i * dy) / 2
b1 = f1 + D.i * g1
b2 = f2 + D.i * g2

hermite = transpose . conjugate
real x = (x + conjugate x) / 2

-- checking Tr{dalpha^* dalpha^T B B^H} <=> B dZ
result1 = from_fpe - from_sdes where
    from_fpe = T.normalizeTraces $ T.trace (conjugate dalpha * transpose dalpha * b1 * hermite b1)
    b_tilde_sqrt2 = D.expr2by2 f1 (-g1) g1 f1
    bbt = b_tilde_sqrt2 * transpose b_tilde_sqrt2 / 2
    from_sdes = T.normalizeTraces $ T.trace (dxdy * bbt) / 2

-- checking Tr{dalpha dalpha^H B B^H} <=> B^* dZ
result2 = from_fpe - from_sdes where
    from_fpe = T.normalizeTraces $ T.trace (dalpha * hermite dalpha * b1 * hermite b1)
    b_tilde_sqrt2 = D.expr2by2 f1 g1 (-g1) f1
    bbt = b_tilde_sqrt2 * transpose b_tilde_sqrt2 / 2
    from_sdes = T.normalizeTraces $ T.trace (dxdy * bbt) / 2

-- checking
-- Tr{dalpha^* dalpha^T B_1 B_1^H} + Tr{dalpha^* dalpha^T B_2 B_2^H}
-- + Real( Tr{dalpha dalpha^T B_1 B_2^T} + Tr{dalpha dalpha^T B_2 B_1^T} )
-- <=> B_1 dZ + B_2 dZ^*
result3 = from_fpe - from_sdes where
    from_fpe = T.normalizeTraces $ T.trace (
        conjugate dalpha * transpose dalpha * b1 * hermite b1
        + conjugate dalpha * transpose dalpha * b2 * hermite b2
        + real (dalpha * transpose dalpha * b1 * transpose b2)
        + real (dalpha * transpose dalpha * b2 * transpose b1)
        )
    b_tilde_sqrt2 = D.expr2by2 (f1 + f2) (g2 - g1) (g1 + g2) (f1 - f2)
    bbt = b_tilde_sqrt2 * transpose b_tilde_sqrt2 / 2
    from_sdes = T.normalizeTraces $ T.trace (dxdy * bbt) / 2


main = do putStrLn (showTex result1)
          putStrLn (showTex result2)
          putStrLn (showTex result3)
