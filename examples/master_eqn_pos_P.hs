import qualified Wigner.Transformations as T
import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.OperatorAlgebra as O
import Wigner.Texable
import Wigner.Expression
import Wigner.Deltas

import qualified Data.Map as M
import qualified Data.List as L


s_a = S.symbol "a"

corr = M.fromList [(s_a, (S.symbol "\\alpha", S.symbol "\\beta"))]

index :: Integer -> Index
index p = S.index (fromInteger p :: Int)

iind = S.index (S.symbol "i")
jind = S.index (S.symbol "j")


-- Schwinger representation
--a i p = D.operatorIx S.a [i, index p]
--sx i = (dagger (a i 1) * a i 2 + dagger (a i 2) * a i 1) / 2
--sy i = (dagger (a i 1) * a i 2 - dagger (a i 2) * a i 1) / (2 * D.i)
--sz i = (dagger (a i 1) * a i 1 - dagger (a i 2) * a i 2) / 2

-- Number representation
a i = D.operatorIx S.a [i]
sx i = (dagger (a i) + a i) / 2
sy i = (dagger (a i) - a i) / (2 * D.i)
sz i = ( - a i * dagger (a i)) / 2

dij = makeIndexDelta (iind, jind)

s_rho = S.symbol "\\rho"
rho = D.operator s_rho

commutator x y = x * y - y * x
loss x = 2 * x * rho * dagger x - dagger x * x * rho - rho * dagger x * x

master_eqn = loss (a iind)

fpe = T.positivePTransformation corr s_rho master_eqn

main = do
    --putStrLn $ show fpe
    putStrLn $ T.showTexByDifferentials fpe
