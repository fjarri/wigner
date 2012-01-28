import qualified Wigner.Transformations as T
import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.OperatorAlgebra as O
import Wigner.Texable
import Wigner.Expression

import qualified Data.Map as M

s_psi = S.symbol "\\Psi"
x = Func (Element S.x [] [])
psi_j = D.operatorFuncIx s_psi [S.ix_j] [x]
psi_k = D.operatorFuncIx s_psi [S.ix_k] [x]

s_rho = S.symbol "\\rho"

rho = D.operator s_rho

corr = M.fromList [(s_psi, s_psi)]

commutator x y = x * y - y * x

hamiltonian = dagger psi_j * psi_k
master_eqn = commutator hamiltonian rho
fpe = T.wignerTransformation corr s_rho master_eqn

main = do
    putStrLn $ "Non-truncated:"
    putStrLn $ showTex fpe
--    putStrLn $ "Truncated:"
--    putStrLn $ showTex (T.truncateDifferentials 2 fpe)
