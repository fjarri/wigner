import qualified Wigner.Transformations as T
import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.OperatorAlgebra as O
import Wigner.Texable
import Wigner.Expression

import qualified Data.Map as M
import qualified Data.List as L

index :: Integer -> Index
index i = S.index (fromInteger i :: Int)

s_psi = S.symbol "\\Psi"
corr = M.fromList [(s_psi, s_psi)]
psi i = D.operatorFuncIx s_psi [i] [Func (Element S.x [] [])]
psi' i = D.operatorFuncIx s_psi [i] [Func (Element S.x' [] [])]

s_rho = S.symbol "\\rho"
rho = D.operator s_rho

gamma1 i = D.constantIx (S.symbol "\\gamma") [index i]
gamma2 i j = D.constantIx (S.symbol "\\gamma") (L.sort [index i, index j])
gamma3 i j k = D.constantIx (S.symbol "\\gamma") (L.sort [index i, index j, index k])

commutator x y = x * y - y * x
lossTerm x = 2 * x * rho * dagger x - dagger x * x * rho - rho * dagger x * x

--loss_terms =
    --gamma1 1 * lossTerm (psi 1) +
    --gamma3 1 1 1 * lossTerm (psi 1 * psi 1 * psi 1)
    --gamma2 2 2 * lossTerm (a 2 * a 2) +
    --gamma3 1 1 1 * lossTerm (a 1 * a 1 * a 1) +

master_eqn = commutator (dagger (psi S.ix_j) * dagger (psi' S.ix_k)
    * psi' S.ix_k * psi S.ix_j) rho

fpe = T.wignerTransformation corr s_rho master_eqn

main = do
    putStrLn $ T.showTexByDifferentials fpe
