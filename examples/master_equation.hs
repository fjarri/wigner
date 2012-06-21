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

a i = D.operatorIx S.a [index i]
b i = D.operatorIx S.b [index i]

s_rho = S.symbol "\\rho"
rho = D.operator s_rho

kappa i = D.constantIx (S.symbol "\\kappa") [index i]
g i j = D.constantIx (S.symbol "g") (L.sort [index i, index j])
gamma1 i = D.constantIx (S.symbol "\\gamma") [index i]
gamma2 i j = D.constantIx (S.symbol "\\gamma") (L.sort [index i, index j])
gamma3 i j k = D.constantIx (S.symbol "\\gamma") (L.sort [index i, index j, index k])

commutator x y = x * y - y * x
lossTerm x = 2 * x * rho * dagger x - dagger x * x * rho - rho * dagger x * x

hamiltonian =
    sum (map
        (\i -> kappa i * (dagger (a i) * b i + dagger (b i) * a i))
    [1, 2])
    + sum (map
        (\(i, j) -> g i j * (
            dagger (a i) * dagger (a j) * a j * a i +
            dagger (b i) * dagger (b j) * b j * b i
        ) / 2)
    [(1,1), (1,2), (2,1), (2,2)])
loss_terms =
    gamma1 1 * lossTerm (a 1) +
    gamma1 1 * lossTerm (b 1) +
    gamma2 1 2 * lossTerm (a 1 * a 2) +
    gamma2 1 2 * lossTerm (b 1 * b 2) +
    gamma2 2 2 * lossTerm (a 2 * a 2) +
    gamma2 2 2 * lossTerm (b 2 * b 2)
    --gamma3 1 1 1 * lossTerm (a 1 * a 1 * a 1) +
    --gamma3 1 1 1 * lossTerm (b 1 * b 1 * b 1)

master_eqn = -D.i * commutator hamiltonian rho + loss_terms

fpe = T.wignerTransformation S.default_map s_rho master_eqn

main = do
    putStrLn $ T.showTexByDifferentials (T.truncateDifferentials 2 fpe)
