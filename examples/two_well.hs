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

s_rho = S.symbol "\\rho"
rho = D.operator s_rho

a i = D.operatorIx S.a [index i]
b i = D.operatorIx S.b [index i]

g i j = D.constantIx (S.symbol "g") (L.sort [index i, index j])
kappa i = D.constantIx (S.symbol "\\kappa") [index i]

components = [1, 2]

tunnelling_term i = kappa i * (dagger (a i) * b i + dagger (b i) * a i)
interaction_term x [i, j] = (g i j * dagger (x i) * dagger (x j) * x j * x i) / 2

hamiltonian = sum (map tunnelling_term components) +
    sum (map (interaction_term a) (sequence [components, components])) +
    sum (map (interaction_term b) (sequence [components, components]))

master_eqn =

--symbol_map =
fpe = T.wignerTransformation D.default_map s_rho hamiltonian

main = do
    --putStrLn $ showTex hamiltonian
    putStrLn $ T.showTexByDifferentials (T.truncateDifferentials 2 fpe)