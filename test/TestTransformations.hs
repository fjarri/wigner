module TestTransformations(test_group) where

import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.Transformations as T
import Wigner.Expression
import Wigner.Complex
import Wigner.Texable

import Data.Ratio
import qualified Data.Map as M

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit


s_psi = S.symbol "\\Psi"
corr = M.fromList [(s_psi, s_psi)]

s_rho = S.symbol "\\rho"
rho = D.operator s_rho

x = Func (Element S.x [] [])

psi_j = D.operatorFuncIx s_psi [S.ix_j] [x]
psi_k = D.operatorFuncIx s_psi [S.ix_k] [x]
f_psi_j = D.functionIx s_psi [S.ix_j] [x]
f_psi_k = D.functionIx s_psi [S.ix_k] [x]
d_psi_j = D.differentialFuncIx s_psi [S.ix_j] [x]
d_psi_k = D.differentialFuncIx s_psi [S.ix_k] [x]

commutator x y = x * y - y * x
transform = T.wignerTransformation corr s_rho


test_linear = fpe @?= result where
    hamiltonian = dagger psi_j * psi_k
    fpe = transform $ commutator hamiltonian rho
    result = -d_psi_j * f_psi_k + conjugate (d_psi_k * f_psi_j)


test_group = testGroup "Expectations" [
        testCase "linear" test_linear
    ]
