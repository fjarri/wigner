module TestPositiveP(test_group) where

import qualified Wigner.Symbols as S
import qualified Wigner.DefineExpression as D
import qualified Wigner.Transformations as T
import Wigner.Expression
import Wigner.Complex
import Wigner.Texable
import Wigner.Deltas

import Data.Ratio
import qualified Data.Map as M

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

s_rho = S.symbol "\\rho"
rho = D.operator s_rho

-- mode symbols
s_a = S.symbol "a"
s_alpha = S.symbol "\\alpha"
s_beta = S.symbol "\\beta"
corr = M.fromList [(s_a, (s_alpha, s_beta))]

a = D.operator s_a

alpha = D.constant s_alpha
beta = D.constant s_beta
d_alpha = D.differential s_alpha
d_beta = D.differential s_beta

-- functional symbols
s_psi = S.symbol "\\Psi"
s_phi = S.symbol "\\Phi"
func_corr = M.fromList [(s_psi, (s_psi, s_phi))]

x = Func (Element S.x [] [])

psi_op = D.operatorFunc s_psi [x]
psi = D.function s_psi [x]
phi = D.function s_phi [x]
d_psi = D.differentialFuncIx s_psi [] [x]
d_phi = D.differentialFuncIx s_phi [] [x]
k = D.constant (S.symbol "\\mathcal{K}")
gamma = D.constant (S.symbol "\\Gamma")


commutator x y = x * y - y * x
transform = T.positivePTransformation corr s_rho
func_transform = T.positivePTransformation func_corr s_rho


test_linear = (showTex fpe) @?= (showTex result) where
    hamiltonian = dagger a * a
    fpe = transform $ commutator hamiltonian rho
    result = -d_alpha * alpha + d_beta * beta

-- Taken from Steel et al, 1998
test_functional = (showTex fpe) @?= (showTex result) where
    hamiltonian = dagger psi_op * k * psi_op + gamma * (dagger psi_op)^2 * psi_op^2 / 2
    fpe = func_transform $ -D.i * (commutator hamiltonian rho)
    result = -d_psi * (-D.i * (k * psi + gamma * psi^2 * phi)) +
        d_psi^2 * (-D.i * gamma * psi^2) / 2 -
        d_phi * (D.i * (k * phi + gamma * phi^2 * psi)) +
        d_phi^2 * (D.i * gamma * phi^2) / 2

test_group = testGroup "Positive-P transformations" [
        testCase "linear" test_linear,
        testCase "functional" test_functional
    ]
