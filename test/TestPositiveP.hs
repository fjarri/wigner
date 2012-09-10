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


s_a = S.symbol "a"
s_alpha = S.symbol "\\alpha"
s_beta = S.symbol "\\beta"
corr = M.fromList [(s_a, (s_alpha, s_beta))]

s_rho = S.symbol "\\rho"
rho = D.operator s_rho
a = D.operator s_a

alpha = D.constant s_alpha
beta = D.constant s_beta
d_alpha = D.differential s_alpha
d_beta = D.differential s_beta

commutator x y = x * y - y * x
transform = T.positivePTransformation corr s_rho


test_linear = (showTex fpe) @?= (showTex result) where
    hamiltonian = dagger a * a
    fpe = transform $ commutator hamiltonian rho
    result = -d_alpha * alpha + d_beta * beta

test_group = testGroup "Positive-P transformations" [
        testCase "linear" test_linear
    ]
